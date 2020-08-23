{-# LANGUAGE OverloadedStrings #-}


module Parser where

import           LispVal

import           Text.Parsec
import           Text.Parsec.Text
import           Text.Parsec.Expr
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Language          as Lang
import           Data.Text                     as T
import           Data.Char                      ( digitToInt )
import           Control.Applicative     hiding ( (<|>) )
import           Data.Functor.Identity          ( Identity )
import           Data.Functor

import           Control.Monad                  ( mzero )

-- data ParsecT s u m a
-- where stream type s, user state type u, underlying monad m and return type a.
-- e.g. `ParsecT Text () Identity Char`

-- data Parsec s u = ParsecT s u Identity
-- data Parser = Parsec Text ()

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd   = "-}"
  , Tok.commentLine  = ";"
  , Tok.opStart      = mzero
  , Tok.opLetter     = mzero
  , Tok.identStart   = letter <|> oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter  = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  }

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

-- pattern binding using record destructing !
Tok.TokenParser { Tok.parens = m_parens, Tok.identifier = m_identifier } =
  Tok.makeTokenParser style

parens :: Parser a -> Parser a
parens = Tok.parens lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.parens lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

decimal :: Parser Integer
decimal = Tok.decimal lexer

sign :: Parser (Integer -> Integer)
sign = char '-' $> negate <|> char '+' $> id <|> return id

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
 where
  specialIdentifier :: Parser String
  specialIdentifier = lexeme $ try $ string "-" <|> string "+" <|> string "..."

-- | The @Radix@ type consists of a base integer (e.g. @10@) and a parser for
-- digits in that base (e.g. @digit@).
type Radix = (Integer, Parser Char)

-- | Parse an integer, given a radix as output by @radix@.
numberWithRadix :: Radix -> Parser Integer
numberWithRadix (base, baseDigit) = do
  digits <- many1 baseDigit
  let n = Prelude.foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

intRadix :: Radix -> Parser Integer
intRadix r = sign <*> numberWithRadix r

nil :: Parser ()
nil = try (char '\'' *> string "()") $> () <?> "nil"

hashval :: Parser LispVal
hashval = lexeme $ char '#' *>
  (   char 't'   $> Bool True
  <|> char 'f'   $> Bool False
  <|> char 'b'   *> (Number <$> intRadix (2, oneOf "01"))
  <|> char 'o'   *> (Number <$> intRadix (8, octDigit))
  <|> char 'd'   *> (Number <$> intRadix (10, digit))
  <|> char 'x'   *> (Number <$> intRadix (16, hexDigit))
  <|> oneOf "ei" *> fail "Unsupported: extractness"
  <|> char '('   *> fail "Unsupported: vector"
  <|> char '\\'  *> fail "Unsupported: char"
  )


text :: Parser T.Text
text = T.pack <$> Tok.stringLiteral lexer


_quote :: LispVal -> LispVal
_quote x = List [Atom "quote", x]

manyLispVal :: Parser [LispVal]
manyLispVal = lispval `sepBy` whitespace

lispval :: Parser LispVal
lispval = hashval
  <|> Nil    <$  nil
  <|> Number <$> try (sign <*> decimal)
  <|> Atom   <$> identifier
  <|> String <$> text
  <|> _quote <$> quoted lispval
  <|> List   <$> parens manyLispVal


contents :: Parser a -> Parser a
contents p = whitespace *> p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispval) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal)) "<file>"
