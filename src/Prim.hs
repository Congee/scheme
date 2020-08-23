{-# LANGUAGE OverloadedStrings #-}

module Prim where

import           LispVal

import           System.Directory
import           System.IO
import           Network.HTTP
import           Data.Text.IO                  as TIO
import           Data.Text                     as T
import           Data.Monoid

import           Control.Exception       hiding ( handle )
import           Control.Monad.Except

type Prim = [(T.Text, LispVal)]
type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

mkf :: ([LispVal] -> Eval LispVal) -> LispVal
mkf = Fun . IFunc

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x]  = op x
unop _  args = throw $ Arity 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x, y] = op x y
binop _  args   = throw $ Arity 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op fargs args = case args of
  [a, b]   -> op a b
  (a : as) -> foldM op fargs args
  []       -> throw $ Arity 2 args

fileExists :: LispVal -> Eval LispVal
fileExists (Atom   atom) = fileExists $ String atom
fileExists (String txt ) = Bool <$> liftIO (doesFileExist $ T.unpack txt)

slurp :: LispVal -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt

wSlurp :: LispVal -> Eval LispVal
wSlurp (String txt) = liftIO $ openUrl txt
wSlurp val = throw $ TypeMismatch "wSlurp expects a string, instead got: " val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp filename = withFile (T.unpack filename) ReadMode go
  where go = readTextFile filename

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile filename handle = do
  exists <- hIsEOF handle
  if exists
    then String <$> TIO.hGetContents handle
    else throw $ IOError $ T.concat [" file does not exist: ", filename]

openUrl :: T.Text -> IO LispVal
openUrl x = do
  req  <- simpleHTTP (getRequest $ T.unpack x)
  body <- getResponseBody req
  return $ String $ T.pack body

cons :: [LispVal] -> Eval LispVal
cons [x, y@(List yList)] = return $ List $ x : yList
cons [c]                 = return $ List [c]
cons []                  = return $ List []
cons _                   = throw $ ExpectedList "cons, in second argument"

car :: [LispVal] -> Eval LispVal
car [List []     ] = return Nil
car [List (x : _)] = return x
car []             = return Nil
car _              = throw $ ExpectedList "car"

cdr :: [LispVal] -> Eval LispVal
cdr [List (x : xs)] = return $ List xs
cdr [List []      ] = return Nil
cdr []              = return Nil
cdr _               = throw $ ExpectedList "cdr"

numbool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numbool op (Number x) = return $ Bool $ op x
numbool op x          = throw $ TypeMismatch "numeric op " x

numop :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numop op (Number x) (Number y) = return $ Number $ op x y
numop op x          (Number y) = throw $ TypeMismatch "numeric op " x
numop op (Number x) y          = throw $ TypeMismatch "numeric op " y
numop op x          y          = throw $ TypeMismatch "numeric op " x

strop :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strop op (String x) (String y) = return $ String $ op x y
strop op x          (String y) = throw $ TypeMismatch "string op " x
strop op (String x) y          = throw $ TypeMismatch "string op " y
strop op x          y          = throw $ TypeMismatch "string op " x

eqop :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqop op (Bool x) (Bool y) = return $ Bool $ op x y
eqop op x        (Bool y) = throw $ TypeMismatch "bool op " x
eqop op (Bool x) y        = throw $ TypeMismatch "bool op " y
eqop op x        y        = throw $ TypeMismatch "bool op " x

numcmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numcmp op (Number x) (Number y) = return . Bool $ op x y
numcmp op x          (Number y) = throw $ TypeMismatch "numeric op " x
numcmp op (Number x) y          = throw $ TypeMismatch "numeric op " y
numcmp op x          y          = throw $ TypeMismatch "numeric op " x

eqcmd :: LispVal -> LispVal -> Eval LispVal
eqcmd (Atom   x) (Atom   y) = return . Bool $ x == y
eqcmd (Number x) (Number y) = return . Bool $ x == y
eqcmd (String x) (String y) = return . Bool $ x == y
eqcmd (Bool   x) (Bool   y) = return . Bool $ x == y
eqcmd Nil        Nil        = return $ Bool True
eqcmd _          _          = return $ Bool False

primEnv :: Prim
primEnv =
  [ ("+"     , mkf $ binopFold (numop (+)) (Number 0))
  , ("*"     , mkf $ binopFold (numop (*)) (Number 1))
  , ("++"    , mkf $ binopFold (strop (<>)) (String ""))
  , ("-"     , mkf $ binop $ numop (-))
  , ("<"     , mkf $ binop $ numcmp (<))
  , ("<="    , mkf $ binop $ numcmp (<=))
  , (">"     , mkf $ binop $ numcmp (>))
  , (">="    , mkf $ binop $ numcmp (>=))
  , ("=="    , mkf $ binop $ numcmp (==))
  , ("even?" , mkf $ unop $ numbool even)
  , ("odd?"  , mkf $ unop $ numbool odd)
  , ("pos?"  , mkf $ unop $ numbool (< 0))
  , ("neg?"  , mkf $ unop $ numbool (> 0))
  , ("eq?"   , mkf $ binop eqcmd)
  , ("bl-eq?", mkf $ binop $ eqop (==))
  , ("and"   , mkf $ binopFold (eqop (&&)) (Bool True))
  , ("or"    , mkf $ binopFold (eqop (||)) (Bool False))
  , ("cons"  , mkf Prim.cons)
  , ("cdr"   , mkf Prim.cdr)
  , ("car"   , mkf Prim.car)
  , ("file?" , mkf $ unop fileExists)
  , ("slurp" , mkf $ unop slurp)
  ]
