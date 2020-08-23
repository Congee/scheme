{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module LispVal
  ( LispVal(..)
  , IFunc(..)
  , Eval(..)
  , EnvCtx(..)
  , LispException(..)
  , showVal
  )
where

import           Data.Typeable                  ( Typeable )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Control.Monad.Reader
import           Control.Exception

type ValCtx = Map.Map T.Text LispVal
type FnCtx  = Map.Map T.Text LispVal

data EnvCtx = EnvCtx {env::ValCtx, fenv::FnCtx} deriving (Eq)

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader EnvCtx
           , MonadIO)

newtype IFunc = IFunc {fn:: [LispVal] -> Eval LispVal} deriving (Typeable)

instance Eq IFunc where
  (==) _ _ = False

data LispVal = Atom T.Text
             | List [LispVal]
             | Number Integer
             | String T.Text
             | Fun IFunc
             | Lambda IFunc EnvCtx  -- e.g. ((lambda (x) (+ x 100)) 42)
             | Nil
             | Bool Bool
             deriving (Typeable, Eq)

showVal :: LispVal -> T.Text
showVal val = case val of
  (Atom   atom    ) -> atom
  (String str     ) -> T.concat ["\"", str, "\""]
  (Number num     ) -> T.pack $ show num
  (Bool   True    ) -> "#t"
  (Bool   False   ) -> "#f"
  (List   contents) -> T.concat ["(", unwordsList contents, ")"]
  (Fun    _       ) -> "(internal function)"
  (Lambda _ _     ) -> "(lambda function)"
  Nil               -> "'()"

instance Show LispVal where
  show = T.unpack . showVal


data LispException = Arity          Integer [LispVal]
                   | LengthOfList   T.Text Int
                   | ExpectedList   T.Text
                   | TypeMismatch   T.Text LispVal
                   | BadSpecialForm T.Text
                   | NotFunction    LispVal
                   | UnboundVar     T.Text
                   | Default        LispVal
                   | PError         String
                   | IOError        T.Text
                   deriving (Typeable)

instance Exception LispException


instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err = case err of
  (IOError txt     )       -> T.concat ["Error reading file: ", txt]
  (Arity int args)       -> T.concat ["Error Number Arguments, expected: " , T.pack $ show int , "received args: " , unwordsList args]
  (LengthOfList txt int)   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
  (ExpectedList txt)       -> T.concat ["Error Expected List in function: ", txt]
  (TypeMismatch txt val)   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
  (BadSpecialForm txt)     -> T.concat ["Error Bad Special Form: ", txt]
  (NotFunction    val)     -> T.concat ["Error Not a function: ", showVal val]
  (UnboundVar     txt)     -> T.concat ["Error Unbound Variable: ", txt]
  (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ", T.pack str]
  (Default val)            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed: ", showVal val]

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
