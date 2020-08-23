{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import           Prim
import           Parser
import           LispVal

import           Control.Exception
import           Control.Monad.Reader

import           Data.Foldable                  ( toList )
import qualified Data.Map                      as Map
import           Data.Monoid
import           System.Directory
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Text.Parsec


funcEnv :: Map.Map T.Text LispVal
funcEnv = Map.fromList $ primEnv
    <> [ ("read" , Fun $ IFunc $ unop readFn)
       , ("parse", Fun $ IFunc $ unop parseFn)
       , ("eval" , Fun $ IFunc $ unop eval)
       , ("show" , Fun $ IFunc $ unop (return . String . showVal))
       ]

-- basicEnv :: EnvCtx
-- basicEnv = EnvCtx Map.empty funcEnv

-- -- fileToEvalForm :: T.Text -> Eval LispVal
-- -- fileToEvalForm input =
-- --   either (throw . PError . show) evalBody $ readExprFile input

-- -- runParseTest :: T.Text -> T.Text
-- -- runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

-- -- runASTinEnv :: EnvCtx -> Eval b -> IO b
-- -- runASTinEnv code action = runResourceT $ runReaderT (unEval action) code

-- -- evalFile :: T.Text -> IO ()
-- -- evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print


eval :: LispVal -> Eval LispVal


getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  EnvCtx{..} <- ask
  case Map.lookup atom (Map.union env fenv) of
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch "failure to get varabe: " n

getEven :: [t] -> [t]
getEven []       = []
getEven (x : xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd []       = []
getOdd (x : xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n          = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
-- autoquote
eval (Number i ) = return $ Number i
eval (String i ) = return $ String i
eval (Bool   i ) = return $ Bool i
eval (List   []) = return Nil
eval Nil         = return Nil


-- -- write
-- eval (List [Atom "showSF", rest])    = return . String . T.pack . show $ rest
-- eval (List ((Atom "showSF") : rest)) = return . String . T.pack . show . List $ rest


-- -- quote
-- eval (List [Atom "quote", val]) = return val

-- -- e.g. (let ((x 10) (y 4) ) y)
-- -- e.g. (let ((x 10)) x) => 10
-- -- e.g. ((lambda () 1)) => 1
-- -- e.g. (let ((x 1)) x) => 1
-- -- let is a syntatic sugar for lambda ((lambda (x) (+ x 3) 4) => 7
-- eval (List [Atom "let", List pairs, expr]) = do
--   EnvCtx {..} <- ask
--   atoms       <- mapM ensureAtom $ getEven pairs
--   vals        <- mapM eval       $ getOdd  pairs
--   bindArgsEval atoms vals expr
-- eval (List (Atom "let" : _)) = throw $ BadSpecialForm
--   "let function expectes a list of parameters and S-Expression body\
--   \(let <pairs> <s-expr>)"


-- eval (List [Atom "dumpenv", x]) = do
--   EnvCtx{..} <- ask
--   liftIO . print $ toList env
--   liftIO . print $ toList fenv
--   eval x

-- -- begin, define & evalBody
-- eval (List [Atom "begin", rest]   ) = evalBody rest
-- eval (List ((Atom "begin") : rest)) = evalBody $ List rest

-- -- eval (List [Atom "define", varExpr, expr]) = do
-- --   varAtom <- ensureAtom varExpr
-- --   evalVal <- eval expr
-- --   env <- ask
-- --   let envFn = const $ Map.insert (extractVar varAtom) evalVal env
-- --    in local envFn $ return varExpr

-- updateEnv :: T.Text -> LispVal -> EnvCtx -> EnvCtx
-- updateEnv var e@(Fun _     ) EnvCtx{..} = EnvCtx env $ Map.insert var e fenv
-- updateEnv var e@(Lambda _ _) EnvCtx{..} = EnvCtx env $ Map.insert var e fenv
-- updateEnv var e              EnvCtx{..} = EnvCtx (Map.insert var e env) fenv

-- -- | (define x 3)
-- -- x => 3
-- -- (define f
-- --   (lambda (x) (+ x x)))
-- -- (f 3) => 6
-- evalBody :: LispVal -> Eval LispVal
-- evalBody (List [List ((Atom "define") : [Atom var, expr]), rest]) = do
--   val <- eval expr
--   ctx <- ask
--   local (const $ updateEnv var val ctx) $ eval rest

-- evalBody (List ((List ((Atom "define") : [Atom var, expr])) : rest)) = do
--   val <- eval expr
--   ctx <- ask
--   local (const $ updateEnv var val ctx) $ evalBody $ List rest

-- evalBody x = eval x

-- applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
-- applyLambda expr params args = bindArgsEval params args expr

-- isLambda :: LispVal -> Bool
-- isLambda (List ((Atom "lambda"):_)) = True
-- isLambda _ = False

-- lambda & applyLambda
-- eval (List [Atom "lambda", List params, expr]) = do
--   ctx <- ask
--   return $ Lambda (IFunc $ applyLambda expr params) ctx
-- eval (List (Atom "lambda" :_)) = throw $ BadSpecialForm "lambda"

-- -- Atom
-- eval n@(Atom _) = getVar n

-- bindArgsEval :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
-- bindArgsEval params args expr = do
--   EnvCtx{..} <- ask
--   let newVars = Prelude.zipWith (\a b -> (extractVar a, b)) params args
--   let (newEnv, newFenv) = Map.partition (not . isLambda) $ Map.fromList newVars
--   -- Control.Monad.Reader local :: MonadReader r m => (r -> r) -> m a -> m a
--   local (const $ EnvCtx (newEnv <> env) (newEnv <> fenv)) $ eval expr

-- -- -- application
-- -- eval (List x:xs) = do
-- --   funVar <- eval x
-- --   xVal   <- mapM eval xs
-- --   case funVar of
-- --     (Fun (IFunc internalFn)) -> internalFn xVal
-- --     (Lambda (IFunc internalFn) boundenv) -> local (const boundenv) $ internalFn xVal
-- --     _                                    -> throw $ NotFunction funVar


stdlib :: FilePath
stdlib = "lib/stdlib.scm"

safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) -> case fromException eTop of
      Just (enclosed :: LispException) -> return $ Left (show enclosed)
      Nothing                          -> return $ Left (show eTop)
    Right val -> return $ Right val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expects string, instead got: " val

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) eval $ readExpr input

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val = throw $ TypeMismatch "read expects string, instead got: " val

-- getFileContents :: FilePath -> IO T.Text
-- getFileContents filename = do
--   exists <- doesFileExist filename
--   if exists then TIO.readFile filename else return "File does not exist."

-- textToEvalForm :: T.Text -> T.Text -> Eval LispVal
-- textToEvalForm std input = either (throw . PError . show) evalBody $ parseWithLib std input

-- -- repl
-- evalText :: T.Text -> IO ()
-- evalText textExpr = do
--   std <- getFileContents stdlib
--   res <- runAstInEnv basicEnv$ textToEvalForm std textExpr
--   return res
