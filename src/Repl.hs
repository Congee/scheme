module Repl
  -- ( loop
  -- )
where

import           Eval
import           Data.Text                     as T

import           Control.Monad.Trans
import           System.Console.Haskeline

type Repl a = InputT IO a

-- process :: String -> IO ()
-- process str = do
--   res <- safeExec $ evalText $ T.pack str
--   either putStrLn return res

-- repl :: Repl ()
-- repl = do
--   minput <- getInputLine "Repl> "
--   case minput of
--     Nothing    -> outputStrLn "Goodbye."
--     Just input -> liftIO (process input) >> repl

-- loop :: IO ()
-- loop = runInputT defaultSettings repl
