{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc, Todo
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Logger ( logInfo, LoggingT )
import qualified Data.Text as T ( pack )

data Todo = Todo {
  _title :: String,
  _completed :: Bool,
  _logging :: String -> LoggingT IO ()
}
makeLenses ''Todo

type App = StateT Todo IO ()

test1 :: App
test1 = do
  -- Get the current title using the lens operator (^.)
  currentTitle <- use title
  liftIO $ putStrLn $ "Current title: " ++ currentTitle

  -- Read a new title from the user
  liftIO $ putStrLn "Enter a new title:"
  newTitle <- liftIO getLine

  -- Update the title using the lens operator (.=)
  title .= newTitle

  use title >>= (\lens' -> liftIO $ putStrLn ("Updated title: " ++ lens')) 


someFunc :: IO ()
someFunc = do
  putStrLn "running test1..."
  evalStateT test1  Todo {
    _title = "test",
    _completed = False,
    _logging = $(logInfo) . T.pack
  }
  putStrLn "done"
