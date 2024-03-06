{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
  ( someFunc, Todo
  ) where

import           Control.Lens
import           Control.Monad.Logger (LoggingT, logInfo, runStderrLoggingT)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad (when)
import qualified Data.Text            as T (pack)
import           Data.Time.Clock      (UTCTime, getCurrentTime)

import           System.Console.ANSI      (clearScreen, hideCursor, showCursor)
import           System.IO                (BufferMode (LineBuffering, NoBuffering),
                                           hSetBuffering, hSetEcho, stdin)
import Data.Maybe (fromMaybe)

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return c

data Todo = Todo
  { _title      :: String
  , _completed  :: Bool
  , _created_at :: UTCTime
  , _modified   :: UTCTime
  }
makeLenses ''Todo

data TodoList = TodoList
  { _todos :: [Todo],
    _selected :: Int,
    _notice :: String
  }
makeLenses ''TodoList

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

type App = LoggingT (StateT TodoList IO) ()

-- type TestType a b =  ASetter TodoList TodoList a (a -> b) -> (Int -> a -> b) -> App

-- (.#=) :: TestType a b
-- (.#=) lens' f = do
--   selected' <- use selected
--   let f' = f selected'
--   lens' .= f'


-- (.<<) :: WithSelected
-- (.<<) = modifyWithSelected

-- type LensModifierWithAction a b = ASetter Todo Todo a b -> (a -> b) -> App
-- type LensModifierWithValue  a b = ASetter Todo Todo a b -> b        -> App

-- modifyAndSetModified :: LensModifierWithAction a b
-- modifyAndSetModified lens' f = do
--   lens' %= f
--   now <- liftIO getCurrentTime
--   modified .= now

-- setAndSetModified :: LensModifierWithValue a b
-- setAndSetModified lens' v = do
--   lens' .= v
--   now <- liftIO getCurrentTime
--   modified .= now

-- (%:=) :: LensModifierWithAction a b
-- (%:=) = modifyAndSetModified

-- (.:=) :: LensModifierWithValue a b
-- (.:=) = setAndSetModified

showTodos :: [Todo] -> Int -> IO ()
showTodos todo_list selected' = putStr $ ifoldMap (\i todo
  -> (if i == selected' then " > " else "   ")
  <> show (i +1)
  <> ". "
  <> (if todo ^. completed then "[x] " else "[ ] ")
  <> (todo ^. title) <> "\n") todo_list

test1 :: App
test1 = do
  notce' <- use notice
  todo_list <- use todos
  selected' <- use selected
  liftIO $ do
    clearScreen
    putStrLn notce'
    putStrLn  $ "Selected: " <> fromMaybe "" (todo_list ^? ix selected' . title)
  -- $(logInfo) "Start test1"

  liftIO $ showTodos todo_list selected'
  liftIO $ putStrLn "j and k to navigate, enter to toggle, n to create todo, d to delete, q to quit"

  c <- liftIO getCharNoEcho
  case c of
    'q' -> do
      liftIO $ putStrLn "Quitting..."
      return ()
    'j' -> do
      selected .= (selected' + 1) `mod` length todo_list
      
    'k'  -> do
      selected .= (selected' - 1) `mod` length todo_list
      
    '\n' -> do
      todos . ix selected' . completed %= not
    'n' -> do
      liftIO $ putStrLn "Enter a new todo:"
      newTodo <- liftIO getLine
      now <- liftIO getCurrentTime
      todos %= (++ [Todo newTodo False now now])
    'd' ->
      if null todo_list
        then notice .= "no todos to delete"
        else do
          liftIO $ putStrLn ("Are you sure you want to delete this todo?: " <> fromMaybe "" (todo_list ^? ix selected' . title) <> " (y/n)")
          c' <- liftIO getCharNoEcho
          case c' of
            'y' -> do
              let todos_length = length todo_list - 1
              when (selected' == todos_length) $ selected -= 1
              todos %= (\todos' -> take selected' todos' ++ drop (selected' + 1) todos')

            _ -> pure ()

    -- 'l'  -> displayDetails 
    _ -> notice .= "Invalid command"
  
  test1

someFunc :: IO ()
someFunc = do
  hideCursor
  putStrLn "running test1..."
  currentTime <- getCurrentTime
  evalStateT (runStderrLoggingT test1) $ TodoList
    [ Todo
      { _title = "test"
      , _completed = False
      , _created_at = currentTime
      , _modified = currentTime }
    , Todo
      { _title = "second test"
      , _completed = False
      , _created_at = currentTime
      , _modified = currentTime }
    , Todo
      { _title = "third test"
      , _completed = False
      , _created_at = currentTime
      , _modified = currentTime }
    ] 0 "Welcome to the todo app!"
  putStrLn "Goodbye!"
  showCursor
