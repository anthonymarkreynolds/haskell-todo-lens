{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
  ( someFunc, Todo
  ) where

import           Control.Lens
import           Control.Monad.Logger (LoggingT, logInfo, runStderrLoggingT)
import           Control.Monad.IO.Class 
import           Control.Monad.State
import qualified Data.Text            as T (pack)
import           Data.Time.Clock      (UTCTime, getCurrentTime)

data Todo = Todo
  { _title      :: String
  , _completed  :: Bool
  , _created_at :: UTCTime
  , _modified   :: UTCTime
  }
makeLenses ''Todo

-- newtype TodoStore = TodoStore {
--   _todos :: [Todo]
-- }
-- makeLenses ''TodoStore

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

type App = LoggingT (StateT Todo IO) ()

type LensModifierWithAction a b = ASetter Todo Todo a b -> (a -> b) -> App
type LensModifierWithValue  a b = ASetter Todo Todo a b -> b        -> App

modifyAndSetModified :: LensModifierWithAction a b
modifyAndSetModified lens' f = do
  lens' %= f
  now <- liftIO getCurrentTime
  modified .= now

setAndSetModified :: LensModifierWithValue a b
setAndSetModified lens' v = do
  lens' .= v
  now <- liftIO getCurrentTime
  modified .= now

(%:=) :: LensModifierWithAction a b
(%:=) = modifyAndSetModified

(.:=) :: LensModifierWithValue a b
(.:=) = setAndSetModified

test1 :: App
test1 = do
  -- Get the current title using the lens operator (^.)
  $(logInfo) "getting title..."
  currentTitle <- use title 
  dateCreated <- use created_at
  dateModified <- use modified
  liftIO $ putStrLn $ "Current title: " ++ currentTitle 
  liftIO $ putStrLn $ "Created at: " ++ show dateCreated
  liftIO $ putStrLn $ "Modified at: " ++ show dateModified
  -- Read a new title from the user
  $(logInfo) "prompting user for title..." 
  liftIO (putStrLn "Enter a new title:")

  $(logInfo) "getting title from user..."
  newTitle <- liftIO getLine 
  -- Update the title using the lens operator (.=)
  $(logInfo) "setting title..."
  title .:= newTitle
  use modified >>= (\time ->liftIO $ putStrLn $ "Modified at: " ++ show time)

  use title >>= (\title' -> liftIO $ putStrLn ("Updated title: " ++ title'))

someFunc :: IO ()
someFunc = do
  putStrLn "running test1..."
  currentTime <- getCurrentTime
  evalStateT (runStderrLoggingT test1) $ Todo
    { _title = "test"
    , _completed = False
    , _created_at = currentTime
    , _modified = currentTime
    }
  putStrLn "done"
