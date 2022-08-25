module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify, runStateT)
import qualified Data.ByteString.Char8 as B
import Data.Char (isLetter, toLower)
import Data.Function (on)
import Data.List (intersect)
import Data.List.NonEmpty (toList)

import Types (MonadTodoList (..))

data TodoList = TodoList {}
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList =
  _emptyTodoList

newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add    = _add
  done   = _done
  search = _search
