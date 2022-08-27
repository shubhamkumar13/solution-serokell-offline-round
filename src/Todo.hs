module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void, join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify', withStateT, runStateT)
import qualified Data.ByteString.Char8 as B
import Data.Char (isLetter, toLower)
import Data.Function (on)
import Data.List (intersect, map)
import Data.List.NonEmpty (toList)

import Types (MonadTodoList (..), TodoItem (..), Description (..), Index (..), Tag (..))

newtype TodoList = TodoList [TodoItem]
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList []

incrementIndex :: Index -> Index
incrementIndex = Index . (+) 1 . getIndex

createTodoItem :: Index -> Description -> [Tag] -> TodoItem
createTodoItem index descr tags
  | index == Index 0 = TodoItem
                        (Index . getIndex $ index)
                        (Description . getDescription $ descr)
                        (map (Tag . getTag) tags)
  | otherwise        = TodoItem
                        (incrementIndex index)
                        (Description . getDescription $ descr)
                        (map (Tag . getTag) tags)


newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add descr tags = do
     newTodoList <- TodoListM $ withStateT addTodoItem get
     pure $ getIndex newTodoList
    where
      addTodoItem (TodoList [])     = TodoList [createTodoItem (Index 0) descr tags]
      addTodoItem (TodoList (x:xs)) = TodoList $ createTodoItem (tiIndex x) (tiDescription x) (tiTags x) : x : xs

      getIndex (TodoList xs) = tiIndex $ head xs

  done index =
    void $ TodoListM $ withStateT removeTodoItem get
      where
        removeTodoItem (TodoList []) = TodoList []
        removeTodoItem (TodoList xs) = TodoList $ filter (\x -> tiIndex x == index) xs