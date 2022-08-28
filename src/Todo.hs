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

import Types (MonadTodoList (..), TodoItem (..), Description (..), Index (..), Tag (..), SearchParams(..), SearchWord(..))

newtype TodoList = TodoList [TodoItem]
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList []

incrementIndex :: Index -> Index
incrementIndex = Index . (+) 1 . getIndex

newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add descr tags = do
     newTodoList <- TodoListM $ withStateT addTodoItem get
     pure $ getIndex newTodoList
    where
      addTodoItem (TodoList [])     = TodoList [TodoItem (Index 0) descr tags]
      addTodoItem (TodoList (x:xs)) = TodoList $ TodoItem (incrementIndex $ tiIndex x) (tiDescription x) (tiTags x) : x : xs

      getIndex (TodoList xs) = tiIndex $ head xs

  done index =
    void $ TodoListM $ withStateT removeTodoItem get
      where
        removeTodoItem (TodoList []) = TodoList []
        removeTodoItem (TodoList xs) = TodoList $ filter (\x -> tiIndex x == index) xs

  search params = do
    newTodoList <- TodoListM $ withStateT filterSearchTodoItems get
    case newTodoList of
      TodoList xs -> pure xs
    where
      filterSearchTodoItems (TodoList xs) = TodoList $
        filter (\x ->
          containsSearchWords (getDescription $ tiDescription x) ||
          containsTags (map getTag $ tiTags x)) xs

      containsSearchWords descr =
              descr `elem` map getSearchWord (spWords params)
          || containsSubseq descr (map getSearchWord $ spWords params)

      containsTags tags = not (any (\tag ->
                  tag `elem` map getTag (spTags params)
                || containsSubseq tag (map getTag $ spTags params)) tags)

      containsSubseq word searchParams = not (any
            (\s ->
              B.isPrefixOf s word ||
              B.isSuffixOf s word ||
              B.isInfixOf s word) searchParams)