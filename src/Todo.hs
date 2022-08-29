{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import Data.List (intersect, map, deleteBy)
import Data.List.NonEmpty (toList)

import Types (MonadTodoList (..), TodoItem (..), Description (..), Index (..), Tag (..), SearchParams(..), SearchWord(..))

newtype TodoList = TodoList [TodoItem]
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList []

newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add descr tags = TodoListM $ do
    modify' addTodoItem
    getLatestIndexValue <$> get
    where
      addTodoItem (TodoList [])     = TodoList [TodoItem (Index 0) descr tags]
      addTodoItem (TodoList (x:xs)) = TodoList (TodoItem ((succ . tiIndex) x) descr tags : x : xs)

      getLatestIndexValue (TodoList xs) = (tiIndex . head) xs

  done index = TodoListM $ do
    modify' removeTodoItem
      where
        removeTodoItem (TodoList xs) = TodoList $
          filter (\x -> index /= tiIndex x) xs

  search params = TodoListM $ do
    filterSearchTodoItems <$> get
    where
      filterSearchTodoItems (TodoList xs) =
         filter (\x ->
          containsSearchWords (getDescription $ tiDescription x) || 
          containsTags (map getTag $ tiTags x)) xs

      containsSearchWords descr =
              descr `elem` map getSearchWord (spWords params) || 
              containsSubseq descr (map getSearchWord $ spWords params)

      containsTags tags = any (\tag ->
                  tag `elem` map getTag (spTags params) || 
                  containsSubseq tag (map getTag $ spTags params)) tags

      containsSubseq word searchParams = any
            (\s ->
              B.isPrefixOf s word ||
              B.isSuffixOf s word ||
              B.isInfixOf s word) searchParams