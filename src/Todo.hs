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
    modify' (TodoList <$> addTodoItem)
    getLatestIndexValue <$> get
    where
      addTodoItem (TodoList [])     = [TodoItem (Index 0) descr tags]
      addTodoItem (TodoList (x:xs)) = TodoItem ((succ . tiIndex) x) descr tags : x : xs

      getLatestIndexValue (TodoList xs) = (tiIndex . head) xs

  done index = TodoListM $ do
    modify' removeTodoItem
      where
        removeTodoItem (TodoList xs) = TodoList $ filter ((index /=) . tiIndex) xs

  search params = TodoListM $ do
    filterTodoItems <$> get
    where
      filterTodoItems (TodoList xs) = filter (\x -> condWords x || condTags x) xs

      -- Conditions to filter the query parameters
      condWords = containsSearchWords <$> getDescription . tiDescription
      condTags  = containsTags <$> map getTag . tiTags

      -- Check for query parameters in the TodoList
      containsSearchWords descr = 
              descr `elem` searchWordStrings ||
              containsSubseq descr searchWordStrings

      containsTags tags = any (\tag ->
              tag `elem` tagStrings ||
              containsSubseq tag tagStrings) tags
      
      -- Check for partial matches in queries
      containsSubseq word = any (\s ->
              B.isPrefixOf s word ||
              B.isSuffixOf s word ||
              B.isInfixOf s word)

      -- Convert the query parameters as ByteStrings
      searchWordStrings = (map getSearchWord . spWords) params
      tagStrings        = (map getTag . spTags) params