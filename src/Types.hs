module Types
  ( Index (..)
  , SearchWord (..)
  , Description (..)
  , Tag (..)
  , SearchParams (..)
  , TodoItem (..)
  , MonadTodoList (..)
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty (NonEmpty)

newtype Index = Index { getIndex :: Word }
  deriving (Show, Eq, Enum, Bounded, Ord)

newtype Description = Description { getDescription :: ByteString }
  deriving (Show, Eq, Ord)

newtype Tag = Tag { getTag :: ByteString }
  deriving (Show, Eq, Ord)

newtype SearchWord = SearchWord { getSearchWord :: ByteString }
  deriving (Show, Eq, Ord)

data SearchParams = SearchParams
  { spWords :: ![SearchWord]
  , spTags  :: ![Tag]
  } deriving (Show, Eq)

data TodoItem = TodoItem
  { tiIndex       :: !Index
  , tiDescription :: !Description
  , tiTags        :: ![Tag]
  } deriving (Show, Eq, Ord)

class MonadTodoList m where
  add    :: Description -> [Tag] -> m Index
  done   :: Index -> m ()
  search :: SearchParams -> m [TodoItem]
