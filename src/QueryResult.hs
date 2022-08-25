module QueryResult
  ( QueryResult (..)
  , toBytestring
  ) where

import Control.Monad (void)
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, isLetter, isPrint)
import Data.Either (partitionEithers)
import Data.String (fromString)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import Types (Description (..), Index (..), Tag (..), TodoItem (..))

data QueryResult
  = Added Index
  | Done
  | Found [TodoItem]
  deriving (Show, Eq)

toBytestring :: QueryResult -> B.ByteString
toBytestring result = case result of
  Added i     -> fromString (show i)
  Done        -> "done"
  Found items ->
    fromString (show $ length items)
    <> foldMap itemToBytestring items
    where
      itemToBytestring (TodoItem (Index index) (Description d) tags) =
        "\n"
        <> fromString (show index)
        <> " \""
        <> d
        <> "\""
        <> foldMap showTag tags
        where
          showTag (Tag tag) = " #" <> tag
