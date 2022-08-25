module Query
  ( Query (..)
  , queryParser
  ) where

import Control.Monad (void)
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, isLetter, isPrint)
import Data.Either (partitionEithers)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import Types (Description (..), Index (..), SearchParams (..), SearchWord (..), Tag (..),
              TodoItem (..))

data Query
  = Add      Description [Tag]
  | MakeDone Index
  | Search   SearchParams
  deriving (Show, Eq)

type Parser = Parsec Void B.ByteString

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: B.ByteString -> Parser ()
symbol = void . L.symbol space

toChar :: Word8 -> Char
toChar = chr . fromIntegral

queryParser :: Parser Query
queryParser = addQueryParser <|> doneQueryParser <|> searchQueryParser

addQueryParser :: Parser Query
addQueryParser = do
  symbol "add"
  uncurry Add <$> descriptionAndTagsParser

descriptionAndTagsParser :: Parser (Description, [Tag])
descriptionAndTagsParser = do
  let check c = isPrint c && c /= '"'
  description <- string "\"" *> takeWhileP Nothing (check . toChar) <* symbol "\""
  tags <- many $ lexeme tagParser
  pure (Description description, tags)

doneQueryParser :: Parser Query
doneQueryParser = do
  symbol "done"
  index <- Index <$> lexeme L.decimal
  pure $ MakeDone index

searchQueryParser :: Parser Query
searchQueryParser = do
  symbol "search"
  params <- many $ lexeme (Left <$> letterStringParser <|> Right <$> tagParser)
  let (wordParams, tagParams) = partitionEithers params
  pure $ Search $ SearchParams wordParams tagParams

tagParser :: Parser Tag
tagParser = string "#" >> Tag <$> word

letterStringParser :: Parser SearchWord
letterStringParser = SearchWord <$> word

word :: Parser B.ByteString
word = takeWhile1P Nothing (isLetter . toChar)
