module Runner
  ( performQuery
  , runNQueries
  ) where

import Control.Monad (forM_, replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parse)

import Query (Query (..), queryParser)
import QueryResult (QueryResult (..), toBytestring)
import Types (MonadTodoList (..))

performQuery :: (MonadTodoList m, MonadIO m) => Query -> m QueryResult
performQuery (Add d tags) = do
  index <- add d tags
  return $ Added index

performQuery (MakeDone index) = do
  done index
  return Done

performQuery (Search params) = do
  items <- search params
  return $ Found items

runNQueries :: (MonadTodoList m, MonadIO m) => Int -> m ()
runNQueries n =
  replicateM_ n $ do
    rawQuery <- liftIO $ B.getLine
    case parse queryParser "-" rawQuery of
      Left err -> do
        error (show err)

      Right parsedQuery -> do
        result <- performQuery parsedQuery
        liftIO $ B.putStrLn (toBytestring result)
