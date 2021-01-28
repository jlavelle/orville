module Main
  ( main
  ) where

import Data.Pool (Pool)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (Spec, testSpec)

import Database.Orville.PostgreSQL.Connection (Connection, createConnectionPool, executeRawVoid, executeRaw)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (decodeRows)
import Database.Orville.PostgreSQL.Internal.SqlType (SqlType
                                                      -- numeric types
                                                    , integer
                                                    , serial
                                                    , bigInteger
                                                    , bigserial
                                                    , double

                                                    -- textual-ish types
                                                    , boolean
                                                    , unboundedText
                                                    , fixedText
                                                    , boundedText
                                                    , textSearchVector

                                                    -- date types
                                                    , date
                                                    , timestamp
                                                    )
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import Test.SqlType (sqlTypeSpecs)

{- The following are just the beginnings of tests that for now at least let us inspect manually
  if the sqltypes are in fact
-}
main :: IO ()
main = do
  let connBStr = B8.pack $ "host=testdb user=orville_test password=orville"
  pool <- createConnectionPool 1 10 1 connBStr

  testTree <- testSpec "specs" $ do
    sqlTypeSpecs pool

  defaultMain testTree

-- -- VARCHAR
--   visualTest
--     pool
--     (boundedText 5)
--     "myboundedtext"
--     "VARCHAR(5)"
--     [ "'abcde'", "'fghi'"]

-- -- TSVector
--   visualTest
--     pool
--     (textSearchVector)
--     "mytsvector"
--     "TSVECTOR"
--     [ "'abcde'", "'fghi'"]

-- -- Date
--   visualTest
--     pool
--     date
--     "mydate"
--     "DATE"
--     [ "'2020-12-21 14:30:32-00'", "'2020-12-21 14:30:32+00'" ]

-- -- UTCTime
--   visualTest
--     pool
--     timestamp
--     "myutctime"
--     "Timestamp with time zone"
--     [ "'2020-12-21 14:30:32-00'", "'2020-12-21 14:30:32+00'" ]
