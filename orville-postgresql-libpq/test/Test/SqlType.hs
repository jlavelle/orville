module Test.SqlType
  ( sqlTypeSpecs
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO)) 
import Data.Pool (Pool)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Time as Time
import Test.Tasty.Hspec (Spec, describe, it, testSpec, shouldBe)

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

sqlTypeSpecs pool = describe "Tests of SqlType encode/decode" $ do
  integerSpecs pool
  bigIntegerSpecs pool
  serialSpecs pool
  doubleSpecs pool
  boolSpecs pool
  unboundedTextSpecs pool
  fixedTextSpecs pool
  boundedTextSpecs pool
  textSearchVectorSpecs pool
  dateSpecs pool

integerSpecs pool = do
  roundTripSqlTypeTest
    pool
    integer
    "myinteger"
    "INTEGER"
    0
  roundTripSqlTypeTest
    pool
    integer
    "myinteger"
    "INTEGER"
    2147483647  
  roundTripSqlTypeTest
    pool
    integer
    "myinteger"
    "INTEGER"
    (-2147483648)

bigIntegerSpecs pool = do
  roundTripSqlTypeTest
    pool
    bigInteger
    "mybiginteger"
    "BIGINT"
    0
  roundTripSqlTypeTest
    pool
    bigInteger
    "mybiginteger"
    "BIGINT"
    21474836470

serialSpecs pool = do
  roundTripSqlTypeTest
    pool
    serial
    "myserial"
    "SERIAL"
    0
  roundTripSqlTypeTest
    pool
    serial
    "myserial"
    "SERIAL"
    2147483647  
  roundTripSqlTypeTest
    pool
    serial
    "myserial"
    "SERIAL"
    (-2147483648)

bigSerialSpecs pool = do
  roundTripSqlTypeTest
    pool
    bigserial
    "mybigserial"
    "BIGSERIAL"
    0
  roundTripSqlTypeTest
    pool
    bigserial
    "mybigserial"
    "BIGSERIAL"
    21474836470

doubleSpecs pool = do
  roundTripSqlTypeTest
    pool
    double
    "mydouble"
    "DOUBLE PRECISION"
    0.0
  roundTripSqlTypeTest
    pool
    double
    "mydouble"
    "DOUBLE PRECISION"
    1.5

boolSpecs pool = do
  roundTripSqlTypeTest
    pool
    boolean
    "mybool"
    "BOOL"
    False
  roundTripSqlTypeTest
    pool
    boolean
    "mybool"
    "BOOL"
    True

unboundedTextSpecs pool = do
  roundTripSqlTypeTestText
    pool
    unboundedText
    "myunboundedtext"
    "TEXT"
    (T.pack "abcde")


fixedTextSpecs pool = do
  roundTripSqlTypeTestText
    pool
    (fixedText 5)
    "myfixedtext"
    "CHAR(5)"
    (T.pack "abcde")

  roundTripSqlTypeTestText
    pool
    (fixedText 5)
    "myfixedtext"
    "CHAR(5)"
    (T.pack "fghi ")

boundedTextSpecs pool = do
  roundTripSqlTypeTestText
    pool
    (boundedText 5)
    "myboundedtext"
    "VARCHAR(5)"
    (T.pack "abcde")

  roundTripSqlTypeTestText
    pool
    (boundedText 5)
    "myboundedtext"
    "VARCHAR(5)"
    (T.pack "fghi")
    
textSearchVectorSpecs pool = do
  roundTripSqlTypeTestTextSearch
    pool
    textSearchVector
    "mytsvector"
    "TSVECTOR"
    (T.pack "abcde")

  roundTripSqlTypeTestTextSearch
    pool
    textSearchVector
    "mytsvector"
    "TSVECTOR"
    (T.pack "fghi")

dateSpecs pool = do
  roundTripSqlTypeTest
    pool
    date
    "mydate"
    "Date"
    (Time.fromGregorian 2020 12 21)


roundTripSqlTypeTest :: (Show a, Eq a) => Pool Connection -> SqlType a -> String -> String -> a -> Spec
roundTripSqlTypeTest pool sqlType tableName sqlTypeDDL' value = it ("Testing the insert/decode of " <> sqlTypeDDL' <> " with value " <> show  value) $ do
  executeRawVoid pool $ B8.pack $ "create table if not exists " <> tableName <> "(foo " <> sqlTypeDDL' <> ")"
  executeRawVoid pool $ B8.pack $ "truncate table " <> tableName

  let insertBS value' = Expr.insertExprToSql (Expr.InsertExpr (T.pack tableName) [T.pack $ show value'])
  print $ insertBS value
  executeRawVoid pool $ insertBS value

  let selectBS = Expr.queryExprToSql (Expr.QueryExpr [T.pack "*"] $ Expr.TableExpr (T.pack tableName))
  maybeResult <- executeRaw pool selectBS

  case maybeResult of
    Nothing ->
      shouldBe True False

    Just res -> do
      (maybeA:_) <- decodeRows res sqlType
      shouldBe maybeA (Just value)


roundTripSqlTypeTestText :: Pool Connection -> SqlType T.Text -> String -> String -> T.Text -> Spec
roundTripSqlTypeTestText pool sqlType tableName sqlTypeDDL' value = it ("Testing the insert/decode of " <> sqlTypeDDL' <> " with value " <> show  value) $ do
  executeRawVoid pool $ B8.pack $ "create table if not exists " <> tableName <> "(foo " <> sqlTypeDDL' <> ")"
  executeRawVoid pool $ B8.pack $ "truncate table " <> tableName

  let escapedValueForInsert = (T.pack "\'") <> value <> (T.pack "\'")
  let insertBS = Expr.insertExprToSql (Expr.InsertExpr (T.pack tableName) [escapedValueForInsert])
  print $ insertBS
  executeRawVoid pool $ insertBS

  let selectBS = Expr.queryExprToSql (Expr.QueryExpr [T.pack "*"] $ Expr.TableExpr (T.pack tableName))
  maybeResult <- executeRaw pool selectBS

  case maybeResult of
    Nothing ->
      shouldBe True False

    Just res -> do
      (maybeA:_) <- decodeRows res sqlType
      shouldBe maybeA (Just value)


roundTripSqlTypeTestTextSearch :: Pool Connection -> SqlType T.Text -> String -> String -> T.Text -> Spec
roundTripSqlTypeTestTextSearch pool sqlType tableName sqlTypeDDL' value = it ("Testing the insert/decode of " <> sqlTypeDDL' <> " with value " <> show  value) $ do
  executeRawVoid pool $ B8.pack $ "create table if not exists " <> tableName <> "(foo " <> sqlTypeDDL' <> ")"
  executeRawVoid pool $ B8.pack $ "truncate table " <> tableName

  let escapedValue = (T.pack "\'") <> value <> (T.pack "\'")
  let insertBS = Expr.insertExprToSql (Expr.InsertExpr (T.pack tableName) [escapedValue])
  print $ insertBS
  executeRawVoid pool $ insertBS

  let selectBS = Expr.queryExprToSql (Expr.QueryExpr [T.pack "*"] $ Expr.TableExpr (T.pack tableName))
  maybeResult <- executeRaw pool selectBS

  case maybeResult of
    Nothing ->
      shouldBe True False

    Just res -> do
      (maybeA:_) <- decodeRows res sqlType
      shouldBe maybeA (Just escapedValue)
