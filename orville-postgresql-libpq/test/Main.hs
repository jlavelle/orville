module Main
  ( main,
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as B8
import qualified System.Exit as SE

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Test.AutoMigration as AutoMigration
import qualified Test.Connection as Connection
import qualified Test.EntityOperations as EntityOperations
import qualified Test.Expr.InsertUpdateDelete as ExprInsertUpdateDelete
import qualified Test.Expr.OrderBy as ExprOrderBy
import qualified Test.Expr.TableDefinition as ExprTableDefinition
import qualified Test.Expr.Where as ExprWhere
import qualified Test.FieldDefinition as FieldDefinition
import qualified Test.InformationSchema as InformationSchema
import qualified Test.PgCatalog as PgCatalog
import qualified Test.Plan as Plan
import qualified Test.Property as Property
import qualified Test.RawSql as RawSql
import qualified Test.ReservedWords as ReservedWords
import qualified Test.SelectOptions as SelectOptions
import qualified Test.SqlMarshaller as SqlMarshaller
import qualified Test.SqlType as SqlType
import qualified Test.TableDefinition as TableDefinition
import qualified Test.Transaction as Transaction

main :: IO ()
main = do
  let connBStr = B8.pack "host=testdb user=orville_test password=orville"
  pool <- Connection.createConnectionPool 1 10 1 connBStr

  summary <-
    Property.checkGroups
      [ Connection.connectionTests pool
      , RawSql.rawSqlTests
      , TableDefinition.tableDefinitionTests pool
      , SqlMarshaller.sqlMarshallerTests
      , FieldDefinition.fieldDefinitionTests pool
      , EntityOperations.entityOperationsTests pool
      , ExprInsertUpdateDelete.insertUpdateDeleteTests pool
      , ExprWhere.whereTests pool
      , ExprOrderBy.orderByTests pool
      , ExprTableDefinition.tableDefinitionTests pool
      , SqlType.sqlTypeTests pool
      , SelectOptions.selectOptionsTests
      , ReservedWords.reservedWordsTests pool
      , Transaction.transactionTests pool
      , Plan.planTests pool
      , InformationSchema.informationSchemaTests pool
      , PgCatalog.pgCatalogTests pool
      , AutoMigration.autoMigrationTests pool
      ]

  Monad.unless (Property.allPassed summary) SE.exitFailure
