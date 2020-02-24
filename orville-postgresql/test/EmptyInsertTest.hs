module EmptyInsertTest where



import Control.Exception.Lifted (try)
import Control.Monad (void)
import Data.Int (Int32)

data Foobar key = Foobar key (Maybe Int32)

data FoobarId = FoobarId { unFoobarId :: Int32 }

test_emptyInsert :: TestTree
test_emptyInsert =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Empty Insert Test"
      [ testCase "Insert works when no columns are specified" $ do
          run (TestDB.reset schema)
          insertedEntry <- run (O.insertRecord emptyInsertTable $ Foobar ())
          assertEqual
            "Inserting into a table with no columns failed"
            (Foobar $ FoobarId 1)
            result
      ]

schema :: O.SchemaDefinition
schema = [O.Table emptyInsertTable]

emptyInsertTable :: O.TableDefinition (Foobar FoobarId) (Foobar ()) FoobarId
emptyInsertTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "empty_insert"
    , O.tblPrimaryKey = emptyInsertIdField
    , O.tblMapper = Virus <$> O.readOnlyField emptyInsertIdField
    , O.tblGetKey = emptyInsertId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

emptyInsertIdField :: O.FieldDefinition FoobarId
emptyInsertIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unFoobarId FoobarId
