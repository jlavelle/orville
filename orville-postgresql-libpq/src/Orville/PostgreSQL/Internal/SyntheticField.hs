module Orville.PostgreSQL.Internal.SyntheticField
  ( SyntheticField,
    syntheticFieldExpression,
    syntheticFieldAlias,
    syntheticFieldValueFromSqlValue,
    syntheticField,
    nullableSyntheticField,
  )
where

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.FieldDefinition (FieldName, stringToFieldName)
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A 'SyntheticField' can be used to evaluate a SQL expression based on the
  columns of a table when records are selected from the database. Synthetic
  fields are inherently read-only.
-}
data SyntheticField a = SyntheticField
  { _syntheticFieldExpression :: Expr.ValueExpression
  , _syntheticFieldAlias :: FieldName
  , _syntheticFieldValueFromSqlValue :: SqlValue.SqlValue -> Maybe a
  }

{- |
  Returns the SQL expression that should be in with select statements to
  calculated the sythetic field.
-}
syntheticFieldExpression :: SyntheticField a -> Expr.ValueExpression
syntheticFieldExpression =
  _syntheticFieldExpression

{- |
  Returns the alias that should be used in select statements to name the
  the synthetic field.
-}
syntheticFieldAlias :: SyntheticField a -> FieldName
syntheticFieldAlias =
  _syntheticFieldAlias

{- |
  Decodes a calculated value selected from the database to its expected
  Haskell type. Returns 'Nothing' if the decoding fails.
-}
syntheticFieldValueFromSqlValue :: SyntheticField a -> SqlValue.SqlValue -> Maybe a
syntheticFieldValueFromSqlValue =
  _syntheticFieldValueFromSqlValue

{- |
  Constructs a 'SyntheticField' that will select a SQL expression using
  the given alias.
-}
syntheticField ::
  -- | The SQL expression to be selected
  Expr.ValueExpression ->
  -- | The alias to be used to name the calculation in SQL experios
  String ->
  -- | A function to decode the expression result from a 'SqlValue.SqlValue'
  (SqlValue.SqlValue -> Maybe a) ->
  SyntheticField a
syntheticField expression alias fromSqlValue =
  SyntheticField
    { _syntheticFieldExpression = expression
    , _syntheticFieldAlias = stringToFieldName alias
    , _syntheticFieldValueFromSqlValue = fromSqlValue
    }

{- |
  Modifies a 'SyntheticField' to allow it to decode @NULL@ values.
-}
nullableSyntheticField :: SyntheticField a -> SyntheticField (Maybe a)
nullableSyntheticField synthField =
  synthField
    { _syntheticFieldValueFromSqlValue = \sqlValue ->
        if SqlValue.isSqlNull sqlValue
          then Just Nothing
          else Just <$> syntheticFieldValueFromSqlValue synthField sqlValue
    }
