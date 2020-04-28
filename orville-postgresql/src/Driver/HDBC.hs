{-|
Module    : Driver.HDBC
Copyright : Flipstone Technology Partners 2020
License   : MIT
-}

module Driver.HDBC
  (Value)
where

import Database.HDBC (SqlValue)

newtype Value = Value SqlValue
