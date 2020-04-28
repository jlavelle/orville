{-|
Module    : Driver.PostgreSQLSimple
Copyright : Flipstone Technology Partners 2020
License   : MIT
-}
module Driver.PostgreSQLSimple
  ()
where

import Database.PostgreSQL.Simple.ToField (Action)

instance Eq Action where
  -- This is an awful ugly hack, but it should tell us if two actions are the same modulo character encoding oddities...
  (==) x y = show x == show y
