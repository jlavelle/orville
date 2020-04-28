{-|
Module    : Driver.Interface.Value
Copyright : Flipstone Technology Partners 2020
License   : MIT
-}

module Driver.Interface.Value
  (Value(unwrap))
where

class Value a where
  unwrap :: a
