module PureScript.CoreFn.Meta where

import Data.Eq (class Eq)
import PureScript.Common.Names (Ident)

data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere

derive instance Eq Meta

data ConstructorType
  = ProductType
  | SumType

derive instance Eq ConstructorType
