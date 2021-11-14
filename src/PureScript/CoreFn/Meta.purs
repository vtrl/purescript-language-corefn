module PureScript.CoreFn.Meta where

import PureScript.CoreFn.Types (Ident)

data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere

data ConstructorType
  = ProductType
  | SumType
