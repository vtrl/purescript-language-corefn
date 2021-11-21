module PureScript.CoreFn.Types where

import Prelude

import Data.Either (Either)
import Foreign.Object (Object)

data Comment
  = LineComment String
  | BlockComment String

derive instance Eq Comment

data Literal a
  = NumericLiteral (Either Int Number)
  | StringLiteral String
  | CharLiteral Char
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (Object a)

derive instance Eq a ⇒ Eq (Literal a)
derive instance Functor Literal
