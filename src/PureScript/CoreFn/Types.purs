module PureScript.CoreFn.Types where

import Prelude

import Data.Newtype (class Newtype)
import Data.Either (Either)

type SourcePos =
  { line ∷ Int
  , column ∷ Int
  }

type SourceSpan =
  { name ∷ String
  , start ∷ SourcePos
  , end ∷ SourcePos
  }

data Literal a
  = NumericLiteral (Either Int Number)
  | StringLiteral String
  | CharLiteral Char
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (Array { key ∷ String, value ∷ a })

newtype Qualified a = Qualified
  { module ∷ ModuleName
  , name ∷ a
  }

derive instance Newtype (Qualified a) _
derive newtype instance (Eq a) ⇒ Eq (Qualified a)
derive newtype instance (Ord a) ⇒ Ord (Qualified a)

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName

newtype Ident = Ident String

derive instance Newtype Ident _
derive newtype instance Eq Ident
derive newtype instance Ord Ident

newtype Proper = Proper String

derive instance Newtype Proper _
derive newtype instance Eq Proper
derive newtype instance Ord Proper
