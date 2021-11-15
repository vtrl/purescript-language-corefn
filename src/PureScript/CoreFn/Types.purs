module PureScript.CoreFn.Types where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Object (Object)

newtype SourcePos = SourcePos
  { line ∷ Int
  , column ∷ Int
  }

type SourceSpan =
  { name ∷ String
  , start ∷ SourcePos
  , end ∷ SourcePos
  }

data Comment
  = LineComment String
  | BlockComment String

data Literal a
  = NumericLiteral (Either Int Number)
  | StringLiteral String
  | CharLiteral Char
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (Object a)

newtype Qualified a = Qualified
  { module ∷ Maybe ModuleName
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

newtype Version = Version
  { branch ∷ Array Int
  , tags ∷ Array String
  }

derive instance Newtype Version _
derive newtype instance Eq Version
derive newtype instance Ord Version

showVersion ∷ Version → String
showVersion (Version { branch, tags }) =
  Array.intercalate "." (show <$> branch) <> "-" <> Array.intercalate "-" tags
