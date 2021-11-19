module PureScript.CoreFn.Binder where

import Prelude

import PureScript.CoreFn.Types (Ident, Literal, Proper, Qualified)

data Binder a
  = NullBinder a
  | LiteralBinder a (Literal (Binder a))
  | VarBinder a Ident
  | ConstructorBinder a (Qualified Proper) (Qualified Proper) (Array (Binder a))
  | NamedBinder a Ident (Binder a)

derive instance Eq a => Eq (Binder a)
derive instance Functor Binder
