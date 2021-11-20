module PureScript.CoreFn.Expr where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either)
import Foreign.Object (Object)
import PureScript.CoreFn.Binder (Binder)
import PureScript.CoreFn.Types (Ident, Literal, Proper, Qualified)

data Expr a
  = Literal a (Literal (Expr a))
  | Constructor a Proper Proper (Array Ident)
  | Accessor a String (Expr a)
  | ObjectUpdate a (Expr a) (Object (Expr a))
  | Abs a Ident (Expr a)
  | App a (Expr a) (Expr a)
  | Var a (Qualified Ident)
  | Case a (Array (Expr a)) (Array (CaseAlternative a))
  | Let a (Array (Bind a)) (Expr a)

derive instance Eq a ⇒ Eq (Expr a)
derive instance Functor Expr

data Bind a
  = NonRec a Ident (Expr a)
  | Rec (Array { annotation ∷ a, identifier ∷ Ident, expression ∷ Expr a })

derive instance Eq a ⇒ Eq (Bind a)
derive instance Functor Bind

type Guard = Expr

newtype CaseAlternative a = CaseAlternative
  { binders ∷ Array (Binder a)
  , result ∷ Either (Array { guard ∷ Guard a, expression ∷ Expr a }) (Expr a)
  }

derive instance Eq a ⇒ Eq (CaseAlternative a)

instance Functor CaseAlternative where
  map f (CaseAlternative { binders, result }) =
    let
      f' { guard, expression } =
        { guard: map f guard
        , expression: map f expression
        }
    in
      CaseAlternative
        { binders: map (map f) binders
        , result: bimap (map f') (map f) result
        }
