module PureScript.CoreFn.Expr where

import Data.Either (Either)
import PureScript.CoreFn.Binder (Binder)
import PureScript.CoreFn.Types (Ident, Literal, Proper, Qualified)

data Expr a
  = Literal a (Literal (Expr a))
  | Constructor a Proper Proper (Array Ident)
  | Accessor a String (Expr a)
  | ObjectUpdate a (Expr a) (Array { key ∷ String, value ∷ Expr a })
  | Abs a Ident (Expr a)
  | App a (Expr a) (Expr a)
  | Var a (Qualified Ident)
  | Case a (Array (Expr a)) (Array (CaseAlternative a))
  | Let a (Array (Bind a)) (Expr a)

data Bind a
  = NonRec a Ident (Expr a)
  | Rec (Array { ann ∷ a, ident ∷ Ident, expr ∷ Expr a })

type Guard = Expr

newtype CaseAlternative a = CaseAlternative
  { binders ∷ Array (Binder a)
  , result ∷ Either (Array { guard ∷ Guard a, expr ∷ Expr a }) (Expr a)
  }
