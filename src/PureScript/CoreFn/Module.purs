module PureScript.CoreFn.Module where

import Data.Map (Map)
import PureScript.CoreFn.Expr (Bind)
import PureScript.CoreFn.Types (Comment, Ident, ModuleName, SourceSpan)

newtype Module a = Module
  { sourceSpan ∷ SourceSpan
  , comments ∷ Array Comment
  , moduleName ∷ ModuleName
  , modulePath ∷ String
  , imports ∷ Array { annotation ∷ a, moduleName ∷ ModuleName }
  , exports ∷ Array Ident
  , reExports ∷ Map ModuleName (Array Ident)
  , foreign ∷ Array Ident
  , decls ∷ Array (Bind a)
  }
