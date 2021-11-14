module PureScript.CoreFn.Ann where

import Data.Maybe (Maybe)
import PureScript.CoreFn.Meta (Meta)
import PureScript.CoreFn.Types (SourceSpan)

newtype Ann = Ann
  { sourceSpan ∷ SourceSpan
  , meta ∷ Maybe Meta
  }
