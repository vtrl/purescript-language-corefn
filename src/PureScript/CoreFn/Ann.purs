module PureScript.CoreFn.Ann where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import PureScript.CoreFn.Meta (Meta)
import PureScript.CoreFn.Types (SourceSpan)

newtype Ann = Ann
  { sourceSpan ∷ SourceSpan
  , meta ∷ Maybe Meta
  }

derive instance Eq Ann
