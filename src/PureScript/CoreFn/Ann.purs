module PureScript.CoreFn.Ann where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import PureScript.Common.Position (SourceSpan)
import PureScript.CoreFn.Meta (Meta)

newtype Ann = Ann
  { sourceSpan ∷ SourceSpan
  , meta ∷ Maybe Meta
  }

derive instance Eq Ann
