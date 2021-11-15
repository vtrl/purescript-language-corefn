module Main where

import Prelude

import Data.Argonaut.Core (Json, isNull)
import Data.Argonaut.Decode (JsonDecodeError, parseJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Traversable (for_)
import Data.String.Regex (Regex, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Glob.Basic (expandGlobs)
import Node.Process as Process
import PureScript.CoreFn.DecodeJson (moduleFromJson)
import PureScript.CoreFn.EncodeJson (moduleToJson)

foreign import diff ∷ Json → Json → Json

unmakyr :: Regex.Regex
unmakyr = Regex.unsafeRegex "(Unicode|Nested)" Regex.noFlags

parseCoreFn
  ∷ String → Effect (Either JsonDecodeError { moduleDecoded ∷ Json, moduleOriginal ∷ Json })
parseCoreFn filePath = do
  s ← readTextFile UTF8 filePath
  pure do
    j ← parseJson s
    m ← moduleFromJson j
    pure $
      { moduleOriginal: j
      , moduleDecoded: moduleToJson m.version m."module"
      }

main ∷ Effect Unit
main = launchAff_ do
  cwd ← liftEffect Process.cwd
  globs ← expandGlobs cwd [ "output/**/corefn.json" ]

  for_ globs \glob → do
    unless (Regex.test unmakyr glob) do
      log glob
      eCoreFn ← liftEffect $ parseCoreFn glob
      case eCoreFn of
        Left e → log $ "Failed parsing: " <> printJsonDecodeError e
        Right coreFn →
          if isNull $ diff coreFn.moduleOriginal coreFn.moduleDecoded then
            log $ "Success diffing: " <> glob
          else
            log $ "Failed diffing: " <> glob
