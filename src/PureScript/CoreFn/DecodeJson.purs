module PureScript.CoreFn.DecodeJson where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.Unsafe as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Version.Haskell (Version, parseVersion)
import Foreign.Object (Object, fromFoldable)
import PureScript.CoreFn.Ann (Ann(..))
import PureScript.CoreFn.Binder (Binder(..))
import PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import PureScript.CoreFn.Module (Module(..))
import PureScript.CoreFn.Types
  ( Comment(..)
  , Ident(..)
  , Literal(..)
  , ModuleName(..)
  , Proper(..)
  , Qualified(..)
  , SourcePos(..)
  , SourceSpan
  )
import Safe.Coerce (coerce)

type FilePath = String

constructorTypeFromJson ∷ Json → Either JsonDecodeError ConstructorType
constructorTypeFromJson ctorType = do
  o ← decodeJson ctorType
  case o of
    "ProductType" → pure ProductType
    "SumType" → pure SumType
    u → Left $ TypeMismatch $ "Unknown constructor type: " <> u

metaFromJson ∷ Json → Either JsonDecodeError (Maybe Meta)
metaFromJson meta = do
  o ← decodeJson meta
  case o of
    Just meta' → do
      o' ← decodeJson meta'
      metaType ← o' .: "metaType"
      case metaType of
        "IsConstructor" → do
          t ← o' .: "constructorType" >>= constructorTypeFromJson
          i ← o' .: "identifiers"
          pure $ Just $ IsConstructor t (coerce (i ∷ Array String))
        "IsNewtype" →
          pure $ Just IsNewtype
        "IsTypeClassConstructor" →
          pure $ Just IsTypeClassConstructor
        "IsForeign" →
          pure $ Just IsForeign
        "IsWhere" →
          pure $ Just IsWhere
        u →
          Left $ TypeMismatch $ "Unknown meta type: " <> u
    Nothing → pure Nothing

annFromJson ∷ FilePath → Json → Either JsonDecodeError Ann
annFromJson modulePath annotation = do
  o ← decodeJson annotation
  sourceSpan ← o .: "sourceSpan" >>= sourceSpanFromJson modulePath
  meta ← o .: "meta" >>= metaFromJson
  pure $ Ann { sourceSpan, meta }

sourceSpanFromJson ∷ FilePath → Json → Either JsonDecodeError SourceSpan
sourceSpanFromJson modulePath sourceSpan = do
  o ← decodeJson sourceSpan
  start ← o .: "start" >>= sourcePosFromJson
  end ← o .: "end" >>= sourcePosFromJson
  pure $
    { name: modulePath
    , start
    , end
    }
  where
  sourcePosFromJson o = do
    a ← decodeJson o
    line ← getIndex a 0
    column ← getIndex a 1
    pure $ SourcePos { line, column }

  getIndex a i =
    maybe
      (Left (AtIndex i MissingValue))
      (lmap (AtIndex i) <<< decodeJson)
      (Array.index a i)

literalFromJson
  ∷ ∀ a
  . (Json → Either JsonDecodeError a)
  → Json
  → Either JsonDecodeError (Literal a)
literalFromJson f l = do
  o ← decodeJson l
  literalType ← o .: "literalType"
  case literalType of
    "IntLiteral" →
      NumericLiteral <<< Left <$> o .: "value"
    "NumberLiteral" →
      NumericLiteral <<< Right <$> o .: "value"
    "StringLiteral" →
      StringLiteral <$> o .: "value"
    "CharLiteral" →
      CharLiteral <<< String.charAt 0 <$> o .: "value"
    "BooleanLiteral" →
      BooleanLiteral <$> o .: "value"
    "ArrayLiteral" → do
      v ← o .: "value"
      ArrayLiteral <$> traverse f v
    "ObjectLiteral" → do
      v ← o .: "value"
      ObjectLiteral <$> parseObjectLiteral v
    u →
      Left $ TypeMismatch $ "Unknown literal type: " <> u
  where
  getIndex a i =
    maybe
      (Left (AtIndex i MissingValue))
      (lmap (AtIndex i) <<< decodeJson)
      (Array.index a i)

  parseObjectLiteral ∷ Array (Array Json) → Either JsonDecodeError (Object a)
  parseObjectLiteral = map fromFoldable <<< traverse parsePair
    where
    parsePair a = do
      k ← getIndex a 0 >>= decodeJson
      v ← getIndex a 1 >>= f
      pure $ Tuple k v

identFromJson ∷ Json → Either JsonDecodeError Ident
identFromJson i = coerce (decodeJson i ∷ _ _ String)

properFromJson ∷ Json → Either JsonDecodeError Proper
properFromJson p = coerce (decodeJson p ∷ _ _ String)

moduleNameFromJson ∷ Json → Either JsonDecodeError ModuleName
moduleNameFromJson m = ModuleName <<< Array.intercalate "." <$> decodeJson m

qualifiedFromJson ∷ ∀ n. (String → n) → Json → Either JsonDecodeError (Qualified n)
qualifiedFromJson f m = do
  o ← decodeJson m
  moduleName ← o .:? "moduleName" >>= traverse moduleNameFromJson
  identifier ← o .: "identifier" >>= (pure <<< f)
  pure $ Qualified { "module": moduleName, name: identifier }

exprFromJson ∷ FilePath → Json → Either JsonDecodeError (Expr Ann)
exprFromJson modulePath e = do
  o ← decodeJson e
  exprType ← o .: "type"
  case exprType of
    "Var" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      q ← o .: "value" >>= qualifiedFromJson Ident
      pure $ Var a q
    "Literal" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      l ← o .: "value" >>= literalFromJson (exprFromJson modulePath)
      pure $ Literal a l
    "Constructor" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      t ← o .: "typeName" >>= properFromJson
      c ← o .: "constructorName" >>= properFromJson
      i ← o .: "fieldNames" >>= traverse identFromJson
      pure $ Constructor a t c i
    "Accessor" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      f ← o .: "fieldName"
      n ← o .: "expression" >>= exprFromJson modulePath
      pure $ Accessor a f n
    "ObjectUpdate" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      r ← o .: "expression" >>= exprFromJson modulePath
      u ← o .: "updates" >>= traverse (exprFromJson modulePath)
      pure $ ObjectUpdate a r u
    "Abs" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      i ← o .: "argument" >>= identFromJson
      b ← o .: "body" >>= exprFromJson modulePath
      pure $ Abs a i b
    "App" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      f ← o .: "abstraction" >>= exprFromJson modulePath
      x ← o .: "argument" >>= exprFromJson modulePath
      pure $ App a f x
    "Case" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      c ← o .: "caseExpressions" >>= traverse (exprFromJson modulePath)
      t ← o .: "caseAlternatives" >>= traverse (caseAlternativeFromJson modulePath)
      pure $ Case a c t
    "Let" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      b ← o .: "binds" >>= traverse (bindFromJson modulePath)
      r ← o .: "expression" >>= exprFromJson modulePath
      pure $ Let a b r
    u →
      Left $ TypeMismatch $ "Unknown expression type: " <> u

bindFromJson ∷ FilePath → Json → Either JsonDecodeError (Bind Ann)
bindFromJson modulePath b = do
  o ← decodeJson b
  bindType ← o .: "bindType"
  case bindType of
    "NonRec" → do
      b_ ← bindFromJson_ o
      pure $ NonRec b_.annotation b_.identifier b_.expression
    "Rec" → do
      Rec <$> (o .: "binds" >>= traverse bindFromJson_)
    u →
      Left $ TypeMismatch $ "Unknown bind type: " <> u
  where
  bindFromJson_ o = do
    annotation ← o .: "annotation" >>= annFromJson modulePath
    identifier ← o .: "identifier" >>= identFromJson
    expression ← o .: "expression" >>= exprFromJson modulePath
    pure $ { annotation, identifier, expression }

caseAlternativeFromJson ∷ FilePath → Json → Either JsonDecodeError (CaseAlternative Ann)
caseAlternativeFromJson modulePath c = do
  o ← decodeJson c
  b ← o .: "binders" >>= traverse (binderFromJson modulePath)
  isGuarded ← o .: "isGuarded"
  if isGuarded then do
    r ← o .: "expressions" >>= traverse \o' → do
      guard ← o' .: "guard" >>= exprFromJson modulePath
      expression ← o' .: "expression" >>= exprFromJson modulePath
      pure $ { guard, expression }
    pure $ CaseAlternative { binders: b, result: Left r }
  else do
    r ← o .: "expression" >>= exprFromJson modulePath
    pure $ CaseAlternative { binders: b, result: Right r }

binderFromJson ∷ FilePath → Json → Either JsonDecodeError (Binder Ann)
binderFromJson modulePath b = do
  o ← decodeJson b
  binderType ← o .: "binderType"
  case binderType of
    "NullBinder" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      pure $ NullBinder a
    "VarBinder" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      i ← o .: "identifier" >>= identFromJson
      pure $ VarBinder a i
    "LiteralBinder" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      l ← o .: "literal" >>= literalFromJson (binderFromJson modulePath)
      pure $ LiteralBinder a l
    "ConstructorBinder" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      t ← o .: "typeName" >>= qualifiedFromJson Proper
      c ← o .: "constructorName" >>= qualifiedFromJson Proper
      s ← o .: "binders" >>= traverse (binderFromJson modulePath)
      pure $ ConstructorBinder a t c s
    "NamedBinder" → do
      a ← o .: "annotation" >>= annFromJson modulePath
      i ← o .: "identifier" >>= identFromJson
      s ← o .: "binder" >>= binderFromJson modulePath
      pure $ NamedBinder a i s
    u →
      Left $ TypeMismatch $ "Unknown binder type: " <> u

moduleFromJson ∷ Json → Either JsonDecodeError { version ∷ Version, "module" ∷ Module Ann }
moduleFromJson m = do
  o ← decodeJson m
  version ← o .: "builtWith" >>= versionFromJson
  moduleName ← o .: "moduleName" >>= moduleNameFromJson
  modulePath ← o .: "modulePath"
  sourceSpan ← o .: "sourceSpan" >>= sourceSpanFromJson modulePath
  imports ← o .: "imports" >>= traverse (importFromJson modulePath)
  exports ← o .: "exports" >>= traverse identFromJson
  reExports ← o .: "reExports" <#> reExportsFromJson
  decls ← o .: "decls" >>= traverse (bindFromJson modulePath)
  foreign_ ← o .: "foreign" >>= traverse identFromJson
  comments ← o .: "comments" >>= traverse commentFromJson
  pure $
    { version
    , "module": Module
        { sourceSpan
        , comments
        , moduleName
        , modulePath
        , imports
        , exports
        , reExports
        , "foreign": foreign_
        , decls
        }
    }
  where
  versionFromJson v = case parseVersion v of
    Right v' → pure v'
    Left _ → Left $ TypeMismatch "Invalid version."

  importFromJson modulePath o = do
    annotation ← o .: "annotation" >>= annFromJson modulePath
    moduleName ← o .: "moduleName" >>= moduleNameFromJson
    pure { annotation, moduleName }

  reExportsFromJson ∷ Object (Array String) → (Map ModuleName (Array Ident))
  reExportsFromJson = coerce go
    where
    go ∷ Object (Array String) → (Map ModuleName (Array String))
    go = foldrWithIndex (\i → Map.insert (ModuleName i)) Map.empty

  commentFromJson c = do
    o ← decodeJson c
    (l ∷ _ String) ← o .:? "LineComment"
    (b ∷ _ String) ← o .:? "BlockComment"
    case l, b of
      Just l', Nothing → pure $ LineComment l'
      Nothing, Just b' → pure $ BlockComment b'
      _, _ → Left $ TypeMismatch "Invalid comment."
