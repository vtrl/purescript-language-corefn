module PureScript.CoreFn.EncodeJson where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromArray
  , fromBoolean
  , fromNumber
  , fromObject
  , fromString
  , jsonNull
  )
import Data.Either (Either(..), isLeft)
import Data.Int (toNumber)
import Data.Map (toUnfoldable)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Data.String.CodeUnits as SCU
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Foreign.Object (fromHomogeneous, insert, singleton)
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
  , Version
  , showVersion
  )

constructorTypeToJson ∷ ConstructorType → Json
constructorTypeToJson = case _ of
  ProductType → fromString "ProductType"
  SumType → fromString "SumType"

metaToJson ∷ Meta → Json
metaToJson = case _ of
  IsConstructor c i →
    fromObject $ fromHomogeneous
      { metaType: fromString "IsConstructor"
      , constructorType: constructorTypeToJson c
      , identifiers: fromArray $ identToJson <$> i
      }
  IsNewtype →
    fromObject $ singleton "metaType" (fromString "IsNewtype")
  IsTypeClassConstructor →
    fromObject $ singleton "metaType" (fromString "IsTypeClassConstructor")
  IsForeign →
    fromObject $ singleton "metaType" (fromString "IsForeign")
  IsWhere →
    fromObject $ singleton "metaType" (fromString "IsWhere")

sourceSpanToJson ∷ SourceSpan → Json
sourceSpanToJson { start, end } =
  fromObject $ fromHomogeneous
    { start: sourcePosToJson start
    , end: sourcePosToJson end
    }
  where
  sourcePosToJson ∷ SourcePos → Json
  sourcePosToJson (SourcePos { line, column }) =
    fromArray
      [ fromNumber <<< toNumber $ line
      , fromNumber <<< toNumber $ column
      ]

annToJson ∷ Ann → Json
annToJson (Ann { sourceSpan, meta }) =
  fromObject $ fromHomogeneous
    { sourceSpan: sourceSpanToJson sourceSpan
    , meta: maybe jsonNull metaToJson meta
    }

commentToJson ∷ Comment → Json
commentToJson = case _ of
  LineComment t → fromObject $ singleton "LineComment" (fromString t)
  BlockComment t → fromObject $ singleton "BlockComment" (fromString t)

literalToJson ∷ ∀ a. (a → Json) → Literal a → Json
literalToJson f = case _ of
  NumericLiteral (Left n) →
    mkLiteral "IntLiteral" (fromNumber <<< toNumber $ n)
  NumericLiteral (Right n) →
    mkLiteral "NumberLiteral" (fromNumber n)
  StringLiteral s →
    mkLiteral "StringLiteral" (fromString $ s)
  CharLiteral c →
    mkLiteral "CharLiteral" (fromString <<< SCU.singleton $ c)
  BooleanLiteral b →
    mkLiteral "BooleanLiteral" (fromBoolean b)
  ArrayLiteral a →
    mkLiteral "ArrayLiteral" (fromArray $ f <$> a)
  ObjectLiteral o →
    mkLiteral "ObjectLiteral" (fromObject $ f <$> o)
  where
  mkLiteral ∷ String → Json → Json
  mkLiteral literalType value =
    fromObject $ fromHomogeneous
      { literalType: fromString literalType
      , value
      }

identToJson ∷ Ident → Json
identToJson (Ident n) = fromString n

properToJson ∷ Proper → Json
properToJson (Proper n) = fromString n

qualifiedToJson ∷ ∀ n. (n → String) → Qualified n → Json
qualifiedToJson f (Qualified q) = fromObject $ fromHomogeneous
  { moduleName: maybe jsonNull moduleNameToJson q."module"
  , identifier: fromString $ f q.name
  }

moduleNameToJson ∷ ModuleName → Json
moduleNameToJson (ModuleName n) =
  fromArray $ fromString <$> split (Pattern " ") n

bindToJson ∷ Bind Ann → Json
bindToJson = case _ of
  NonRec a i e →
    fromObject $ fromHomogeneous
      { bindType: fromString "NonRec"
      , annotation: annToJson a
      , identifier: identToJson i
      , expression: exprToJson e
      }
  Rec xs →
    fromObject $ fromHomogeneous
      { bindType: fromString "Rec"
      , binds: fromArray $ xs <#> \{ identifier, annotation, expression } →
          fromObject $ fromHomogeneous
            { identifier: identToJson identifier
            , annotation: annToJson annotation
            , expression: exprToJson expression
            }
      }

exprToJson ∷ Expr Ann → Json
exprToJson = case _ of
  Var annotation value →
    fromObject $ fromHomogeneous
      { "type": fromString "Var"
      , annotation: annToJson annotation
      , value: qualifiedToJson (\(Ident n) → n) value
      }
  Literal annotation value →
    fromObject $ fromHomogeneous
      { "type": fromString "Literal"
      , annotation: annToJson annotation
      , value: literalToJson exprToJson value
      }
  Constructor annotation typeName constructorName fieldNames →
    fromObject $ fromHomogeneous
      { "type": fromString "Constructor"
      , annotation: annToJson annotation
      , typeName: properToJson typeName
      , constructorName: properToJson constructorName
      , fieldNames: fromArray $ identToJson <$> fieldNames
      }
  Accessor annotation fieldName expression →
    fromObject $ fromHomogeneous
      { "type": fromString "Accessor"
      , annotation: annToJson annotation
      , fieldName: fromString fieldName
      , expression: exprToJson expression
      }
  ObjectUpdate annotation expression updates →
    fromObject $ fromHomogeneous
      { "type": fromString "ObjectUpdate"
      , annotation: annToJson annotation
      , expression: exprToJson expression
      , updates: fromObject $ exprToJson <$> updates
      }
  Abs annotation argument body →
    fromObject $ fromHomogeneous
      { "type": fromString "Abs"
      , annotation: annToJson annotation
      , argument: identToJson argument
      , body: exprToJson body
      }
  App annotation abstraction argument →
    fromObject $ fromHomogeneous
      { "type": fromString "Abs"
      , annotation: annToJson annotation
      , abstraction: exprToJson abstraction
      , argument: exprToJson argument
      }
  Case annotation caseExpressions caseAlternatives →
    fromObject $ fromHomogeneous
      { "type": fromString "Case"
      , annotation: annToJson annotation
      , caseExpressions: fromArray $ exprToJson <$> caseExpressions
      , caseAlternatives: fromArray $ caseAlternativeToJson <$> caseAlternatives
      }
  Let annotation binds expression →
    fromObject $ fromHomogeneous
      { "type": fromString "Case"
      , annotation: annToJson annotation
      , binds: fromArray $ bindToJson <$> binds
      , expression: exprToJson expression
      }

caseAlternativeToJson ∷ CaseAlternative Ann → Json
caseAlternativeToJson (CaseAlternative { binders, result }) =
  let
    isGuarded ∷ Json
    isGuarded = fromBoolean $ isLeft result

    name ∷ String
    name = case result of
      Left _ → "expressions"
      Right _ → "expression"

    value ∷ Json
    value = case result of
      Left r → fromArray $ r <#> \{ guard, expression } →
        fromObject $ fromHomogeneous
          { guard: exprToJson guard
          , expression: exprToJson expression
          }
      Right r → exprToJson r
  in
    fromObject $ insert name value $ fromHomogeneous
      { binders: fromArray $ binderToJson <$> binders
      , isGuarded
      }

binderToJson ∷ Binder Ann → Json
binderToJson = case _ of
  VarBinder annotation identifier →
    fromObject $ fromHomogeneous
      { binderType: fromString "VarBinder"
      , annotation: annToJson annotation
      , identifier: identToJson identifier
      }
  NullBinder annotation →
    fromObject $ fromHomogeneous
      { binderType: fromString "NullBinder"
      , annotation: annToJson annotation
      }
  LiteralBinder annotation literal →
    fromObject $ fromHomogeneous
      { binderType: fromString "NullBinder"
      , annotation: annToJson annotation
      , literal: literalToJson binderToJson literal
      }
  ConstructorBinder annotation typeName constructorName binders →
    fromObject $ fromHomogeneous
      { binderType: fromString "ConstructorBinder"
      , annotation: annToJson annotation
      , typeName: qualifiedToJson (\(Proper n) → n) typeName
      , constructorName: qualifiedToJson (\(Proper n) → n) constructorName
      , binders: fromArray $ binderToJson <$> binders
      }
  NamedBinder annotation identifier binder →
    fromObject $ fromHomogeneous
      { binderType: fromString "NamedBinder"
      , annotation: annToJson annotation
      , identifier: identToJson identifier
      , binder: binderToJson $ binder
      }

moduleToJson ∷ Version → Module Ann → Json
moduleToJson v (Module m) =
  fromObject $ fromHomogeneous
    { sourceSpan: sourceSpanToJson m.sourceSpan
    , moduleName: moduleNameToJson m.moduleName
    , modulePath: fromString m.modulePath
    , imports: fromArray $ importToJson <$> m.imports
    , exports: fromArray $ identToJson <$> m.exports
    , reExports: fromArray $ reExportsToJson m.reExports
    , "foreign": fromArray $ identToJson <$> m."foreign"
    , decls: fromArray $ bindToJson <$> m.decls
    , builtWith: fromString $ showVersion v
    , comments: fromArray $ commentToJson <$> m.comments
    }
  where
  importToJson { annotation, moduleName } =
    fromObject $ fromHomogeneous
      { annotation: annToJson annotation
      , moduleName: moduleNameToJson moduleName
      }

  reExportsToJson e = toUnfoldable e <#> \(ModuleName n /\ identifiers) ->
    fromObject $ singleton n (fromArray $ identToJson <$> identifiers)
