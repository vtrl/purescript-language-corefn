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
import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (maybe)
import Data.String.CodeUnits as SCU
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Version.Haskell (Version, showVersion)
import Foreign.Object (Object, fromHomogeneous, singleton)
import Foreign.Object as Object
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

constructorTypeToJson ∷ ConstructorType → Json
constructorTypeToJson c = fromString $ case c of
  ProductType → "ProductType"
  SumType → "SumType"

metaToJson ∷ Meta → Json
metaToJson m = fromObject $ case m of
  IsConstructor c i →
    fromHomogeneous
      { metaType: fromString "IsConstructor"
      , constructorType: constructorTypeToJson c
      , identifiers: fromArray $ identToJson <$> i
      }
  IsNewtype →
    singleton "metaType" (fromString "IsNewtype")
  IsTypeClassConstructor →
    singleton "metaType" (fromString "IsTypeClassConstructor")
  IsForeign →
    singleton "metaType" (fromString "IsForeign")
  IsWhere →
    singleton "metaType" (fromString "IsWhere")

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
commentToJson c = fromObject $ case c of
  LineComment t → singleton "LineComment" (fromString t)
  BlockComment t → singleton "BlockComment" (fromString t)

literalToJson ∷ ∀ a. (a → Json) → Literal a → Json
literalToJson f l = fromObject $ case l of
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
    mkLiteral "ObjectLiteral" (fromArray $ mkObject o)
  where
  mkLiteral ∷ String → Json → Object Json
  mkLiteral literalType value =
    fromHomogeneous
      { literalType: fromString literalType
      , value
      }

  mkObject ∷ Object a → Array Json
  mkObject = foldrWithIndex (\k v → Array.cons (fromArray [ fromString k, f v ])) []

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
  fromArray $ fromString <$> split (Pattern ".") n

bindToJson ∷ Bind Ann → Json
bindToJson b = fromObject $ case b of
  NonRec a i e →
    fromHomogeneous
      { bindType: fromString "NonRec"
      , annotation: annToJson a
      , identifier: identToJson i
      , expression: exprToJson e
      }
  Rec xs →
    fromHomogeneous
      { bindType: fromString "Rec"
      , binds: fromArray $ xs <#> \{ identifier, annotation, expression } →
          fromObject $ fromHomogeneous
            { identifier: identToJson identifier
            , annotation: annToJson annotation
            , expression: exprToJson expression
            }
      }

exprToJson ∷ Expr Ann → Json
exprToJson v = fromObject $ case v of
  Var annotation value →
    fromHomogeneous
      { "type": fromString "Var"
      , annotation: annToJson annotation
      , value: qualifiedToJson (\(Ident n) → n) value
      }
  Literal annotation value →
    fromHomogeneous
      { "type": fromString "Literal"
      , annotation: annToJson annotation
      , value: literalToJson exprToJson value
      }
  Constructor annotation typeName constructorName fieldNames →
    fromHomogeneous
      { "type": fromString "Constructor"
      , annotation: annToJson annotation
      , typeName: properToJson typeName
      , constructorName: properToJson constructorName
      , fieldNames: fromArray $ identToJson <$> fieldNames
      }
  Accessor annotation fieldName expression →
    fromHomogeneous
      { "type": fromString "Accessor"
      , annotation: annToJson annotation
      , fieldName: fromString fieldName
      , expression: exprToJson expression
      }
  ObjectUpdate annotation expression updates →
    fromHomogeneous
      { "type": fromString "ObjectUpdate"
      , annotation: annToJson annotation
      , expression: exprToJson expression
      , updates: fromObject $ exprToJson <$> updates
      }
  Abs annotation argument body →
    fromHomogeneous
      { "type": fromString "Abs"
      , annotation: annToJson annotation
      , argument: identToJson argument
      , body: exprToJson body
      }
  App annotation abstraction argument →
    fromHomogeneous
      { "type": fromString "App"
      , annotation: annToJson annotation
      , abstraction: exprToJson abstraction
      , argument: exprToJson argument
      }
  Case annotation caseExpressions caseAlternatives →
    fromHomogeneous
      { "type": fromString "Case"
      , annotation: annToJson annotation
      , caseExpressions: fromArray $ exprToJson <$> caseExpressions
      , caseAlternatives: fromArray $ caseAlternativeToJson <$> caseAlternatives
      }
  Let annotation binds expression →
    fromHomogeneous
      { "type": fromString "Let"
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
    fromObject $ Object.insert name value $ fromHomogeneous
      { binders: fromArray $ binderToJson <$> binders
      , isGuarded
      }

binderToJson ∷ Binder Ann → Json
binderToJson b = fromObject $ case b of
  VarBinder annotation identifier →
    fromHomogeneous
      { binderType: fromString "VarBinder"
      , annotation: annToJson annotation
      , identifier: identToJson identifier
      }
  NullBinder annotation →
    fromHomogeneous
      { binderType: fromString "NullBinder"
      , annotation: annToJson annotation
      }
  LiteralBinder annotation literal →
    fromHomogeneous
      { binderType: fromString "LiteralBinder"
      , annotation: annToJson annotation
      , literal: literalToJson binderToJson literal
      }
  ConstructorBinder annotation typeName constructorName binders →
    fromHomogeneous
      { binderType: fromString "ConstructorBinder"
      , annotation: annToJson annotation
      , typeName: qualifiedToJson (\(Proper n) → n) typeName
      , constructorName: qualifiedToJson (\(Proper n) → n) constructorName
      , binders: fromArray $ binderToJson <$> binders
      }
  NamedBinder annotation identifier binder →
    fromHomogeneous
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
    , reExports: fromObject $ reExportsToJson m.reExports
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

  reExportsToJson ∷ Map ModuleName (Array Ident) → Object Json
  reExportsToJson = foldrWithIndex insert Object.empty
    where
    insert (ModuleName n) i = Object.insert n (fromArray $ identToJson <$> i)
