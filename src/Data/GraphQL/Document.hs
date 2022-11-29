module Data.GraphQL.Document where

import qualified Data.Map.Lazy as Map

data Document = Document [Definition]
  deriving (Show,Eq)

type Name = String

data Definition =
    Operation OperationType (Maybe Name) [VariableDefinition] [Directive] SelectionSet
  | Selection SelectionSet
  | Fragment Name String [Directive] SelectionSet
  deriving (Show,Eq)

data OperationType =
    Query
  | Mutation
  deriving (Show,Eq)

type DefaultValue = Maybe Value

data VariableDefinition = VariableDefinition String Type DefaultValue
  deriving (Show,Eq)

data Type =
    NamedType String
  | ListType Type
  | NonNullType Type
  deriving (Show,Eq)

data Value =
    Variable String
  | IntValue Integer
  | FloatValue Double
  | StringValue String
  | BooleanValue Bool
  | EnumValue String
  | ListValue [Value]
  | ObjectValue (Map.Map String Value)
  deriving (Show,Eq)

data Directive = Directive String [Argument]
  deriving (Show,Eq)

data Argument = Argument String Value
  deriving (Show,Eq)

data SelectionSet = SelectionSet [Selection]
  deriving (Show,Eq)

data Selection =
    Field (Maybe Name) Name [Argument] [Directive] (Maybe SelectionSet)
  | FragmentSpread Name [Directive]
  | InlineFragment (Maybe String) [Directive] SelectionSet
  deriving (Show,Eq)
