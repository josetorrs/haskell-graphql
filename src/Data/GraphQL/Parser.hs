{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
module Data.GraphQL.Parser
  ( parseDocument
  ) where

import Control.Arrow (left)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Text

import Data.GraphQL.Document

import qualified Data.Text as T
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map.Lazy as Map

-- lexer functions
void :: Parser a -> Parser ()
void p = p >> return ()

whitespace :: Parser ()
whitespace = void (spaceChar <|> tab)

terminators :: Parser ()
terminators = void eol

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (whitespace <|> terminators) lineComment lineComment

symbol :: String -> Parser String
symbol = L.symbol scn

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
comma = symbol ","

-- tokens
underscore :: Parser Char
underscore = char '_'

nameStart :: Parser Char
nameStart = asciiChar <|> underscore

nameLetter :: Parser Char
nameLetter = asciiChar <|> digitChar <|> underscore

name :: Parser String
name = label "name" $ L.lexeme scn $ (:) <$> nameStart <*> (some nameLetter)

signed :: Num a => Parser a -> Parser a
signed p = ($) <$> option id (char '-' *> return negate) <*> p

intValue :: Parser Integer
intValue = label "int" $ L.lexeme scn $ signed L.integer

floatValue :: Parser Double
floatValue = label "float" $ L.lexeme scn $ signed L.float

escapedUnicode :: Parser String
escapedUnicode = (++) <$> string "\\u" <*> count 4 (oneOf "ABCDEFabcdef" <|> digitChar)

escapedChararacter :: Parser String
escapedChararacter = (:) <$> char '\\' <*> (pure <$> oneOf "\"\\/bfnrt")

stringChar :: Parser String
stringChar = (pure <$> noneOf "\"\n\r") <|> escapedUnicode <|> escapedChararacter

stringValue :: Parser String
stringValue = label "string" $ L.lexeme scn $ concat <$> (char '"' *> manyTill stringChar (char '"'))

boolValue :: Parser Bool
boolValue =
      (symbol "true" *> return True)
  <|> (symbol "false" *> return False)
  <?> "bool value"

-- utilities
list = many

-- documents
document :: Parser Document
document = label "document" $ do
  scn
  defs <- list definition
  eof
  return $ Document defs

definition :: Parser Definition
definition = try operation <|> try fragment <|> (Selection <$> selectionSet) <?> "definition"

-- operations
operation :: Parser Definition
operation = label "operation" $ do
  t <- operationType
  n <- nameOpt
  vdefs <- option [] variableDefinitions
  dirs <- directives
  set <- selectionSet
  return $ Operation t n vdefs dirs set

-- operation type
operationType :: Parser OperationType
operationType =
      (symbol "query" >> return Query)
  <|> (symbol "mutation" >> return Mutation)
  <?> "operation type"

-- optional name
nameOpt :: Parser (Maybe String)
nameOpt = option Nothing (Just <$> name)

-- variable definitions
variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = parens (list variableDefinition) <?> "variable definitions"

variableDefinition :: Parser VariableDefinition
variableDefinition = label "variable definition" $ do
  v <- variable
  symbol ":"
  t <- qlType
  value <- option Nothing (Just <$> value)
  return $ VariableDefinition v t value

variable :: Parser String
variable = symbol "$" *> name <?> "variable"

-- types
qlType :: Parser Type
qlType = namedType <|> listType <|> nonNullType <?> "type"

namedType :: Parser Type
namedType = NamedType <$> name <?> "named type"

listType :: Parser Type
listType = ListType <$> brackets qlType <?> "list type"

nonNullType :: Parser Type
nonNullType = NonNullType <$> (symbol "!" *> qlType) <?> "non null type"

-- values
value :: Parser Value
value = (Variable <$> variable)
  <|> (IntValue <$> intValue)
  <|> (FloatValue <$> floatValue)
  <|> (StringValue <$> stringValue)
  <|> (BooleanValue <$> boolValue)
  <|> (EnumValue <$> enumValue)
  <|> (ListValue <$> many value)
  <|> (ObjectValue <$> objectValue)
  <?> "value"

enumValue :: Parser String
enumValue = label "enum" $ name >>= check
  where
    check n = if n `elem` ["true", "false", "null"]
                then fail $ "reserved name " ++ n ++ " cannot be an enum"
                else return n

objectValue :: Parser (Map.Map String Value)
objectValue = Map.fromList <$> braces (list objectField) <?> "object value"

objectField :: Parser (String,Value)
objectField = (,) <$> (name <* symbol ":") <*> value <?> "object field"

-- directives
directives :: Parser [Directive]
directives = list directive <?> "directives"

directive :: Parser Directive
directive = Directive <$> (symbol "@" *> name) <*> arguments <?> "directive"

arguments :: Parser [Argument]
arguments = label "arguments" $ option [] $ parens (list argument)

argument :: Parser Argument
argument = label "argument" $ Argument <$> (name <* symbol ":") <*> value

-- selections
selectionSet :: Parser SelectionSet
selectionSet = label "selection set" $ SelectionSet <$> braces (list selection)

selection :: Parser Selection
selection = try field <|> try fragmentSpread <|> try inlineFragment <?> "selection"

field :: Parser Selection
field = label "field" $ do
  alias <- option Nothing (Just <$> name <* symbol ":")
  n <- name
  args <- arguments
  dirs <- directives
  set <- option (Nothing) (Just <$> selectionSet)
  return $ Field alias n args dirs set

fragmentSpread :: Parser Selection
fragmentSpread = label "fragment spread" $ do
  symbol "..."
  n <- fragmentName
  ds <- directives
  return $ FragmentSpread n ds

inlineFragment :: Parser Selection
inlineFragment = label "inline fragment" $ do
  symbol "..."
  symbol "on"
  namedType <- option Nothing (Just <$> name)
  dirs <- directives
  set <- selectionSet
  return $ InlineFragment namedType dirs set

-- fragments
fragment :: Parser Definition
fragment = label "fragment" $ do
  symbol "fragment"
  n <- fragmentName
  cond <- name
  dirs <- directives
  set <- selectionSet
  return $ Fragment n cond dirs set

fragmentName :: Parser String
fragmentName = label "fragment name" $ name >>= check
  where
    check n = if n == "on"
                then fail $ "reserved name " ++ n ++ " cannot be a fragment name"
                else return n

-- final function
parseDocument :: String -> T.Text -> Either String Document
parseDocument name query = left parseErrorPretty $ parse document name query
