{-# LANGUAGE FlexibleContexts #-}
module Parser where

import ParserAST

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Foldable as Foldable
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec

languageDef = emptyDef {
                        Token.identStart = letter,
                        Token.identLetter = alphaNum,
                        Token.reservedOpNames = [],
                        Token.reservedNames = ["EXIT", "Int", "String", "CREATE TABLE", "INSERT INTO", "SELECT", "FROM", "JOIN", "ON", "WHERE", "REMOVE", "UPDATE", "SET", "LOAD", "UPLOAD", "IN", "FILE", "VALUES"]
                       }

lexer = Token.makeTokenParser languageDef -- строит парсер
identifier = Token.identifier lexer -- парсер переменных (колонок)
reserved = Token.reserved lexer -- парсер reservedNames
reservedOp = Token.reservedOp lexer -- парсер reservedOpNames
parens = Token.parens lexer -- парсер ( )
integer = Token.integer lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace = Token.whiteSpace lexer
symbol = Token.symbol lexer
commaSep = Token.commaSep lexer

mainParser :: Parser Commands -- пробелы вначале
mainParser = whiteSpace >> expression

expression :: Parser Commands
expression = buildExpressionParser [] command

command :: Parser Commands
command = do
   myCommand <- createParse <|> insertParse <|> updateParse <|> removeParse <|> loadParse <|> exitParse <|> uploadParse <|> showParse
   end <- many anyChar
   case end of
     "" -> return $ myCommand
     _  -> unexpected end

listColumnTypeParse :: Parser [ColumnType]
listColumnTypeParse =  commaSep columnTypeParser

listColumnNameParse :: Parser [ColumnName]
listColumnNameParse = commaSep parserString

listDataParse :: Parser [Data]
listDataParse = commaSep dataParser
-------------------------
columnTypeParser :: Parser ColumnType
columnTypeParser = typeIntParser <|> typeStringParser

typeIntParser :: Parser ColumnType
typeIntParser = do
  reserved "Int"
  body <- parserString
  return $ I body

typeStringParser :: Parser ColumnType
typeStringParser = do
  reserved "String"
  body <- parserString
  return $ S body
-----------------------
dataParser :: Parser Data
dataParser = Text.Parsec.try dataStringParser <|> dataIntParser

dataIntParser :: Parser Data
dataIntParser = do
  num <- integer
  return $ Int (fromIntegral num :: Int)

dataStringParser :: Parser Data
dataStringParser = do
  body <- between (string "'") (string "'") (many (anyLiteral <|> char ' '))
  return $ String body

createParse :: Parser Commands
createParse = do
           reserved "CREATE TABLE"
           tableName <- parserString
           list <- parens listColumnTypeParse
           return $ Create tableName list
------------------------

insertParse :: Parser Commands
insertParse = do
           reserved "INSERT INTO"
           tableName <- parserString
           list'  <- (Text.Parsec.try $ parens listColumnNameParse) <|> pure []
           reserved "VALUES"
           list'' <- parens listDataParse
           return $ Insert tableName (if list' == [] then Nothing else Just list') list''

------------------------

showParse :: Parser Commands
showParse = do
  res <- selectParse
  return $ Show res

selectParse :: Parser Select
selectParse = do
           reserved "SELECT"
           list'  <- (Text.Parsec.try $ parens listColumnNameParse) <|> (string "* " *> pure [])
           reserved "FROM"
           nestedCommand <- nestedCommandParse <|> (Left <$> parserString) -- Text.Parsec.try nestedCommandParse <|> (Left <$> parserString)
           predicate <- (Just <$> (Text.Parsec.try predicateParseData)) <|> pure Nothing
           return $ Select (if list' == [] then Nothing else Just list') nestedCommand predicate

predicateParseData :: Parser (Predicate Data)
predicateParseData = do
  reserved "WHERE"
  columnName <- parserString
  predicate <- (choice (Text.Parsec.try . string <$> ["==", "<=", ">=", "<>", "<", ">"])) <* space
  userData <- dataParser
  case predicate of
    "==" -> return $ Cons columnName userData Equal
    "<>" -> return $ Cons columnName userData NEqual
    "<=" -> return $ Cons columnName userData ELess
    ">=" -> return $ Cons columnName userData EBigger
    "<"  -> return $ Cons columnName userData Less
    ">"  -> return $ Cons columnName userData Bigger
    _    -> unexpected ""

predicateParseColumnName :: Parser (Predicate ColumnName)
predicateParseColumnName = do
    reserved "ON"
    columnName' <- parserString
    predicate <- (choice (Text.Parsec.try . string <$> ["==", "<=", ">=", "<>", "<", ">"])) <* space
    columnName'' <- parserString
    case predicate of
      "==" -> return $ Cons columnName' columnName'' Equal
      "<>" -> return $ Cons columnName' columnName'' NEqual
      "<=" -> return $ Cons columnName' columnName'' ELess
      ">=" -> return $ Cons columnName' columnName'' EBigger
      "<"  -> return $ Cons columnName' columnName'' Less
      ">"  -> return $ Cons columnName' columnName'' Bigger
      _    -> unexpected ""

nestedCommandParse :: Parser (Either TableName NestedCommand)
nestedCommandParse = Text.Parsec.try joinParser <|> Text.Parsec.try selectParser -- <|> tableNameParser

selectParser :: Parser (Either TableName NestedCommand)
selectParser = do
  res <- parens selectParse -- Text.Parsec.try (parens selectParse) <|> selectParse  -- <|> Text.Parsec.try selectParse
  return $ Right $ Left res

joinParser :: Parser (Either TableName NestedCommand)
joinParser = do
  res <- parens joinParse -- Text.Parsec.try (parens joinParse) <|> Text.Parsec.try joinParse
  return $ Right $ Right res

tableNameParser :: Parser (Either TableName NestedCommand)
tableNameParser = do
  res <- parserString
  return $ Left res

joinParse :: Parser Join
joinParse = do
  nestedCommand  <- nestedCommandParse <|> (Left <$> parserString)
  reserved "JOIN"
  nestedCommand' <- nestedCommandParse <|> (Left <$> parserString)
  predicate <- (Just <$> (Text.Parsec.try predicateParseColumnName)) <|> pure Nothing
  return $ Join nestedCommand nestedCommand' predicate
------------------------
updateParse :: Parser Commands
updateParse = do
    reserved "UPDATE"
    tableName <- parserString
    reserved "SET"
    list  <- parens listUpdate
    predicate <- (Just <$> (Text.Parsec.try predicateParseData)) <|> pure Nothing
    return $ Update tableName list predicate

listUpdate :: Parser [(ColumnName, Data)]
listUpdate = commaSep listColumnNameDataParser

listColumnNameDataParser :: Parser (ColumnName, Data)
listColumnNameDataParser = do
  name <- parserString
  string "="
  userData <- dataParser
  return (name, userData)
------------------------
removeParse :: Parser Commands
removeParse = do
  reserved "REMOVE"
  reserved "FROM"
  tableName <- parserString
  predicate <- (Just <$> (Text.Parsec.try predicateParseData)) <|> pure Nothing
  return $ Remove tableName predicate
------------------------
loadParse :: Parser Commands
loadParse = do
  reserved "LOAD"
  list  <- (Text.Parsec.try $ parens listTableNameParse) <|> (string "* " *> pure [])
  reserved "IN"
  reserved "FILE"
  fileName <- between (string "'") (string "'") (many anyLiteral)
  return $ Load (if list == [] then Nothing else Just list) fileName

listTableNameParse :: Parser [TableName]
listTableNameParse = commaSep parserString
------------------------
uploadParse :: Parser Commands
uploadParse = do
  reserved "UPLOAD"
  reserved "FILE"
  fileName <- between (string "'") (string "'") (many anyLiteral)
  return $ Upload fileName
------------------------
exitParse :: Parser Commands
exitParse = do
  reserved "EXIT"
  return Exit
------------------------
parserString :: Parser String
parserString = pure (++) <*> (many1 anyLiteral) <*> (Text.Parsec.try (string " " *> pure []) <|> pure [])

anyLiteral :: Stream s m Char => ParsecT s u m Char
anyLiteral = oneOf ".1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_"

parser :: String -> Either String Commands
parser str = case parse mainParser "" str of
                    Left e  -> Left $ show e
                    Right r -> Right r
