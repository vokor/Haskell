module ParserAST where

data Data = String String | Int Int
  deriving (Show,Read)
data ColumnType = S ColumnName | I ColumnName
   deriving (Show,Read)

data Predicate a = Cons ColumnName a Cmp deriving (Show)
data Cmp = NEqual | Equal | ELess | EBigger | Less | Bigger deriving (Show)

type TableName = String
type ColumnName = String
type FileName = String
type NestedCommand = Either Select Join

data Select = Select (Maybe [ColumnName]) (Either TableName NestedCommand) (Maybe (Predicate Data)) deriving Show
data Join = Join (Either TableName NestedCommand) (Either TableName NestedCommand) (Maybe (Predicate ColumnName)) deriving Show

data Commands =
    Exit
  | Create TableName [ColumnType]
  | Insert TableName (Maybe [ColumnName]) [Data]
  | Update TableName [(ColumnName, Data)] (Maybe (Predicate Data))
  | Remove TableName (Maybe (Predicate Data))
  | Show Select
  | Load (Maybe [TableName]) FileName
  | Upload FileName
  deriving Show
