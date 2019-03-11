module TableAST where

import ParserAST

data Table = Table {getName :: TableName, getData :: [(ColumnType, DataTable)]} deriving (Show,Read)

type CommonTable = [Table]
type DataTable = [Maybe (Data)]

instance Eq Table where
   a  == b = getName a == getName b

instance Eq ColumnType where
  S a  == S b = a == b
  I a  == I b = a == b
  _    == _   = False

instance Eq Data where
  String str1 == String str2 = str1 == str2
  Int int1 == Int int2 = int1 == int2
  _ == _ = False

instance Ord Data where
  String str1 <= String str2 = str1 <= str2
  Int int1 <= Int int2 = int1 <= int2
  _ <= _ = False
