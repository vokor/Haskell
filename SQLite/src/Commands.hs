module Commands where

import Data.List
import Data.Char
import System.Environment
--import Control.Monad.State
import Control.Exception

--Таблица хранится в виде: [Left "name_table", Right 2, ">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
executeCreate:: [Either String Int] -> [Either String Int]
executeCreate (x:xs) = x:(Right $ length xs):xs

-------------------------------------
executeUpload :: [[Either String Int]] -> String -> IO (String, [[Either String Int]])
executeUpload table fileName = do
   contents <- (Control.Exception.try (readFile fileName)) :: IO (Either SomeException String)
   let
      inf = case contents of
         Left _  -> "Error"
         Right a -> a
      (logRes, tableRes) = case inf of
        "Error" -> (">Error: File \'" ++ fileName ++ "\' not found", [])
        _       -> case maybeRead inf of
          Just a    -> checkTable a table
          _         -> (">Error: File contents must be of type [[Either String Int]]", [])
   return (logRes, table ++ tableRes)

maybeRead :: String -> Maybe [[Either String Int]]
maybeRead s = case reads s of
    [(x, "")] -> Just (x :: [[Either String Int]])
    _         -> Nothing


checkTable::[[Either String Int]] -> [[Either String Int]] -> (String, [[Either String Int]])
checkTable table table_In = if any (== 0) (f <$> table) then
       (">Error: Wrong format", [])
    else
      if (and $ (fmap (checkTableNames $ table_In) table) ++
        (fmap (\(x:(Right y):z) -> checkColumnNames (take y z) (take y z)) table)) then
         (">Done", table)
      else
         (">Error: Table or column naming error", [])
  where
    f []          = 0
    f (x:[])      = 0
    f (x:y:[])    = 0
    f (x:y:table') = case y of
      Right a -> if (not $ checkName x || a == 0) then 0 else
        if (any (== -1) $ checkBase a $ take a table') then
          0
        else
          g (checkBase a $ take a table') (drop a table')
      _       -> 0

    -- проверяет первые имена колонок, идентифицирует типы
    checkBase:: Int -> [Either String Int] -> [Int]
    checkBase k line | length line == k = h <$> line
                     | otherwise        = [-1]
          where
            h (Left str) = case parseChar ':' str of
              ("",_)         -> -1
              (">String", b) -> if (checkName $ Left b) then 0 else -1
              (">Int", b)    -> if (checkName $ Left b) then 1 else -1
              _              -> -1
            h _          = -1

   -- проверяет строки таблицы на соответствие по типу, правила именования
    g _ []       = 1
    g types table' = if (
      sum $ (\(a,b) -> case b of
              (Left str) -> if (a == 0) then 1 else 0                   --(Left str) -> if (a == 0 && (not $ any (\x -> x == '(' || x == ')') str)) then 1 else 0
              _          -> if (a == 1) then 1 else 0
            ) <$> (zip types (take len table'))
        ) == len then g types (drop len table') else 0
      where
        len = length types

   --проверяет правило именования
    checkName (Left "") = False
    checkName (Left x)  = all (\x -> (isDigit x || isLetter x || x == '_')) x
    checkName _         = False

   --проверяет именование колонок и имен таблиц (на различие)
    checkTableNames::[[Either String Int]] -> [Either String Int] -> Bool
    checkTableNames _ [] = True
    checkTableNames a (b:_) = if sum (fmap (\x -> if (head x == b) then 1 else 0) a) == 0 then True else False

    checkColumnNames::[Either String Int] -> [Either String Int] -> Bool
    checkColumnNames [] list = True
    checkColumnNames (n:m) (y:list) = (not $ any (== (getName n)) (getName <$> list)) && checkColumnNames m (list ++ [y])
----------------------------------------------------------------------
executeLoad::String -> [[Either String Int]] -> IO String
executeLoad _ []                 = return ">Done"
executeLoad fileName table@(a:_) = case a of
  ((Left str):[]) -> return str
  _               -> do
    a <- writeFile fileName (show table)
    return ">Done"



-- Таблицы имеют вид [Right 2, ">String:name_table.FirstPole", ">Int:name_table.SecondPole", (данные таблицы)]
-- предикат в виде ("A.key1", "=", "B.key2")
-- если результат с обшибкой, то возвращается только ошибка
executeInnerJoin::[Either String Int] -> [Either String Int] -> (String, String, String) -> [Either String Int]
executeInnerJoin _ [] _ = []
executeInnerJoin [] _ _ = []
executeInnerJoin (e:table1) (o:table2) (name1, predicate, name2) = let
  tableLen' = getNum e   -- количество столбцов первой таблицы
  tableLen'' = getNum o  -- количество столбцов второй таблицы

  -- номера имен в предикате
  (pos1, pos2) = case tryFind (take tableLen' table1) name1 0 of
    -1 -> (tryFind (take tableLen' table1) name2 0, tryFind (take tableLen'' table2) name1 0)
    r  -> (r, tryFind (take tableLen'' table2) name2 0)

  -- ищет в таблице имя
  -- передается [">String:name_table.FirstPole", ">Int:name_table.SecondPole"] и "A.key1" и 0
  tryFind::[Either String Int] -> String -> Int -> Int
  tryFind [] _ _              = -1
  tryFind (x:tableF) valueF t = if (getName x == valueF) then t else tryFind tableF valueF (t + 1)

  flH (Left a) = a
  flag = value (flH (table1 !! pos1)) /= value (flH (table2 !! pos2))

  newTable = if (pos1 == -1 || pos2 == -1) then
         []
      else
        (Right (tableLen' + tableLen'')):(take tableLen' table1) ++ (take tableLen'' table2) ++ tryInnerJoin (drop tableLen' table1) (drop tableLen'' table2) flag

  -- передаются две таблицы в троки, без имен столбцов
  tryInnerJoin::[Either String Int] -> [Either String Int] -> Bool -> [Either String Int]
  tryInnerJoin [] _ _= []
  tryInnerJoin a b f = innerJoinHelper (take tableLen' a) b f ++ tryInnerJoin (drop tableLen' a) b f

  innerJoinHelper::[Either String Int] -> [Either String Int] -> Bool -> [Either String Int]
  innerJoinHelper _ [] _= []
  innerJoinHelper a b f = let
    row = take tableLen'' b
    res = if (f || checkPredicate (a !! pos1) (row !! pos2) (Left predicate)) then
      a ++ row ++ innerJoinHelper a (drop tableLen'' b) f
    else
      innerJoinHelper a (drop tableLen'' b) f
    in res

  in newTable

{-
команда, таблица
-}

executeSelect:: [Either String Int] -> [[Either String Int]] -> String
executeSelect ((Right a):xs) tableMain@(st:_) = logErr where
  (num,table) = (st !! 1, drop 2 st)
  logErr = case a of
    2 -> let
      count = getNum(head xs)
      columnsToOut = take count (tail xs)
      tree = drop count (tail xs)

      -- ищет таблицу по имени, модифицирует столбцы (склеивает имя таблицы и имя столбца)
      getAndUpdateTable::String -> [Either String Int]
      getAndUpdateTable name = case find (\((Left x):xs) -> x == name) tableMain of
        Just (name':(Right t):table') ->
          (Right t):
          ((\(Left inp) -> Left ((takeWhile (/= ':') inp) ++ ":" ++ name ++ "." ++ tail (dropWhile (/= ':') inp))) <$> (take t table')) ++
          (drop t table')

      -- основная функция, сворачивающая дерево
      doWorkWithTree::[Either String Int] -> [Either String Int]
      doWorkWithTree ((Left name):[]) = getAndUpdateTable name
      doWorkWithTree ((Left name1):(Left predicate):(Left name2):(Right len):table') =
        executeInnerJoin (doWorkWithTree (take len table')) (doWorkWithTree (drop len table')) (name1, predicate, name2)

      toOut::String
      toOut = case doWorkWithTree tree of
        []               -> ">Error: Table creation completed with error"
        resTable@(t:res) -> case count of
          0 -> trySelect (getNum t) (drop (getNum t) res)
          _ -> if (any (== -1) (steps resTable columnsToOut)) then
                  ">Error: Invalid column name entered"
               else
                 h (getNum t) (drop (getNum t) res) (steps resTable columnsToOut)

      -- передается полная таблица, выделяются строки и переводятся в String
      h::Int -> [Either String Int] -> [Int]-> String
      h _ [] _      = ""
      h t table1 it = (g (take t table1) it) ++ "\n" ++ (h t (drop t table1) it)

      in toOut

    _ -> if (length xs == 0) then
         trySelect (getNum num) (drop (getNum num) table)
      else
         let
           --позиция названия предиката в общем списке названий столбцов
           pos = if (a == 1) then checkValue (getNum $ num) table else 0

           (name, predicate, value1) = if (a == 1) then (xs !! 0, xs !! 1, xs !! 2) else (Left "", Left "", Left "")

           checkValue::Int -> [Either String Int] -> Int
           checkValue 0 _ = -1
           checkValue n ((Left x):xss) = if (name /= Left (getName (Left x))) then checkValue (n - 1) xss else
             case value1 of
               Right _ -> if value x then ((getNum $ num) - n) else -1
               _       -> if value x then -1 else ((getNum $ num) - n)

           -- передается полная таблица, выделяются строки и переводятся в String
           h::[Either String Int] -> [Int]-> String
           h [] _      = ""
           h table1 it = if (pos == -1) then
               ">Error: Invalid predicate"
             else
               case (a == 0 || checkPredicate (table1 !! pos) value1 predicate) of
                  True -> (g (take (getNum num) table1) it) ++ "\n" ++ (h (drop (getNum num) table1) it)
                  False -> h (drop (getNum num) table1) it

           myLog = case a of
             0 -> if (any (== -1) (steps (tail st) xs)) then ">Error: Invalid column name entered" else h (drop (getNum num) table) (steps (tail st) xs)
             -- [Left "Age",Left ">",Right 18,Left "FirstName",Left "LastName"]
             1 -> if (any (== -1) (steps (tail st) $ drop 3 xs)) then ">Error: Invalid column name entered" else h (drop (getNum num) table) (steps (tail st) (drop 3 xs))


           in myLog

  -- выдает номер (с нуля) имен столбцов, введенных пользователем
  f::Int -> [Either String Int] -> Either String Int -> Int
  f _ [] _         = -1
  f t (y:table1) (Left x) = if (x == getName y) then (t - length table1 - 1) else f t table1 (Left x)
  -- список индексов искомых столбцов в общем списке таблицы столбцов. Передавать только имена, упакованные в Left
  steps::[Either String Int] -> [Either String Int] -> [Int]
  steps (n:tableFind) columns = fmap (f (getNum n) $ (take (getNum n) tableFind)) columns

  -- Передается полная строка таблицы, выдается ее строковое представление только тех столцов, номера индексов которых указаны
  -- -1 быть не должно!!!
  g::[Either String Int] -> [Int] -> String
  g _ []       = "|"
  g columns (i:it) = case (columns !! i) of
      (Left z)  -> (if z == ">undefined<" then "|" else ("|" ++ z)) ++ (g columns it)
      (Right z) -> (if z == (minBound ::Int) then "|" else ("|" ++ show z)) ++ (g columns it)

  trySelect::Int -> [Either String Int] -> String
  trySelect _ [] = ""
  trySelect n table1 = (trySelect' (take n table1)) ++ "\n" ++ trySelect n (drop n table1)

  trySelect':: [Either String Int] -> String
  trySelect' [] = "|"
  trySelect' (x:xs) = case x of
    (Left y)  -> (if y == ">undefined<" then "|" else ("|" ++ y)) ++ trySelect' xs
    (Right y) -> (if y == (minBound ::Int) then "|" else ("|" ++ show y)) ++ trySelect' xs


{-
первый аргумент - что, второй - куда

[Right 0,Left "Age",Left ">",Right 18]

newTable1
--количество не проверенных стролбцов в таблице
[">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
--количество не исправленных столбцов
[Right 0,Left "Age",Left ">",Right 18]

-}

executeRemove:: [Either String Int] -> [Either String Int] -> (String, [Either String Int])
executeRemove ((Right a):xs) table = checkCommand myRes myRes table where
    --позиция названия предиката в общем списке названий столбцов
     pos = if (a == 0) then checkValue (getNum $ head table) (tail table) else 0
     u = if (a == 0) then 3 else 0

     myRes = if (pos /= -1) then
            take (1 + (getNum $ head table)) table ++ if (a == 0) then newTable1 (drop (getNum $ head table) (tail table)) else []
       else
         [(Left ">Error: Invalid predicate")]
     (name, predicate, value1) = if (a == 0) then (xs !! 0, xs !! 1, xs !! 2) else (Left "", Left "", Left "")

     newTable1::[Either String Int] -> [Either String Int]
     newTable1 []  = []
     newTable1 xss = (tryRemove (take (getNum $ head table) xss)) ++ (newTable1 $ drop (getNum $ head table) xss)

     tryRemove::[Either String Int] -> [Either String Int]
     tryRemove table1 = if checkPredicate (table1 !! pos) value1 predicate then
         []
        else
          table1
     -- поиск столбца по имени
     tryFind:: String -> [Either String Int] -> Int -> Int
     tryFind _ [] _         = -1
     tryFind n (y:ys:yss) k = if ((Left $ getName (Left n)) == y) then (k + 1) else tryFind n yss (k + 2)

     checkValue::Int -> [Either String Int] -> Int
     checkValue 0 _ = -1
     checkValue n ((Left x):xss) = if (name /= Left (getName (Left x))) then checkValue (n - 1) xss else
       case value1 of
         Right _ -> if value x then ((getNum $ head table) - n) else -1
         _       -> if value x then -1 else ((getNum $ head table) - n)




--первый аргумент - что, второй - куда
{-
[Right 1,Left "Units",Left "148",Left "Money",Right 23680]

[Right 0,Left "ID",Left "=",Right 4, Left "Units",Right 148,Left "Money",Right 23680]

UPDATE table_name SET (Age=148) WHERE Name = 'Vova'

[Right 2, Left ">String:Name", Left ">Int:Age", Left "Vova", Right 18, Left "Vova", Right 19, Left "Andew", Right 20]
[Right 0,Left "Name",Left "=",Left "Vova",Left "Age",Right 148]

[Right 2, ">String:FirstPole", ">Int:SecondPole", (данные таблицы)]

newTable1
--количество не проверенных стролбцов в таблице
[">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
--количество не исправленных столбцов
[Left "Units",Right 148,Left "Money",Right 23680]

-}


executeUpdate:: [Either String Int] -> [Either String Int] -> (String, [Either String Int])
executeUpdate ((Right a):xs) table = checkCommand myRes myRes table where
    --позиция названия предиката в общем списке названий столбцов
     pos = if (a == 0) then checkValue (getNum $ head table) (tail table) else 0
     u = if (a == 0) then 3 else 0

     myRes = if (pos /= -1) then
         {-if (length table == 1 + (getNum $ head table)) then table
           else -}
            (head table):newTable1 (getNum $ head table) (tail table) (length $ drop u xs) (drop u xs)
       else
         [(Left ">Error: Invalid predicate")]
     (name, predicate, value1) = if (a == 0) then (xs !! 0, xs !! 1, xs !! 2) else (Left "", Left "", Left "")

     newTable1::Int -> [Either String Int] -> Int -> [Either String Int] ->[Either String Int]
     newTable1 n res 0 _              = res
     newTable1 0 res n _              = if (n /= 0) then [(Left ">Error: Сolumn with the specified name not found or some names match")] else res
     newTable1 n ((Left x):xss) m y   = case tryFind x y 0 of
       -1 -> (Left x):newTable1 (n - 1) xss m y
       it -> case value x of
         True  -> case (y !! it) of
           Right a -> (Left x):newTable1 (n - 1) ((take (n - 1) xss) ++ tryUpdate (drop (n - 1) xss) ((getNum $ head table) - n) (y !! it) 0) (m - 2) y
           _       -> [(Left ">Error: Type mismatch")]
         False -> case (y !! it) of
           Left a  -> (Left x):newTable1 (n - 1) ((take (n - 1) xss) ++ tryUpdate (drop (n - 1) xss) ((getNum $ head table) - n) (y !! it) 0) (m - 2) y
           _       -> [(Left ">Error: Type mismatch")]

     tryUpdate::[Either String Int] -> Int -> Either String Int -> Int -> [Either String Int]
     tryUpdate [] n _ _ = []
     tryUpdate table1 step replacement itt = if (a == 1) || checkPredicate ((drop (1 + (getNum $ head table)) table) !! (itt * (getNum $ head table) + pos)) value1 predicate then
         (take step table1) ++ [replacement] ++ (drop (step + 1) (take (getNum $ head table) table1)) ++ tryUpdate (drop (getNum $ head table) table1) step replacement (itt + 1)
        else
          (take (getNum $ head table) table1) ++ tryUpdate (drop (getNum $ head table) table1) step replacement (itt + 1)
     -- поиск столбца по имени
     tryFind:: String -> [Either String Int] -> Int -> Int
     tryFind _ [] _         = -1
     tryFind n (y:ys:yss) k = if ((Left $ getName (Left n)) == y) then (k + 1) else tryFind n yss (k + 2)

     checkValue::Int -> [Either String Int] -> Int
     checkValue 0 _ = -1
     checkValue n ((Left x):xss) = if (name /= Left (getName (Left x))) then checkValue (n - 1) xss else
       case value1 of
         Right _ -> if value x then ((getNum $ head table) - n) else -1
         _       -> if value x then -1 else ((getNum $ head table) - n)

{-
первый аргумент - что, второй - куда

INSERT INTO table_name (column1,column2,column3) VALUES ('data1','data2','data3')
[Right 0, > Right 3,Left "column1",Left "column2",Left "column3",Left "data1",Left "data2",Left "data3"]

newTable1:
- количество оставшихся полей в table
[">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
- количество не введенных полей
[Left "column1",Left "column2",Left "column3",Left "data1",Left "data2",Left "data3"]

INSERT INTO table_name VALUES (13,'data1','data2',22,4620)
[Right 1, Right 13,Left "data1",Left "data2",Right 22,Right 4620]

--Таблица хранится в виде: [Right 2, ">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
-}

executeInsert:: [Either String Int] -> [Either String Int] -> (String, [Either String Int])
executeInsert ((Right a):xs) table = case a of
  0 -> if ((getNum $ head xs) > (getNum $ head table)) then (">Error: Data mapping error", table)
    else checkCommand myRes myRes table
    where        --
      myRes = (head table):newTable1 (getNum $ head table) (tail table) (getNum $ head xs) (tail xs)

      newTable1::Int -> [Either String Int] -> Int -> [Either String Int] ->[Either String Int]
      newTable1 s (x:xss) 0 y      = if (s /= 0) then (pos x y 0):(newTable1 (s - 1) xss 0 y) else []
      newTable1 0 _ m table2      = if (m /= 0) then [(Left ">Error: Сolumn with the specified name not found or some names match")] else []
      newTable1 n (x:xss) m y     = (if (n == (getNum $ head table)) then x:xss else [] )++ [pos x y (getNum $ head xs)] ++ if (checkRes (pos x y (getNum $ head xs))) then
        newTable1 (n - 1) xss (m - 1) y else
          newTable1 (n - 1) xss m y

      checkRes (Left a) = (a /= ">undefined<") && (a /= ">Error: Type mismatch")
      checkRes (Right a) = a > (minBound :: Int)

      pos:: Either String Int -> [Either String Int] -> Int -> Either String Int
      pos _ [] _             = Left ">Error: Incorrect input"
      pos (Left str) _ 0     = if (value str) then Right (minBound :: Int) else Left ">undefined<"
      pos (Left n) (x:xss) t = if (x /= Left (getName $ Left n)) then pos (Left n) xss (t - 1) else
        if (value n) then
          case xss !! ((length xss) - t) of
            Left _  -> Left ">Error: Type mismatch"
            Right _ -> xss !! ((length xss) - t)
        else
          case xss !! ((length xss) - t) of
            Left _  -> xss !! ((length xss) - t)
            Right _ -> Left ">Error: Type mismatch"
--Таблица хранится в виде: [Right 2, ">String:FirstPole", ">Int:SecondPole", (данные таблицы)]
  1 -> if ((length xs) /= (getNum $ head table)) then (">Error: Data mapping error", table)
    else checkCommand myRes myRes table where
    myRes = (head table):newTable1 (getNum $ head table) (tail table) xs

    newTable1::Int -> [Either String Int] -> [Either String Int] -> [Either String Int]
    newTable1 0 _ _       = []
    newTable1 n (x:xss) (y:ys) = (if (n == (getNum $ head table)) then x:xss else [] ) ++ [(checkRes x y)] ++ newTable1 (n - 1) xss ys

    checkRes::Either String Int -> Either String Int -> Either String Int
    checkRes (Left x) y = case y of
      Left _  -> if (value x) then (Left ">Error: Type mismatch") else y
      Right _ -> if (value x) then y else (Left ">Error: Type mismatch")

--вытаскивает имя
getName (Left a) = tail $ dropWhile (/= ':') a
getNum (Right a) = a

value:: String -> Bool
value str = (take 5 str) == ">Int:"

parseChar:: Char -> String -> (String, String)
parseChar p s = let (a,b) = span (/= p) s in (a, if (b == "") then b else tail b)

checkCommand::[Either String Int] -> [Either String Int] -> [Either String Int] -> (String, [Either String Int])
checkCommand [] table tableOrg = (">Done", table)
checkCommand (x:xs) table tableOrg = case helper x of
  True -> checkCommand xs table tableOrg
  False -> (">Error:" ++ getName x, tableOrg)

--tester::Either String Int -> Either String Int -> Int -> Bool
--tester a b c = (constructor a b) c


checkPredicate::Either String Int -> Either String Int -> Either String Int -> Bool
checkPredicate (Left a) (Left b) (Left predicate) = if (a == ">undefined<") then False else case predicate of
  ">"  -> a > b
  "<"  -> a < b
  "="  -> a == b
  "<>" -> a /= b
  ">=" -> a >= b
  "<=" -> a <= b
checkPredicate (Right a) (Right b) (Left predicate) = if (a == (minBound :: Int)) then False else case predicate of
  ">"  -> a > b
  "<"  -> a < b
  "="  -> a == b
  "<>" -> a /= b
  ">=" -> a >= b
  "<=" -> a <= b

--проверяет элемент на ошибку
helper:: Either String Int -> Bool
helper a = case a of
  Left str -> not $ isInfixOf ">Error:" str
  _        -> True
