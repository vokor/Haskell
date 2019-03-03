module Parser where

import Text.Parsec
import Text.Parsec.Prim
import Data.Char
import Data.Functor.Identity
import Text.Read

parser:: String -> [Either String Int]
parser input = if not (checkInput input) then [] else
  case parse parser' "" input of
    Left err  -> []
    Right xs  -> case typeCommand xs of
                  0 -> if (length xs == 4 && (xs !! 1) == "TABLE" && checkName (xs !! 2) == True)
                         then (parseCreate ((xs !! 2),(xs !! 3)))
                         else []
                  1 -> if (length xs == 6 && (xs !! 1) == "INTO" && checkName (xs !! 2) == True && (xs !! 4) == "VALUES")
                    then
                      parseInsert((xs !! 2), (xs !! 3), (xs !! 5))
                    else
                      if (length xs == 5 && (xs !! 1) == "INTO" && checkName (xs !! 2) == True && (xs !! 3) == "VALUES")
                        then
                          parseInsert((xs !! 2), "undefined", (xs !! 4))
                        else []
                  2 -> if (length xs == 8 && (xs !! 2) == "SET" && (xs !! 3) /= "" && (xs !! 4) == "WHERE" && (xs !! 5) /= "")
                    then
                      parseUpdate(xs !! 1, xs !! 3, xs !! 5, xs !! 6, xs !! 7)
                    else
                      if (length xs == 4 && (xs !! 2) == "SET" && (xs !! 3) /= "")
                        then
                          parseUpdate(xs !! 1, xs !! 3, "undefined", "undefined", "undefined")
                        else
                          []
                  3 -> if (length xs == 7 && (xs !! 1) == "FROM" && (xs !! 3) == "WHERE")
                    then
                      parseRemove(xs !! 2, xs !! 4, xs !! 5, xs !! 6)
                    else
                      if (length xs == 3 && (xs !! 1) == "FROM")
                        then
                          parseRemove(xs !! 2, "undefined", "undefined", "undefined")
                        else
                          []
                  4 -> if (length xs == 4 && (xs !! 2) == "FROM") then
                     if ((xs !! 1) == "*") then
                       parseSelect [(xs !! 3)]
                     else
                       parseSelect[(xs !! 3), (xs !! 1)]
                   else
                     if any (== "JOIN") xs then
                       parseSelect("1":(tail xs))
                     else
                       parseSelect("0":(tail xs))
                  5 -> if (length xs == 5 && (xs !! 2) == "IN" && (xs !! 3) == "FILE")
                    then
                       if ((xs !! 1) == "*") then
                          parseLoad("0":(tail xs))
                       else
                          parseLoad("1":(tail xs))
                     else
                       []
                  6 -> if (length xs == 3 && (xs !! 1) == "FILE")
                    then
                      parseUpload (xs !! 2)
                    else
                      []
                  _ -> []


parseUpload:: String -> [Either String Int] -- TODO :: ловить исключения с помощью catch
parseUpload str = [Right 6, Left (f str)]
  where
     f str = if (head str == '\'') && (last str == '\'') then init (tail str) else ">Error: File name error"

parseLoad:: [String] -> [Either String Int]
parseLoad (t:tokens) = case t of
    "0" -> [Right 5, Left (f $ last tokens)]
    "1" -> [Right 5, Left (f $ last tokens), Left (head tokens)]
 where
   f str = if (head str == '\'') && (last str == '\'') then init (tail str) else ">Error: File name error"

parseSelect:: [String] -> [Either String Int]
parseSelect tokens = let

  p1:: Parsec [Char] u String
  p1 = pure (++) <*> (many space *> many1 (tableData <|> char '\'' <|> char '=' <|> char '.' <|> char '(' <|> char ')' <|> char '-')) <*>
      (try (pure (++) <*> many space <*> many1 anyChar) <|> (many space *> pure []))

  p2 str = case parse p1 "" str of
      Left err  -> ""
      Right ok  -> ok

  res = case length tokens of
     1 -> [Right 4, Left $ head tokens, Right 0]   -- [Right 4,Left "table_name", Right 0]
     2 -> if (length (parseCommas (last tokens)) == 0) then [] else [Right 4, Left $ head tokens, Right 0] ++ fmap (Left . p2) (parseCommas (last tokens))  -- [Right 4,Left "table_name",Right 0,Left "DEPT",Left "NAME",Left "JOB"]
     _ -> case head tokens of
       "0" -> if (length tokens == 8 && (tokens !! 2) == "FROM" && (tokens !! 4) == "WHERE" && length (parseCommas (tokens !! 1)) /= 0) then
          if (checkPredicate (tokens !! 6))
            then -- [Right 4,Left "table_name",Right 1,Left "Age",Left ">",Right 18,Left "FirstName",Left "LastName"]
              [Right 4, Left $ (tokens !! 3), Right 1, Left (tokens !! 5), Left (tokens !! 6), identifyData (tokens !! 7)] ++ fmap (Left . p2) (parseCommas (tokens !! 1))  -- имя таблицы, столбцы, предикат
            else
               [Left "Error> Predicate not recognized"]
          else
            []
       "1" -> if (length tokens == 10 && (tokens !! 2) == "FROM" && (tokens !! 4) == "JOIN") then
            let
              -- список имен колонок для вывода. Список пуст, если t = 0, если нет, то его длина = t
              (t, listColumns) = if (tokens !! 1 == "*") then (0, []) else
                let
                  resList = fmap (Left . p2) (parseCommas (tokens !! 1))
                  in (length resList, resList)

              -- (список имен таблиц, дерево из JOIN)
              -- дерево хранится в виде: предикат из трех элементов, Right k - длина левого поддерева,
              --                        список из k элементов - левое поддерево, конец списка - правое поддерево
              (listNames, tree) = let
                  (a1,b1) = doWork (tokens !! 3)
                  (a2,b2) = doWork (tokens !! 5)
                  predicate = if (checkPredicate (tokens !! 8))
                     then Left <$> (drop 7 tokens)
                     else [Left (">Error: Predicate \"" ++ (tokens !! 8) ++ "\" not recognized")]

                  in (a1 ++ a2, predicate ++ [Right (length b1)] ++ b1 ++ b2)

              doWork::String -> ([Either String Int], [Either String Int])
              doWork []  = ([],[])
              doWork str = case parse parser' "" str of
                  Left err  -> ([],[Left (">Error: \"" ++ str ++ "\" not parse")])
                  Right ok  -> if (1 == length ok) then
                       ([Left str], [Left str])
                    else
                       if (length ok == 7 && (ok !! 1) == "JOIN" && (ok !! 3) == "ON") then
                          let
                            (a1,b1) = doWork (head ok)
                            (a2,b2) = doWork (ok !! 2)
                            predicate = if (checkPredicate (ok !! 5))
                              then Left <$> (drop 4 ok)
                              else [Left (">Error: Predicate \"" ++ (ok !! 5) ++ "\" not recognized")]

                            in (a1 ++ a2, predicate ++ [Right (length b1)] ++ b1 ++ b2)
                       else
                         ([],[Left (">Error: \"" ++ str ++ "\" not parse")])

              in (Right 4):listNames ++ [Right 2, Right t] ++ listColumns ++ tree
              -- номер команды, список имен таблиц, Right 2, Right (n, 0) - выводить все или нет,
              -- если 0 - то выводить все и дальше идет дерево в виде списка
              -- если n - то идут n столбцов для вывода и дальше дерево в виде списка
          else
            []
  in res


parseRemove::(String, String, String, String) -> [Either String Int]
parseRemove (tableName, name, predicate, value) = let
    (res,flag) = case name of
        "undefined" -> ([], 1)
        _           -> if (checkPredicate predicate) then ([(Left name),(Left predicate),(identifyData value)], 0)
            else ([Left "Error> Predicate not recognized"],0)
  -- [Right 2,Left "table_name",Right 0,Left "ID",Left "=",Right 4,Left "Units",Right 148,Left "Money",Right 23680]
  in ((Right 3):(Left tableName):(Right flag):res)

--распознает данные, введнные как строки или как числа
parseData = pure (++) <*>
   (pure (++) <*> (pure (:) <*> char '\'' <*> many (satisfy ( /= '\''))) <*> string "\'" <|>
   many1 (tableData <|> char '-')) <*>
      (try (pure (++) <*> many space <*> many1 anyChar) <|> (many space *> pure []))  -- проверяет случаи: 'abc' abracadabra

parseUpdate::(String, String, String, String, String) -> [Either String Int]
parseUpdate (tableName, userData, name, predicate, value) = let
  y = let
      f str | str == "" = [Left (">Error: Invalid character entered")]
            | otherwise = case span (/= '=') str of
                 (_, "") -> [Left (">Error: Symbol '=' not found")]
                 (a,_:b)  -> if (b == "" || a == "") then [Left (">Error: Argument not found")] else (Left a):[(identifyData b)]

      p1:: Parsec [Char] u String
      p1 = many space *> pure (++) <*> (pure (++) <*> many1 tableData <*> string "=") <*> parseData
      --p1 = many space *> many1 (tableData <|> char '\'' <|> char '=' <|> char '.' <|> char '-')

      p2 str = case parse p1 "" str of
          Left err  -> ""
          Right ok  -> ok

    in (concat $ fmap (f . p2) (parseCommas userData))

  (res,flag) = case name of
    "undefined" -> (y, 1) -- [Right 2,Left "table_name",Right 1,Left "Units",Left "148",Left "Money",Right 23680]

    _           -> if (checkPredicate predicate) then ((Left name):(Left predicate):(identifyData value):y, 0)
      else ([Left "Error> Predicate not recognized"],0)
   -- [Right 2,Left "table_name",Right 0,Left "ID",Left "=",Right 4,Left "Units",Right 148,Left "Money",Right 23680]
  in ((Right 2):(Left tableName):(Right flag):res)


parseInsert::(String, String, String) -> [Either String Int]
parseInsert (tableName, columns, userData) = let
     y = let
           f str   | str == "\'" = Left (">Error: Invalid character entered")
                   | otherwise = identifyData str
           p1:: Parsec [Char] u String
           p1 = many space *> parseData

           p2 str = case parse p1 "" str of
               Left err  -> "\'"
               Right ok  -> ok

          in (fmap (f . p2) (parseCommas userData))

     (res,flag) = case columns of
             "undefined"  -> (y,1)
             _            -> let
                    f str | str == "" = Left (">Error: Invalid character entered")
                          | otherwise = Left str

                    p1:: Parsec [Char] u String
                    p1 = many space *> many1 (tableData <|> char '-')

                    p2 str = case parse p1 "" str of
                        Left err  -> ""
                        Right ok  -> ok

                    count = fmap (f . p2) (parseCommas columns)

                    err = if (length count /= length y) then
                      [Left (">Error: Data mapping error")]
                    else
                      []

               in (((Right $ length count):count ++ y ++ err), 0)
 in ((Right 1):(Left tableName):(Right flag):res)


parseCreate:: (String, String) -> [Either String Int]   -- parseCreate ("Vova", "Int a,String b,Int c")
parseCreate (tableName, userData) = let {
  y = let
        f (a,b)  | a == "String"   = if (b == "") then Left (">Error: After 'String' should go title") else Left (">String:" ++ b)
                 | a == "Int"      = if (b == "")
                      then Left (">Error: After 'Int' should go title") else Left (">Int:" ++ b)
                 | otherwise       = Left (">Error: Сommand not recognized")

        p1:: Parsec [Char] u ([Char], [Char])
        p1 = (,) <$> (many space *> many1 tableData) <*> (many space *> many1 tableData)

        p2 str = case parse p1 "" str of
            Left err  -> ("","")
            Right (a,b)  -> (a,b)

    in (fmap (f . p2) (parseCommas userData))
  } in ((Right 0):(Left tableName):y)


--проверяет строки на корректность: 'data', 4
identifyData:: String -> Either String Int
identifyData str = case identifyData1 str of
       "Int"          -> Right (read str :: Int)
       "String"       -> Left (init (tail str))
       "ErrorInt"     -> Left (">Error: \'" ++ str ++ "\' not Int")
       "ErrorString"  -> Left (">Error: parse error \'" ++ str ++ "\'")
  where
  identifyData1 input = if checkNum input then "Int" else
   if ((dropWhile (/= '\'') input) == [])  then "ErrorInt" else
     if (length input < 2 || (head input /= '\'') || (last input /= '\''))
       then "ErrorString" else "String"

--парсер запятых
parseCommas:: String -> [String]
parseCommas userData = case (parse ((many1 (tableData <|> char ' ' <|> char '\'' <|> char '=' <|> char '.' <|> char '-')) `sepBy1` (char ',')) "" userData) of
    Left err -> []
    Right a -> a

parsePoint:: String -> (String, String)
parsePoint s = let (a,b) = span (/= '.') s in (a, if (b == "") then b else tail b)

--проверяет является ли строка числом
checkNum:: String -> Bool
checkNum []     = True
checkNum z = case (readMaybe z :: Maybe Int) of
  Nothing -> False
  _       -> True

--проверяет предикат
checkPredicate:: String -> Bool
checkPredicate [] = False
checkPredicate s  = any (== s) [">", "<", ">=", "<=", "=", "<>"]

--проверяет имя таблицы и полей
checkName:: String -> Bool
checkName [] = False
checkName (x:xs) = (isLetter x || isDigit x || (x == '_')) && (if (length xs > 0) then checkName xs else True)

--тип команды
typeCommand:: [String] -> Int
typeCommand [] = -1
typeCommand (x:xs) = case x of
  "CREATE" -> 0
  "INSERT" -> 1
  "UPDATE" -> 2
  "REMOVE" -> 3
  "SELECT" -> 4
  "LOAD"   -> 5
  "UPLOAD" -> 6
  _        -> -1

--парсер в строку по пробелам
parser' :: Parsec String u [String]
parser' = (many1 (tableData <|> specialSymbols <|> char ')') <|> brackets) `sepBy1` (char ' ')

brackets :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
brackets = between (string "(") (string ")") ((try helper) <|> nestedBrackets)

helper = (pure (++) <*> many1 (tableData <|> char ' ' <|> specialSymbols) <*> nestedBrackets)

nestedBrackets :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
nestedBrackets = try( (pure combine) <*> string "(" <*> (try helper <|> nestedBrackets) <*> string ")" <*> (many1 (tableData <|> char ' ' <|> specialSymbols)) <*> nestedBrackets) <|> pure []

tableData:: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
tableData = letter <|> digit <|> char '_'

specialSymbols:: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
specialSymbols = char ',' <|> char '\'' <|> char '=' <|> char '>' <|> char '<' <|> char '.' <|> char '*' <|> char '-'

combine::[Char] -> [Char] -> [Char] -> [Char] -> [Char]-> [Char]
combine a b c d e = a ++ b ++ c ++ d ++ e

--проверяет строки вида "CREATE TABLE name_table (String s, Int 5)(String 3)"
checkInput::String -> Bool
checkInput [] = True
checkInput (x:xs) = case x of
  ')' -> if (length xs > 0) then
    if (head xs == ' ') then checkInput (tail xs) else False
    else True
  ' ' -> checkInput xs
  _   -> case xs of
    [] -> True
    (a:b) -> if (isLetter a || isDigit a || any (== a) [',', '\'', '=', '>', '<', '.', '*', ' ', '_', ')', '-', '(']) then checkInput xs else False

{-
words1 = string "CREATE" <|> string "TABLE" <|> string "INSERT" <|> string "INTO" <|> string "VALUES" <|>
   string "UPDATE" <|> string "SET" <|> string "WHERE" <|>
       string "REMOVE" <|> string "FROM" <|> string "SELECT" <|> string "JOIN" <|> string "ON"
       -}
--testParse = string "CREATE TABLE" <|> string "INSERT INTO"
