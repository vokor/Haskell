module SQLite where

import Parser as P
import Commands as C
import Errors as E

newtype CommonTable a = CommonTable {getCommonTable :: [a]} deriving Show
newtype Table a = Table {getTable :: [a]} deriving Show     -- Table [(Left 1)]

startTable = CommonTable []
testTable = CommonTable [(Table [(Left "Hello"), (Right 1)]), (Table [(Left "d"), (Right 1)])]

main':: IO ()
main' = do
  putStrLn "--------started---------"
  s <- executor startTable
  putStrLn "--------finished--------"
  return ()

executor:: CommonTable (Table (Either String Int)) -> IO String
executor table = do
     s <- readInf
     if (s == "exit") then     --проверяет не введена ли команда выхода
       return ""
     else
       executorHelper s table

executorHelper:: String -> CommonTable (Table (Either String Int)) -> IO String
executorHelper str table = do
   let
     list = P.parser str
     (logErr, res) = E.checkList list
     (resLog, newTable) = case res of
        False -> (return logErr, table)
        True  -> case head list of
          Right 0 -> if ((findTable 0 (getName1 (list !! 1)) (getCommonTable table)) /= (-1)) then (return $ ">Error: Table with name '" ++ getName1 (list !! 1) ++ "' already exists", table)
            else (return $ ">Table '" ++ getName1(list !! 1) ++ "' created!",
             CommonTable $ ((Table $ C.executeCreate (tail list)):(getCommonTable table)))
          Right 1 -> f $ tail list
               where
                 f (x:xs) = case findTable 0 (getName1 x) (getCommonTable table) of
                   (-1) -> (return $ ">Error: table with name \'" ++ (getName1 x) ++ "\' does not exist", table)
                   _    -> let
                     t = findTable 1 (getName1 x) (getCommonTable table)
                     (a,b) = C.executeInsert xs (tail $ getTable1 table t)
                     in (return $ a, CommonTable $ ((take (t - 1) $ getCommonTable table) ++ [(Table ((head $ getTable1 table t):b))] ++ (drop t $ getCommonTable table)))
          Right 2 -> f $ tail list
               where
                 f (x:xs) = case findTable 0 (getName1 x) (getCommonTable table) of
                   (-1) -> (return $ ">Error: table with name \'" ++ (getName1 x) ++ "\' does not exist", table)
                   _    -> let
                     t = findTable 1 (getName1 x) (getCommonTable table)
                     (a,b) = C.executeUpdate xs (tail $ getTable1 table t)
                     in (return $ a, CommonTable $ ((take (t - 1) $ getCommonTable table) ++ [(Table ((head $ getTable1 table t):b))] ++ (drop t $ getCommonTable table)))
          Right 3 -> f $ tail list
               where
                 f (x:xs) = case findTable 0 (getName1 x) (getCommonTable table) of
                   (-1) -> (return $ ">Error: table with name \'" ++ (getName1 x) ++ "\' does not exist", table)
                   _    -> let
                     t = findTable 1 (getName1 x) (getCommonTable table)
                     (a,b) = C.executeRemove xs (tail $ getTable1 table t)
                     in (return $ a, CommonTable $ ((take (t - 1) $ getCommonTable table) ++ [(Table ((head $ getTable1 table t):b))] ++ (drop t $ getCommonTable table)))
          Right 4 -> let
                 g:: [Either String Int] -> Int -> Int
                 g (x:xs) step = case x of
                   Right _ -> step
                   Left  _ -> g xs (step + 1)

                 h:: [Either String Int] -> [[Either String Int]]
                 h x =  f <$> (take (g x 0) (tail list))

                 f::Either String Int -> [Either String Int]
                 f x = case findTable 0 (getName1 x) (getCommonTable table) of
                   (-1) -> [Left (">Error: table with name \'" ++ (getName1 x) ++ "\' does not exist")]
                   _    -> getTable1 table (findTable 1 (getName1 x) (getCommonTable table))

                 res = if any ( == False) (E.helper . head <$> (h (tail list))) then
                    ">Error: Table not found"
                  else
                    C.executeSelect (drop ((g (tail list) 0) + 1) list) (h (tail list))
               in (return $ res, table)

          Right 5 -> let
               fileName = getName1 (list !! 1)
               dumpInf = case length list of
                 2 -> (\x -> getTable x) <$> (getCommonTable table)
                 _ -> case findTable 0 (getName1 (list !! 2)) (getCommonTable table) of
                   (-1) -> [[Left (">Error: table with name \'" ++ (getName1 (list !! 2)) ++ "\' does not exist")]]
                   _    -> [getTable1 table (findTable 1 (getName1 (list !! 2)) (getCommonTable table))]
               in (C.executeLoad fileName dumpInf, table)

          Right 6 -> (return "", CommonTable []){-let
             (s,t) = do
                       (a,b) <- executeUpload ((\x -> getTable x) <$> (getCommonTable table)) (getName1 $ list !! 1)
                       return (a, b-)
             in (return a, CommonTable ((\x -> Table x) <$> b)) -}

          _       -> undefined                   --TODO::
   --do
   if (length list == 0 || head list /= Right 6) then do
      a <- resLog
      putStrLn a
      executor newTable
   else do
      (a,b) <- executeUpload ((\x -> getTable x) <$> (getCommonTable table)) (getName1 $ list !! 1)
      putStrLn a
      executor $ CommonTable ((\x -> Table x) <$> b)

--считывает пока не получает строку
readInf :: IO String
readInf = do
  str <- getLine
  if (str == "" || str == "\n") then
      readInf
  else
      return str

getName1 (Left a) = a

--ищет таблицу по имени
findTable::Int -> String -> [Table (Either String Int)] -> Int
findTable _ _ [] = -1
findTable i name (x:xs) = if (getName1(head (getTable x)) == name) then i else findTable (i + 1) name xs

--выдает количество столбцов переданной таблицы
getCountOfColumns:: Table (Either String Int) -> Int
getCountOfColumns x = case (getTable x) !! 1 of Right a -> a

--возвращает таблицу (в виде списка) по номеру (нумерация с 1)
getTable1 (CommonTable table) it | it < 1              = [Left "undefined"]
                                 | it > (length table) = [Left "undefined"]
                                 | otherwise           = getTable $ table !! (it - 1)
