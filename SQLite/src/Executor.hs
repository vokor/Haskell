module Executor where

import TableAST
import ParserAST

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Data.List


executeExit :: CommonTable -> IO (String, CommonTable)
executeExit table = return (">Goodbye!!!", table)

---------------------------------------
executeCreate :: TableName -> [ColumnType] -> Reader CommonTable (String, CommonTable)
executeCreate tableName nameColumns = do
  table <- ask
  case or $ map (\t -> getName t == tableName) table of
    True  -> return (">Error: Table with name '" ++ tableName ++ "' already exists", table)
    False -> do
      newTable <- local ((Table tableName (map (\name -> (name, [])) nameColumns)):) ask
      return (">Done", newTable)
---------------------------------------
executeInsert :: CommonTable -> TableName -> Maybe [ColumnName] -> [Data] -> Writer String CommonTable
executeInsert commonTable tableName maybeNameColumns userData = do
  let tryFind = find (\t -> getName t == tableName) commonTable
  case tryFind of
    Nothing    -> writer (commonTable, ">Error: table with name \'" ++ tableName ++ "\' does not exist")
    Just table -> do
     let
        withoutTable = delete table commonTable
        (lstTable,_) = unzip $ getData table
        list = case maybeNameColumns of
          Nothing          -> if (length lstTable == length userData) then zip lstTable userData else []
          Just nameColumns -> do
            True  <- return $ length (nub nameColumns) == length userData
            True  <- return $ length nameColumns == length userData
            (x,y) <- zip nameColumns userData
            z     <- lstTable
            case z of
              S name -> do
                True <- return $ x == name
                return (S name, y)
              I name -> do
                True <- return $ x == name
                return (I name, y)
     if (length list == length userData) then do
       let
          f []         = ">Done"
          f ((a,b):xs) = case a of
            S name -> case b of
              String _ -> f xs
              Int str  -> ">Error: Column " ++ name ++ " should have type String, but " ++ show str ++ " is Int"
            I name -> case b of
              String str -> ">Error: Column " ++ name ++ " should have type Int, but '" ++ str ++ "' is String"
              Int _      -> f xs

          newTable = case f list of
            ">Done" -> Table tableName (g $ getData table)
            _       -> table

          g []         = []
          g ((a,b):xs) = case find (\(x,y) -> x == a) list of
            Nothing    -> (a,b ++ [Nothing]):(g xs)
            Just (x,y) -> (a,b ++ [Just y]):(g xs)
       tell $ f list
       return $ newTable:withoutTable
     else do
       tell ">Error: Type mismatch or column not recognized"
       return commonTable
---------------------------------------
executeSelectHelper :: Select -> CommonTable -> String
executeSelectHelper select table = let
  (logError, resTable) = runState (runReader (executeSelect select) table) defaultTable

  toOut | resTable == defaultTable = logError
        | otherwise = outTable resTable
  in toOut

--вывод таблицы
outTable :: Table -> String
outTable table = let
  (_,tableData) = unzip (getData table)
  -- делает из таблицы строку
  doStringHelper list = let
      f x = case x of
        Nothing           -> "|"
        Just (String str) -> str ++ "|"
        Just (Int int)    -> (show int) ++ "|"
    in "|" ++ concat (map f list) ++ "\n"
  in concat $ map doStringHelper (transpose tableData)

-- ищет таблицу по имени
findTable :: String -> Reader CommonTable (Maybe Table)
findTable tableName = asks $ find (\t -> getName t == tableName)

deleteTable ::Table -> Reader CommonTable CommonTable
deleteTable table = asks $ delete table

-- идентификация вложенной команды и передача исполняющей функции
invokeCommand :: Either TableName NestedCommand -> Reader CommonTable (State Table String)
invokeCommand (Left tableName) = do
  maybeTable <- findTable tableName
  case maybeTable of
    Nothing    -> reader $ \e -> return (">Error: Table with name '" ++ tableName ++ "' not found")
    Just table -> reader $ \e -> state $ \s -> ("",Table (getName table) (map (\(x,y) -> (updateData table x,y)) $ getData table))
invokeCommand (Right (Left select)) = do
  commonTable <- ask
  return $ runReader (executeSelect select) commonTable
invokeCommand (Right (Right join)) = do
  commonTable <- ask
  return $ runReader (executeJoin join) commonTable

posName :: Table -> ColumnName -> Maybe (DataTable)
posName table name = snd <$> find (\(x,y) -> findName (getName table) x name) (getData table)

findName :: TableName -> ColumnType -> ColumnName -> Bool
findName nameTable nameColumn name | ident nameColumn == name = True
                                   | otherwise = case span (/= '.') name of
                                     (a,x:[]) -> False
                                     ([],x)   -> False
                                     (a,[])   -> ident nameColumn == updateColumn nameTable a
                                     (a,x:xs) -> case span (/= '.') $ ident nameColumn of
                                       (b,[])   -> a == nameTable && b == xs
                                       (b,y:ys) -> a == nameTable && xs == ys


--вытащить имена
ident (S n) = n
ident (I n) = n

updateData table (S n) = case find (== '.') n of
  Nothing -> S ((getName table) ++ "." ++ n)
  _ -> S n
updateData table (I n) = case find (== '.') n of
  Nothing -> I ((getName table) ++ "." ++ n)
  _ -> I n

updateColumn table s = case find (== '.') s of
  Nothing -> table ++ "." ++ s
  _ -> s

deleteFromTableS :: (Data -> Data -> Bool) -> Table -> DataTable -> Data -> State Table String
deleteFromTableS predicate table list userData = do
  let
    toList = do
      (x,y) <- zip list (transpose . snd $ unzip (getData table))
      True <- return $ case x of
        Nothing -> False
        Just c  -> predicate c userData
      return y
  let
    lst = updateData table <$> (fst $ unzip (getData table))
    newTable = case null toList of
      True  -> Table (getName table) $ zip lst (repeat [])
      False -> Table (getName table) $ zip lst (transpose toList)
  state $ \e -> ("Done",newTable)

predicateSelect :: Predicate Data -> State Table String
predicateSelect predicate = do
  table <- get
  case predicate of
    Cons columnName userData cmp -> case posName table columnName of
      Nothing   -> state $ \e -> (">Error: In table '" ++ (getName table) ++ "' column with name '" ++ columnName ++ "' not found", defaultTable)
      Just list -> case cmp of
        NEqual  -> deleteFromTableS (/=) table list userData
        Equal   -> deleteFromTableS (==) table list userData
        ELess   -> deleteFromTableS (<=) table list userData
        EBigger -> deleteFromTableS (>=) table list userData
        Less    -> deleteFromTableS (<) table list userData
        Bigger  -> deleteFromTableS (>) table list userData

executeSelect :: Select -> Reader CommonTable (State Table String)
executeSelect (Select maybeColumnName command maybePredicate) = do
  stateTable <- invokeCommand command
  let
    composeTable :: State Table String
    composeTable = case maybePredicate of
      Nothing        -> stateTable
      Just predicate -> if execState stateTable defaultTable == defaultTable
        then
          stateTable
        else
          state $ \e -> runState (predicateSelect predicate) (execState stateTable defaultTable)

  return $ case maybeColumnName of
    Nothing   -> composeTable
    Just list -> do
      let
        table = execState composeTable defaultTable
        newData = do
          c <- (updateColumn $ getName table) <$> list
          (a,b) <- getData table
          True <- return $ c == ident a
          return (a,b)
      state $ \e -> case (table /= defaultTable && (length list == length newData || null (getData table))) of
        True  -> (mempty, Table (getName table) newData)
        False -> if (table == defaultTable)
          then
             runState composeTable defaultTable
          else
             (">Error: Incorrect list of columns in table", defaultTable)


executeJoinHelper :: Table -> Table -> State Table String
executeJoinHelper table1 table2 = do
  let
    name1 = getName table1
    name2 = getName table2
    (column1,data1) = unzip $ getData table1
    (column2,data2) = unzip $ getData table2
  case name1 == name2 of
    True  -> return (">Error: Unable to perform an operation of the form (" ++ name1 ++ " JOIN " ++ name2 ++ ")")
    False -> do
      let
        newData = do
          a <- transpose data1
          b <- transpose data2
          return (a ++ b)
      state $ \e -> ("", Table (name1 ++ name2) $ zip (column1 ++ column2) (transpose newData))

deleteFromTableJ :: (Data -> Data -> Bool) -> Table -> DataTable -> DataTable -> State Table String
deleteFromTableJ predicate table data1 data2 = do
  let
    toList = do
      (x,y,z) <- zip3 data1 data2 (transpose . snd $ unzip (getData table))
      True <- return $ case x of
        Nothing -> False
        Just a  -> case y of
          Nothing -> False
          Just b   -> predicate a b
      return z
  let
    lst = updateData table <$> (fst $ unzip (getData table))
    newTable = Table (getName table) $ zip lst (transpose toList)
  state $ \e -> ("",newTable)

predicateJoin :: Predicate ColumnName -> [ColumnType] -> State Table String
predicateJoin predicate data1 = do
  table <- get
  let
    check str = case find (\x -> ident x == updateColumn (getName table) str) data1 of
       Nothing -> False
       _ -> True
  case predicate of
    Cons columnName1 columnName2 cmp -> if (columnName1 == columnName2 || (not $ check columnName1) || check columnName2)
     then
      state $ \e -> (">Error: Incorrect column names '" ++ columnName1 ++ "' and '" ++ columnName2, defaultTable)
     else case posName table columnName1 of
      Nothing   -> state $ \e -> (">Error: In table '" ++ (getName table) ++ "' column with name '" ++ columnName1 ++ "' not found", defaultTable)
      Just list1 -> case posName table columnName2 of
        Nothing   -> state $ \e -> (">Error: In table '" ++ (getName table) ++ "' column with name '" ++ columnName2 ++ "' not found", defaultTable)
        Just list2 -> case cmp of
          NEqual  -> deleteFromTableJ (/=) table list1 list2
          Equal   -> deleteFromTableJ (==) table list1 list2
          ELess   -> deleteFromTableJ (<=) table list1 list2
          EBigger -> deleteFromTableJ (>=) table list1 list2
          Less    -> deleteFromTableJ (<) table list1 list2
          Bigger  -> deleteFromTableJ (>) table list1 list2

executeJoin :: Join -> Reader CommonTable (State Table String)
executeJoin (Join command1 command2 maybePredicate) = do
  stateTable1 <- invokeCommand command1
  stateTable2 <- invokeCommand command2
  let
    table1 = execState stateTable1 defaultTable
    table2 = execState stateTable2 defaultTable
    data1 = fst.unzip $ getData table1
    composeTable | table1 == defaultTable = stateTable1
                 | table2 == defaultTable = stateTable2
                 | otherwise = executeJoinHelper table1 table2
    resTable = case maybePredicate of
      Nothing        -> composeTable
      Just predicate -> if execState composeTable defaultTable == defaultTable
        then
          composeTable
        else
          state $ \e -> runState (predicateJoin predicate data1) (execState composeTable defaultTable)
  return resTable
---------------------------------------
executeRemove :: TableName -> Maybe (Predicate Data) -> State CommonTable String
executeRemove tableName maybePredicate = do
  commonTable <- get
  case runReader (findTable tableName) commonTable of
    Nothing    -> return (">Error: Table with name '" ++ tableName ++ "' not found")
    Just table -> do
      newCommonTable <- put $ runReader (deleteTable table) commonTable
      let
        name = getName table
        userData = getData table
      case maybePredicate of
        Nothing -> do
          modify ((Table name (map (\(x,y) -> (x,[])) userData)):)
          return ">Done"
        Just predicate -> do
          case predicate of
            Cons columnName userData cmp -> case posName table columnName of
              Nothing   -> do
                modify (table:)
                return (">Error: In table '" ++ (getName table) ++ "' column with name '" ++ columnName ++ "' not found")
              Just list -> do
                let
                  newTable = execState tableState defaultTable
                  tableState = case cmp of
                    NEqual  -> deleteFromTableS (==) table list userData
                    Equal   -> deleteFromTableS (/=) table list userData
                    ELess   -> deleteFromTableS (>) table list userData
                    EBigger -> deleteFromTableS (<) table list userData
                    Less    -> deleteFromTableS (>=) table list userData
                    Bigger  -> deleteFromTableS (<=) table list userData
                modify (newTable:)
                return ">Done"
---------------------------------------
executeUpload :: FileName -> CommonTable -> IO (String, CommonTable)
executeUpload fileName commonTable = do
  contents <- (Control.Exception.try (readFile fileName)) :: IO (Either SomeException String)
  let
     inf = case contents of
        Left err  -> "Error"
        Right a -> a
  return $ case inf of
       "Error" -> (">Error: File \'" ++ fileName ++ "\' not found", commonTable)
       _       -> case maybeRead inf of
         Just table -> checkTable table commonTable
         _          -> (">Error: File contents must be of type CommonTable", [])

maybeRead :: String -> Maybe CommonTable
maybeRead s = case reads s of
   [(x, "")] -> Just (x :: CommonTable)
   _         -> Nothing

checkTable::CommonTable -> CommonTable -> (String, CommonTable)
checkTable bigTable commonTable = if length (nub bigTable) /= length bigTable || (not $ and (mainChecker <$> bigTable))
     then
       (">Error: Wrong format", commonTable)
     else (">Done", commonTable ++ bigTable)
  where
    mainChecker table = checkData (getData table) && checkName table && checkColumn table

    names table = nubBy (\x y -> ident x == ident y) (fst.unzip $ getData table)
    checkColumn table = length (names table) == length (getData table)

    checkName table = case find (== table) commonTable of
      Nothing   -> True
      otherwise -> False

    checkData [] = True
    checkData ((x,y):xs) = and $ (checkData xs):(map (helper x) y)

    helper t x = case x of
      Just (String s) -> case t of
        S _ -> True
        I _ -> False
      Just (Int s) -> case t of
          S _ -> False
          I _ -> True
      Nothing -> True
---------------------------------------
executeLoad :: Maybe [TableName] -> FileName -> State CommonTable (IO String)
executeLoad maybeTable fileName = do
  commonTable <- get
  let
    inFile str table = return $ do
            writeFile fileName (show table)
            return str

  case null commonTable of
    True -> case maybeTable of
      Nothing   -> inFile ">Done" commonTable
      otherwise -> return $ return ">Error: Вatabase is empty"
    False -> case maybeTable of
      Nothing -> inFile ">Done" commonTable
      Just list -> do
        let
          generateList = do
            x <- list
            y <- commonTable
            True <- return $ x == getName y
            return y
        if (length generateList /= length list) then
          return $ return ">Error: Not all tables in the list are in the database"
        else
          inFile ">Done" generateList
---------------------------------------
---------------------------------------
---------------------------------------
defaultTable :: Table
defaultTable = Table "" []
