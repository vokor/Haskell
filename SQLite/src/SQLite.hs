module SQLite where

import Executor
import Parser
import TableAST
import ParserAST

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.Tuple

main' :: IO ()
main' = do
  putStrLn "--------started---------"
  s <- newIteration startTable
  putStrLn "--------finished--------"
  return ()

newIteration :: CommonTable -> IO String
newIteration table = do
    str <- readInf
    executorHelper str table

executorHelper :: String -> CommonTable -> IO String
executorHelper str commonTable = do
 case parser str of
  Left logError -> do
    putStrLn logError
    newIteration commonTable
  Right command -> do
    (result,newTable) <- executor command commonTable
    putStrLn result
    case command of
      Exit -> return ""
      _    -> newIteration newTable

executor :: Commands -> CommonTable -> IO (String, CommonTable)
executor command commonTable = case command of
  Exit -> executeExit commonTable
  Create tableName nameColumns -> return $ runReader (executeCreate tableName nameColumns) commonTable
  Insert tableName nameColumns userData -> return . swap $ runWriter (executeInsert commonTable tableName nameColumns userData)
  Show select -> return $ (executeSelectHelper select commonTable, commonTable)
  Remove tableName maybePredicate -> return $ runState (executeRemove tableName maybePredicate) commonTable
  Load maybeTable fileName -> do
    result <- evalState (executeLoad maybeTable fileName) commonTable
    return (result, commonTable)
  Upload fileName -> executeUpload fileName commonTable

--считывает пока не получает строку
readInf :: IO String
readInf = do
  str <- getLine
  if (str == "" || str == "\n") then
    readInf
  else
    return str

startTable :: CommonTable
startTable = []
