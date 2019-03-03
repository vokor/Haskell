module Errors where

import Data.List

--проверяет корректен ли список
checkList::[Either String Int] -> (String, Bool)
checkList [] = (">Error: Incorrect Input", False)
checkList (x:xs) = case x of
  (Right 0) -> case dropWhile (helper) xs of     -- CREATE TABLE
            []              -> if (checkDiffNames (tail xs) (tail xs) && (1 < length xs)) then ("", True)
              else if (1 >= length xs) then (">Error: Incorrect Input",False) else (">Error: Some column names are the same", False) where
                checkDiffNames::[Either String Int] -> [Either String Int] -> Bool
                checkDiffNames [] list = True
                checkDiffNames (n:m) (y:list) = (not $ any (== (getName n)) (getName <$> list)) && checkDiffNames m (list ++ [y])

            ((Left str):ys) -> (str, False)

  (Right _) -> case dropWhile (helper) xs of
            []              -> ("", True)
            ((Left str):ys) -> (str, False)


--вытаскивает имя
getName (Left a) = tail $ dropWhile (/= ':') a


--проверяет элемент на ошибку
helper:: Either String Int -> Bool
helper a = case a of
  Left str -> not $ isInfixOf ">Error:" str
  _        -> True
