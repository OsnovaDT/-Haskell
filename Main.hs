import Data.List
import Data.List (delete)
import System.Random

computerGameBoardCoordinates :: [[Char]]
computerGameBoardCoordinates = ["C3", "D3", "E3"]

userGameBoardCoordinates = []

isInList element [] = False
isInList element (x : xs)
  | element == x = True
  | otherwise = element `isInList` xs

getUserStep [] = do
  putStrLn "Вы выиграли"
getUserStep coordinates = do
  putStrLn "Ваш ход: "
  user_coordinate <- getLine
  if user_coordinate `isInList` coordinates
    then do
      putStrLn "Попал\n"
      let new_coordinates = delete user_coordinate coordinates
      getUserStep new_coordinates
    else do
      putStrLn "Не попал\n"
      getComputerStep 0
      getUserStep coordinates

-- Получаем рандомную координату для удара по полю пользователя
getRandomCoordinate std_gen = do
  let a = take 2000 (randomRs ('A', 'J') std_gen)
      b = take 2000 (randomRs ('1', '9') std_gen)
  a ++ b

charToString :: Char -> String
charToString c = [c]

getComputerStep number = do
  std_gen <- getStdGen
  let randomCoordinate = getRandomCoordinate std_gen
      a = charToString (randomCoordinate !! number) ++ charToString (randomCoordinate !! (number + 2000))
  putStrLn ("Мой ход: " ++ a)
  putStrLn a
  putStrLn "Попал? (y - Да; другой символ - Нет)"
  is_get <- getLine
  if is_get == "y"
    then do
      putStrLn "Ура\n"
      getComputerStep (number + 1)
    else putStrLn "Эх...\n"

main = getUserStep computerGameBoardCoordinates

-- main = do
--   std_gen <- newStdGen
--   let a = getRandomCoordinate std_gen
--   print (a)
--   print (a !! 0)
--   print (a !! 20)

-- let a = [1, 2, 3, 4]
-- print (a !! 1)

-- print (getRandomCoordinate std_gen)
-- print (getRandomCoordinate std_gen)
