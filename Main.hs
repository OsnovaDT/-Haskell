import Data.Char (chr, ord)
import Data.List (delete)
import System.Random

isIn element [] = False
isIn element (x : xs)
  | element == x = True
  | otherwise = element `isIn` xs

charToString :: Char -> String
charToString c = [c]

-- Из этой последовательности будут браться координаты для атаки кораблей пользователя
getRandomCoordinateSequence std_gen = do
  let letters = take 2000 (randomRs ('A', 'J') std_gen)
      digits = take 2000 (randomRs ('1', '9') std_gen)
  letters ++ digits

-- Берем координату из рандомной последовательности координат
getComputerHit coordinate_index = do
  std_gen <- getStdGen
  let random_coordinate_sequence = getRandomCoordinateSequence std_gen
      -- Заносим букву и цифру в координату из рандомной последовательности координат
      coordinate = charToString (random_coordinate_sequence !! coordinate_index) ++ charToString (random_coordinate_sequence !! (coordinate_index + 2000))
  putStrLn ("Мой ход: " ++ coordinate)
  putStrLn "Попал? (y - Да; Любой другой символ - Нет)"
  is_hit <- getLine
  -- Если попал
  if is_hit == "y"
    then do
      putStrLn "Ура\n"
      -- Атакуем следующую координату
      getComputerHit (coordinate_index + 1)
    else putStrLn "Эх...\n"

isUserKillOrOnlyHitShip
  computer_ships_coordinates
  user_coordinate = do
    let -- Буква в координате, введенной пользователем
        user_coordinate_letter = head user_coordinate
        -- Цифра в координате, введенной пользователем
        user_coordinate_digit = user_coordinate !! 1
    -- Находим координаты слева, справа, сверху и снизу ближайшие к координате пользователя
    let coordinate_right_of_user_coordinate = charToString (chr (ord user_coordinate_letter + 1)) ++ charToString user_coordinate_digit
        coordinate_left_of_user_coordinate = charToString (chr (ord user_coordinate_letter - 1)) ++ charToString user_coordinate_digit
        coordinate_top_of_user_coordinate = charToString user_coordinate_letter ++ charToString (chr (ord user_coordinate_digit + 1))
        coordinate_bottom_of_user_coordinate = charToString user_coordinate_letter ++ charToString (chr (ord user_coordinate_digit - 1))

    -- Проверяем находится ли координата пользователя с краю
    if user_coordinate_digit == '1'
      then -- Проверяем наличие координат, ближайших к координате пользователя

        if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
          || coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
          || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
          then putStrLn "Попал\n"
          else putStrLn "Убил\n"
      else
        if user_coordinate_letter == 'A'
          then -- Проверяем наличие координат, ближайших к координате пользователя

            if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
              || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
              || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
              then putStrLn "Попал\n"
              else putStrLn "Убил\n"
          else
            if user_coordinate_letter == 'J'
              then -- Проверяем наличие координат, ближайших к координате пользователя

                if coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
                  then putStrLn "Попал\n"
                  else putStrLn "Убил\n"
              else -- Если координата пользователя не находится с краю

                if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
                  then putStrLn "Попал\n"
                  else putStrLn "Убил\n"

-- Если компьютер проиграл
getUserHit [] coordinate_index_for_computer_hit = do
  putStrLn "Вы выиграли"

-- Если компьютер еще не проиграл
getUserHit computer_ships_coordinates coordinate_index_for_computer_hit = do
  putStrLn "Ваш ход: "
  user_coordinate <- getLine
  -- Если пользователь попал
  if user_coordinate `isIn` computer_ships_coordinates
    then do
      let -- Удаляем из коордиант кораблей компьютера координату, введенную пользователем
          computer_ships_coordinates_after_user_hit = delete user_coordinate computer_ships_coordinates

      isUserKillOrOnlyHitShip computer_ships_coordinates_after_user_hit user_coordinate

      getUserHit computer_ships_coordinates_after_user_hit coordinate_index_for_computer_hit
    else -- Если пользователь не попал
    do
      putStrLn "Не попал\n"
      getComputerHit coordinate_index_for_computer_hit
      getUserHit computer_ships_coordinates (coordinate_index_for_computer_hit + 1)

startGame = do
  let computerShipsCoordinates = ["C3", "D3", "E3", "F3", "I3", "I4", "I5", "B9", "C9", "D9", "B7", "C7", "E5", "F5", "I8", "I9", "B5", "G8", "G10", "H1"]
  getUserHit computerShipsCoordinates 0

main = startGame