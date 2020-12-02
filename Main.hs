import Data.Char (chr, ord)
import Data.List (delete)
import System.Exit
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
      digits = take 2000 (randomRs ('0', '9') std_gen)
  letters ++ digits

getComputerHit coordinate_index [] = do
  putStrLn "Вы не захотели со мной играть("
  exitFailure
getComputerHit coordinate_index user_coordinates = do
  std_gen <- getStdGen
  let random_coordinate_sequence = getRandomCoordinateSequence std_gen
      -- Берем координату из рандомной последовательности координат
      initial_coordinate = charToString (random_coordinate_sequence !! coordinate_index) ++ charToString (random_coordinate_sequence !! (coordinate_index + 2000))
      coordinate = if initial_coordinate !! 1 == '0' then charToString (head initial_coordinate) ++ "10" else initial_coordinate
  putStrLn ("Мой ход: " ++ coordinate)
  -- Если компьютер попал
  if coordinate `isIn` user_coordinates
    then do
      putStrLn "Я попал!\n"
      let new_user_coordinates = delete coordinate user_coordinates
      if length new_user_coordinates == 0
        then do
          putStrLn "Игра окончена. Я победил!"
          exitFailure
        else -- Атакуем следующую координату
          getComputerHit (coordinate_index + 1) new_user_coordinates
    else -- Если компьютер не попал
      putStrLn "Эх...\n"

isUserKillOrOnlyHitShip
  computer_ships_coordinates
  user_coordinate = do
    let -- Буква в координате, введенной пользователем
        user_coordinate_letter = head user_coordinate
        -- Цифра в координате, введенной пользователем
        user_coordinate_digit = if (length user_coordinate == 3) && (user_coordinate !! 2 == '0') then '0' else user_coordinate !! 1
    -- user_coordinate_digit = user_coordinate !! 1
    -- Находим координаты слева, справа, сверху и снизу ближайшие к координате пользователя
    let coordinate_right_of_user_coordinate = charToString (chr (ord user_coordinate_letter + 1)) ++ charToString user_coordinate_digit
        coordinate_left_of_user_coordinate = charToString (chr (ord user_coordinate_letter - 1)) ++ charToString user_coordinate_digit
        coordinate_top_of_user_coordinate = charToString user_coordinate_letter ++ charToString (chr (ord user_coordinate_digit + 1))
        coordinate_bottom_of_user_coordinate = charToString user_coordinate_letter ++ charToString (chr (ord user_coordinate_digit - 1))
    -- Проверяем находится ли координата пользователя на координатах A1 - J1
    if user_coordinate_digit == '1'
      then -- Проверяем наличие координат, ближайших к координате пользователя

        if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
          || coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
          || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
          then putStrLn "Попал\n"
          else putStrLn "Убил\n"
      else -- Проверяем находится ли координата пользователя на координатах A10 - J10

        if user_coordinate_digit == '0'
          then -- Проверяем наличие координат, ближайших к координате пользователя

            if (charToString (head coordinate_right_of_user_coordinate) ++ charToString '1' ++ charToString '0') `isIn` computer_ships_coordinates
              || (charToString (head coordinate_left_of_user_coordinate) ++ charToString '1' ++ charToString '0') `isIn` computer_ships_coordinates
              || (charToString (head coordinate_top_of_user_coordinate) ++ charToString '9') `isIn` computer_ships_coordinates
              then do
                putStrLn "Попал\n"
              else putStrLn "Убил\n"
          else -- Проверяем находится ли координата пользователя на координатах A1 - A10

            if user_coordinate_letter == 'A'
              then -- Проверяем наличие координат, ближайших к координате пользователя

                if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
                  || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
                  then putStrLn "Попал\n"
                  else putStrLn "Убил\n"
              else -- Проверяем находится ли координата пользователя на координатах J1 - J10

                if user_coordinate_letter == 'J'
                  then -- Проверяем наличие координат, ближайших к координате пользователя

                    if coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
                      || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
                      || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
                      then putStrLn "Попал\n"
                      else putStrLn "Убил\n"
                  else -- Если координата пользователя не находится на периметре

                    if coordinate_right_of_user_coordinate `isIn` computer_ships_coordinates
                      || coordinate_left_of_user_coordinate `isIn` computer_ships_coordinates
                      || coordinate_top_of_user_coordinate `isIn` computer_ships_coordinates
                      || coordinate_bottom_of_user_coordinate `isIn` computer_ships_coordinates
                      then putStrLn "Попал\n"
                      else putStrLn "Убил\n"

-- Если компьютер проиграл
getUserHit [] coordinate_index_for_computer_hit user_coordinates = do
  putStrLn "Вы выиграли"
  exitFailure

-- Если компьютер еще не проиграл
getUserHit computer_ships_coordinates coordinate_index_for_computer_hit user_coordinates = do
  putStrLn "Ваш ход: "
  user_coordinate <- getLine
  -- Если пользователь попал
  if user_coordinate `isIn` computer_ships_coordinates
    then do
      let -- Удаляем из коордиант кораблей компьютера координату, введенную пользователем
          computer_ships_coordinates_after_user_hit = delete user_coordinate computer_ships_coordinates

      isUserKillOrOnlyHitShip computer_ships_coordinates_after_user_hit user_coordinate

      getUserHit computer_ships_coordinates_after_user_hit coordinate_index_for_computer_hit user_coordinates
    else -- Если пользователь не попал
    do
      putStrLn "Не попал\n"
      getComputerHit coordinate_index_for_computer_hit user_coordinates
      getUserHit computer_ships_coordinates (coordinate_index_for_computer_hit + 1) user_coordinates

main = do
  let computerShipsCoordinates = ["C3", "D3", "E3", "F3", "I3", "I4", "I5", "B9", "C9", "D9", "B7", "C7", "E5", "F5", "I8", "I9", "B5", "G8", "G10", "H1"]
  putStrLn "Введите ваши координаты через пробел:"
  user_coordinates <- getLine
  getUserHit computerShipsCoordinates 0 (words user_coordinates)
