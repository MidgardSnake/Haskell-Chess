{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift, testBit, popCount )
import Data.Char



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished [] = False
gameFinished (a:b:c:d:e:f:g:h:i:rest) =
  --check only one King and random Figures 
  not( -- check for only Black Piece
        findCommanders (Piece White (170)) a
    ||  findCommanders (Piece White 170) b
    ||  findCommanders (Piece White 170) c
    ||  findCommanders (Piece White 170) d
    ||  findCommanders (Piece White 170) e
    ||  findCommanders (Piece White 170) f
    ||  findCommanders (Piece White 170) g
    ||  findCommanders (Piece White 170) h
    ||  findCommanders (Piece White 170) i
    ) &&
        (findCommanders (Piece Black 170) a
    ||  findCommanders (Piece Black 170) b
    ||  findCommanders (Piece Black 170) c
    ||  findCommanders (Piece Black 170) d
    ||  findCommanders (Piece Black 170) e
    ||  findCommanders (Piece Black 170) f
    ||  findCommanders (Piece Black 170) g
    ||  findCommanders (Piece Black 170) h
    ||  findCommanders (Piece Black 170) i
    ) || -- check for ony white Piece

        findCommanders (Piece White 170) a
    ||  findCommanders (Piece White 170) b
    ||  findCommanders (Piece White 170) c
    ||  findCommanders (Piece White 170) d
    ||  findCommanders (Piece White 170) e
    ||  findCommanders (Piece White 170) f
    ||  findCommanders (Piece White 170) g
    ||  findCommanders (Piece White 170) h
    ||  findCommanders (Piece White 170) i
     &&
       not (findCommanders (Piece Black 170) a
    ||   findCommanders (Piece Black 170) b
    ||   findCommanders (Piece Black 170) c
    ||   findCommanders (Piece Black 170) d
    ||   findCommanders (Piece Black 170) e
    ||   findCommanders (Piece Black 170) f
    ||   findCommanders (Piece Black 170) g
    ||   findCommanders (Piece Black 170) h
    ||   findCommanders (Piece Black 170) i )

    || -- check also diagonal commanders

    not( -- check for only Black Piece
        findCommanders (Piece White (85)) a
    ||  findCommanders (Piece White 85) b
    ||  findCommanders (Piece White 85) c
    ||  findCommanders (Piece White 85) d
    ||  findCommanders (Piece White 85) e
    ||  findCommanders (Piece White 85) f
    ||  findCommanders (Piece White 85) g
    ||  findCommanders (Piece White 85) h
    ||  findCommanders (Piece White 85) i
    ) &&
        (findCommanders (Piece Black 85) a
    ||  findCommanders (Piece Black 85) b
    ||  findCommanders (Piece Black 85) c
    ||  findCommanders (Piece Black 85) d
    ||  findCommanders (Piece Black 85) e
    ||  findCommanders (Piece Black 85) f
    ||  findCommanders (Piece Black 85) g
    ||  findCommanders (Piece Black 85) h
    ||  findCommanders (Piece Black 85) i
    ) || -- check for ony white Piece

        findCommanders (Piece White 85) a
    ||  findCommanders (Piece White 85) b
    ||  findCommanders (Piece White 85) c
    ||  findCommanders (Piece White 85) d
    ||  findCommanders (Piece White 85) e
    ||  findCommanders (Piece White 85) f
    ||  findCommanders (Piece White 85) g
    ||  findCommanders (Piece White 85) h
    ||  findCommanders (Piece White 85) i
     &&
       not (
         findCommanders (Piece Black 85) a
    ||   findCommanders (Piece Black 85) b
    ||   findCommanders (Piece Black 85) c
    ||   findCommanders (Piece Black 85) d
    ||   findCommanders (Piece Black 85) e
    ||   findCommanders (Piece Black 85) f
    ||   findCommanders (Piece Black 85) g
    ||   findCommanders (Piece Black 85) h
    ||   findCommanders (Piece Black 85) i )

      where
        findCommanders :: Cell -> [Cell] -> Bool
        findCommanders _ [] = False
        findCommanders n (x:xs)
          | x == n = True
          | otherwise = findCommanders n xs
-- Check for Only Kings           
gameFinished a = checkBoard a
  where
        checkForOnlyKings:: [Cell] -> Bool
        checkForOnlyKings [] = True
        checkForOnlyKings (a : as)
          | checkForEmpty a = checkForOnlyKings as
          | checkForAnyKing a = True
          | otherwise = False

        checkForEmpty:: Cell -> Bool
        checkForEmpty a =
            case a of
                Empty -> True
                _ -> False

        checkForAnyKing :: Cell -> Bool
        checkForAnyKing a =
            case a of
                Piece White 170 -> True
                Piece Black 170 -> True
                _ -> False

        checkBoard :: Board -> Bool
        checkBoard [] = True
        checkBoard (a:as) = ((checkForOnlyKings a)) && checkBoard as




-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Implementation Points            ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove board move = isRotation move || ( areAllSquaresBetweenStartAndTargetEmpty board move 
									&& isCellWhite ( getCell board (start move) ) && isCellBlackOrEmpty ( getCell board (target move )) )
									|| ( areAllSquaresBetweenStartAndTargetEmpty board move 
									&& isCellBlack ( getCell board (start move) ) && isCellWhiteOrEmpty ( getCell board (target move )) )  

isCellBlackOrEmpty:: Cell-> Bool 
isCellBlackOrEmpty a = 
  case a of
    Piece Black _ -> True 
    Empty -> True 
    _ -> False 

isCellWhiteOrEmpty:: Cell-> Bool 
isCellWhiteOrEmpty a = 
  case a of
    Piece White _ -> True 
    Empty -> True 
    _ -> False 

all1 :: (a->Bool) -> [a] -> Bool
all1 p [] = True
all1  p (x:xs) = p x && all1 p xs

areAllSquaresBetweenStartAndTargetEmpty :: Board -> Move -> Bool
areAllSquaresBetweenStartAndTargetEmpty board move = all1 (\pos -> getCell board pos == Empty ) ( removeFirstAndLast (line (start move) (target move)) )

-- typedef Board = [[Cell]]
getCell :: Board -> Pos -> Cell
getCell board (Pos c r ) = ( board!!(9-r) )!!(ord c - ord 'a')

isCellBlack :: Cell -> Bool
isCellBlack a = 
  case a of 
    Piece Black _ -> True 
    _ -> False 

isCellWhite :: Cell -> Bool
isCellWhite a = 
  case a of 
    Piece White _ -> True 
    _ -> False 

isCellEmpty :: Cell -> Bool
isCellEmpty a = 
  case a of 
    Empty -> True
    _ -> False  

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast [] = []
removeFirstAndLast (a:as) = removeLast as

removeLast :: [a] -> [a]
removeLast (x:[]) = []
removeLast (x:xs) = x : removeLast xs 

isRotation:: Move-> Bool
isRotation (Move startPos targetPos turn) =  turn >=1 && turn <=7  && startPos == targetPos


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Implementation Points              ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

--($) :: (a->b) -> a -> b
--f $ x = f x

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ Empty = []
possibleMoves pos (Piece p turn) = filterToLongMoves  (possibleRotations pos (Piece p turn ) ++ if popCount turn == 1  then addShieldRotations movesWithoutRotation else movesWithoutRotation)


 where 
      movesWithoutRotation = (if (givePos 0 (toBinFiller (toBin (turn))))  then (possibleMovesUp pos)  else [])
                          ++ (if givePos 1 (toBinFiller (toBin (turn))) then possibleMovesUpRight pos else [])
                          ++ (if givePos 2 (toBinFiller (toBin (turn))) then  possibleMovesRight pos else [])
                          ++ (if givePos 3 (toBinFiller (toBin (turn))) then  possibleMovesDownRight pos else [])
                          ++ (if givePos 4 (toBinFiller (toBin (turn))) then  possibleMovesDown pos else [])
                          ++ (if givePos 5 (toBinFiller (toBin (turn))) then  possibleMovesDownLeft pos else [])
                          ++ (if givePos 6 (toBinFiller (toBin (turn))) then  possibleMovesLeft pos else [])
                          ++ (if givePos 7 (toBinFiller (toBin (turn))) then  (possibleMovesUpLeft pos) else [])


      addShieldRotations :: [Move] -> [Move]
      addShieldRotations [] = []
      addShieldRotations moves = concat $ map (\move -> map (\t -> (Move (start move) (target move) t) )  [0..7]) moves
                          

      possibleMovesUp :: Pos -> [Move]
      possibleMovesUp (Pos _ 9) = []
      possibleMovesUp ( Pos c r) = (Move pos (Pos c (r+1)) 0) : possibleMovesUp (Pos c (r+1) )

      possibleMovesUpRight :: Pos -> [Move] 
      possibleMovesUpRight (Pos 'i' _) = []
      possibleMovesUpRight ( Pos _ 9 ) = []
      possibleMovesUpRight (Pos c r )= (Move pos (Pos (succ c) (r+1) ) 0 ) : possibleMovesUpRight (Pos (succ c) (r+1))

      possibleMovesRight :: Pos -> [Move]
      possibleMovesRight (Pos 'i' _) = []
      possibleMovesRight (Pos c r ) = (Move pos (Pos (succ c) (r) ) 0 ) : possibleMovesRight (Pos (succ c) (r))

      possibleMovesDownRight :: Pos -> [Move]
      possibleMovesDownRight (Pos 'i' _ ) = []
      possibleMovesDownRight (Pos _  1 ) = []
      possibleMovesDownRight (Pos c r )= (Move pos (Pos (succ c) (r-1) ) 0 ) : possibleMovesDownRight (Pos (succ c) (r-1))

      possibleMovesDown :: Pos -> [Move]
      possibleMovesDown (Pos _ 1) = []
      possibleMovesDown (Pos c r ) = (Move pos (Pos (c) (r-1) ) 0 ) : possibleMovesDown (Pos (c) (r-1))

      possibleMovesDownLeft :: Pos -> [Move]
      possibleMovesDownLeft (Pos 'a' _ ) = []
      possibleMovesDownLeft (Pos _  1 ) = []
      possibleMovesDownLeft (Pos c r )= (Move pos (Pos (pred c) (r-1) ) 0 ) : possibleMovesDownLeft (Pos (pred c) (r-1))

      possibleMovesLeft :: Pos -> [Move]
      possibleMovesLeft (Pos 'a' _) = []
      possibleMovesLeft (Pos c r ) = (Move pos (Pos (pred c) (r) ) 0 ) : possibleMovesLeft (Pos (pred c) (r))

      possibleMovesUpLeft :: Pos -> [Move] 
      possibleMovesUpLeft (Pos 'a' _) = []
      possibleMovesUpLeft ( Pos _ 9 ) = []
      possibleMovesUpLeft (Pos c r )= (Move pos (Pos (pred c) (r+1) ) 0 ) : possibleMovesUpLeft (Pos (pred c) (r+1))


      maxStepsSize :: Int
      maxStepsSize | (popCount turn == 1 || popCount turn == 4) = 1  --popCount zähltBits  , Shield und COmmander 
                   | popCount turn == 2 = 2 -- Probe 
                   | otherwise = 3 -- Lance

      filterToLongMoves :: [Move] -> [Move] -- zieht alle überdrüssigen moves raus 
      filterToLongMoves [] = []
      filterToLongMoves moves = filter (\(Move (Pos c1 r1) (Pos c2 r2) _ ) -> abs (r2 - r1) <= maxStepsSize && abs (ord c1 - ord c2) <= maxStepsSize  ) moves

      toBin :: Int -> [Int] 
      toBin 0 = [0]
      toBin n = reverse (helper n)

      helper 0 = []
      helper args | args == 0 = 0:helper(args)
                  | args `mod` 2 == 1 = 1 : helper(args `div` 2)
                  | args `mod` 2 == 0 = 0 : helper(args `div` 2)
      
      toBinFiller :: [Int] -> [Int]
      toBinFiller (q:qs) | length (q:qs) < 8 = toBinFiller(0:(q:qs))
                         | length (q:qs) == 8 = reverse(q:qs)
      
      givePos :: Int -> [Int ]-> Bool 
      givePos 0 (a) = if ((a!!0)==1) then True else False 
      givePos 1 (a) = if ((a!!1)==1) then True else False 
      givePos 2 (a) = if ((a!!2)==1) then True else False 
      givePos 3 (a) = if ((a!!3)==1) then True else False 
      givePos 4 (a) = if ((a!!4)==1) then True else False 
      givePos 5 (a) = if ((a!!5)==1) then True else False 
      givePos 6 (a) = if ((a!!6)==1) then True else False 
      givePos 7 (a) = if ((a!!7)==1) then True else False 

addList1:: [Move] -> [Move]
addList1 [] = []
addList1 (a:as) = a : addList1 (as)
         
         
toBin :: Int -> [Int] 
toBin 0 = [0]
toBin n = reverse (helper n)
helper 0 = []
      
helper args 
      | args == 0 = 0:helper(args)
      | args `mod` 2 == 1 = 1 : helper(args `div` 2)
      | args `mod` 2 == 0 = 0 : helper(args `div` 2)
      
toBinFiller :: [Int] -> [Int]
toBinFiller (q:qs) | length (q:qs) < 8 = toBinFiller(0:(q:qs))
                         | length (q:qs) == 8 = reverse(q:qs)

givePos :: Int -> [Int ]-> Bool 
givePos 0 (a) = if ((a!!0)==1) then True else False 
givePos 1 (a) = if ((a!!1)==1) then True else False 
givePos 2 (a) = if ((a!!2)==1) then True else False 
givePos 3 (a) = if ((a!!3)==1) then True else False 
givePos 4 (a) = if ((a!!4)==1) then True else False 
givePos 5 (a) = if ((a!!5)==1) then True else False 
givePos 6 (a) = if ((a!!6)==1) then True else False 
givePos 7 (a) = if ((a!!7)==1) then True else False 



possibleRotations :: Pos -> Cell -> [Move]
possibleRotations pos (Piece _ turn) | turn == 170 || turn == 85 = [ (Move pos pos 1) ] 
                                     | turn == 17 || turn == 34 || turn ==  68 || turn == 136 = map (\rot -> Move pos pos rot) [1..3]
                                     | otherwise = g pos 1

  where 
    g:: Pos -> Int -> [Move]
    g pos 8 = []
    g pos turn = (Move pos pos turn) : g pos (turn+1)



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Implementation Points                        ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player = []