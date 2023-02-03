module Board where  -- do NOT CHANGE export of module
import Data.Char (isDigit)

-- IMPORTS HERE


-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars

import Data.List.Split
import Control.Arrow (ArrowChoice(left))

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Implementation Points              ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool   -- Gültigkeit der Figuren wird hier nicht gecheckt, gemäß ISISForum
validateFEN a = (checkerKommaSlash a == ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,") 
 && checkNum (checkerRest a) && checkPiece ( splitOn "/" a )
    where
        checkerKommaSlash :: String -> String
        checkerKommaSlash x = filter(\x-> x =='/' || x== ',') x


        checkerRest :: String -> String             -- filtert spielbrett nur noch auf zahlen runter, wenn nicht, dann false 
        checkerRest x = filter(\x-> x /='/' && x/= ',' && x/='b' && x/='w') x

        checkNum :: String -> Bool
        checkNum = all isDigit

        checkPiece [] = True
        checkPiece (rowString : otherRowStrings) =  checkValidFenRow  rowString && checkPiece otherRowStrings
      
-- let intVal = read xs in inVal > 0 && inVal < 256

checkValidFenSquare :: String -> Bool 
checkValidFenSquare "" = True 
checkValidFenSquare (x:xs) = ( x == 'b' || x== 'w' ) && intVal  > 0 && intVal  < 256
        where  intVal = read xs 

checkValidFenRow :: String -> Bool
checkValidFenRow a = g (splitOn "," a)
          where g [] = True
                g (cellString : otherCellStrings) =  checkValidFenSquare cellString && g otherCellStrings
        


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

-- typedef Board [[Cell]]

buildBoard :: String -> Board
buildBoard a =  ( g [] (splitOn "/" a ) )
        where 
          g rows [] = rows 
          g rows (rowString: otherRowStrings) = buildRow rowString : g rows otherRowStrings 


buildRow :: String -> [Cell]
buildRow a = reverse ( g [] (splitOn "," a ) )
        where g cells [] = reverse (cells)
              g cells (cellString : otherCellStrings) = buildCell cellString : g cells otherCellStrings 

buildCell :: String -> Cell 
buildCell "" = Empty 
buildCell (a:as) =  if a == 'b' then Piece Black intVal else Piece White intVal 
        where  intVal = read as 
 
-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Implementation Points    ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

-- succ 'a' = 'b'
-- succ 'c' = 'd'

line :: Pos -> Pos -> [Pos]
line (Pos c1 r1) (Pos c2 r2)
  | r1 == r2 && c1 ==c2 = (Pos c1 r1):[]        --Rekursionsanker
  | r1 == r2 && c1 < c2 = (Pos c1 r1) : line (Pos (succ c1) r1 ) (Pos c2 r2) -- Zug nach rechts
  | r1 == r2 && c1 > c2 = (Pos c1 r1) : line (Pos (pred c1) r1 ) (Pos c2 r2 ) -- Zug nach links 
  | r1 < r2 && c1 < c2 = (Pos c1 r1) : line (Pos (succ c1) (r1+1)) (Pos c2 r2) -- oben rechts 
  | r1 < r2 && c1 > c2 = (Pos c1 r1) : line(Pos (pred c1)(r1+1)) (Pos c2 r2)   --oben links
  | r1 < r2 && c1 == c2 = (Pos c1 r1): line (Pos (c1) (r1+1)) (Pos c2 r2) -- oben
  | r1 > r2 && c1 == c2 = (Pos c1 r1): line (Pos (c1) (r1-1)) (Pos c2 r2) -- unten 
  | r1 > r2 && c1 < c2 = (Pos c1 r1): line (Pos (succ c1) (r1-1)) (Pos c2 r2)   -- unten rechts 
  | r1 > r2 && c1 > c2 = (Pos c1 r1 ): line (Pos (pred c1) (r1-1)) (Pos c2 r2)  -- unten links 
  | otherwise = []   
