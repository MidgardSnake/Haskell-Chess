

        -- #############################################################################
-- ########## GRADING TESTS                                        #############
-- ########## Note: execute tests using "stack test ploy:grading   #############
-- #############################################################################

import Test.Hspec

import Board
    ( buildBoard,
      line,
      validateFEN,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos) )
import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    gradeValidateFEN
    gradeBuildBoard
    gradeLine
    gradeGameFinished
    gradeIsValidMove
    gradePossibleMoves
    gradeListMoves

-- Common boards
startFen = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"
startBoard = [[Empty,(Piece White 84),(Piece White 41),(Piece White 56),(Piece White 170),(Piece White 56),(Piece White 41),(Piece White 84),Empty],[Empty,Empty,(Piece White 24),(Piece White 40),(Piece White 17),(Piece White 40),(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 16),(Piece White 16),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 1),(Piece Black 1),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,(Piece Black 3),(Piece Black 130),(Piece Black 17),(Piece Black 130),(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),(Piece Black 146),(Piece Black 131),(Piece Black 170),(Piece Black 131),(Piece Black 146),(Piece Black 69),Empty]]

sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]


gradeValidateFEN :: Spec
gradeValidateFEN =
    describe "IF Grade validateFEN 2FP" $ do
        it "fen has not 9 rows" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
        it "fen has not 9 columns" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,," `shouldBe`(False :: Bool)
        it "empty board is valid" $ do 
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(True :: Bool)
        it "valid piece black" $ do
            validateFEN ",,,,,,,,/,,,,w1,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(True :: Bool)
        it "valid piece white" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b13,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(True :: Bool)
        it "only color invalid" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(False :: Bool)
        it "red player does not exist" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,r123,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(False :: Bool)
        it "orientation of piece is 0" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,b0,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(False :: Bool)
        it "orientation of piece is larger than 255" $ do
            validateFEN "w355,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe`(False :: Bool)
        it "strange string for piece not allowed" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,wabc" `shouldBe`(False :: Bool)
        it "start board is valid" $ do
            validateFEN startFen `shouldBe` (True :: Bool)

gradeBuildBoard :: Spec
gradeBuildBoard = describe "IF Grade buildBoard 2FP" $ do
        it "build empty board" $ do
            buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "board contains empty" $ do
            (boardContains (buildBoard "w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,w1/w1,w1,w1,w1,w1,w1,w1,w1,") Empty) `shouldBe` (True :: Bool)
        it "build single piece white player" $ do
            (boardContains (buildBoard ",,,,,,,,/,,,,,,,,/,,,w3,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,") (Piece White 3)) `shouldBe` (True :: Bool)
        it "build single piece black player" $ do
            (boardContains (buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,b232,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,") (Piece Black 232)) `shouldBe` (True :: Bool)
        it "build start board" $ do
            (buildBoard startFen) `shouldBe` (startBoard :: Board)
        it "build other board" $ do
            (buildBoard ",w84,w41,,,w56,,w84,/,,,,,b130,,,/,,,,,w56,,,/,,,,,,w48,,/,,,w16,,,,,/,,,,b17,,,,/b146,,b3,,b130,,,,b146/,,,,,,b129,,/,b69,,b131,b170,b131,,b69,") `shouldBe` [[Empty,(Piece White 84),(Piece White 41),Empty,Empty,(Piece White 56),Empty,(Piece White 84),Empty],[Empty,Empty,Empty,Empty,Empty,(Piece Black 130),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,(Piece White 56),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 16),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 17),Empty,Empty,Empty,Empty],[(Piece Black 146),Empty,(Piece Black 3),Empty,(Piece Black 130),Empty,Empty,Empty,(Piece Black 146)],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece Black 131),(Piece Black 170),(Piece Black 131),Empty,(Piece Black 69),Empty]]

gradeLine :: Spec
gradeLine = describe "IF Grade line 3FP" $ do
        it "start is target"$ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "contains target" $ do
            line (Pos 'g' 6) (Pos 'g' 4) `shouldContain` ([(Pos 'g' 4)])
        it "contains start" $ do
            line (Pos 'a' 3) (Pos 'c' 3) `shouldContain` ([(Pos 'a' 3)])
        it "contains in between" $ do
            line (Pos 'd' 1) (Pos 'a' 4) `shouldContain` ([(Pos 'c' 2)])
        it "diagonal right up" $ do
            line (Pos 'c' 3) (Pos 'e' 5) `shouldMatchList` ([(Pos 'c' 3), (Pos 'd' 4), (Pos 'e' 5)])
        it "diagonal right down" $ do
            line (Pos 'a' 9) (Pos 'i' 1) `shouldBe` ([(Pos 'a' 9), (Pos 'b' 8), (Pos 'c' 7), (Pos 'd' 6), (Pos 'e' 5), (Pos 'f' 4), (Pos 'g' 3), (Pos 'h' 2), (Pos 'i' 1)])
        it "diagonal left up" $ do
            line (Pos 'c' 1) (Pos 'b' 2) `shouldMatchList` ([(Pos 'c' 1), (Pos 'b' 2)])
        it "diagonal left down" $ do
            line (Pos 'd' 8) (Pos 'a' 5) `shouldBe` ([(Pos 'd' 8), (Pos 'c' 7), (Pos 'b' 6), (Pos 'a' 5)])
        it "vertical up" $ do
            line (Pos 'd' 3) (Pos 'd' 6) `shouldMatchList` ([(Pos 'd' 3), (Pos 'd' 4), (Pos 'd' 5), (Pos 'd' 6)])
        it "vertical down" $ do
            line (Pos 'h' 9) (Pos 'h' 1) `shouldBe` ([(Pos 'h' 9), (Pos 'h' 8), (Pos 'h' 7), (Pos 'h' 6), (Pos 'h' 5), (Pos 'h' 4), (Pos 'h' 3), (Pos 'h' 2), (Pos 'h' 1)])
        it "horizontal left" $ do
            line (Pos 'e' 5) (Pos 'a' 5) `shouldMatchList` ([(Pos 'e' 5), (Pos 'd' 5), (Pos 'c' 5), (Pos 'b' 5), (Pos 'a' 5)])
        it "horizontal right" $ do
            line (Pos 'f' 1) (Pos 'h' 1) `shouldBe` ([(Pos 'f' 1), (Pos 'g' 1), (Pos 'h' 1)])

gradeGameFinished :: Spec
gradeGameFinished = describe "IF Grade gameFinished 3FP" $ do
        it "start board not finished" $ do
            gameFinished startBoard `shouldBe` (False :: Bool)
        it "sample board finished" $ do
            gameFinished sampleBoard `shouldBe` (True :: Bool)
        it "almost finished" $ do
            let board = [[Empty,Empty,Empty,(Piece Black 6),(Piece White 170),(Piece Black 64),(Piece Black 129),(Piece Black 131),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[(Piece Black 130),(Piece Black 131),Empty,Empty,(Piece Black 4),(Piece White 136),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 146),Empty,(Piece Black 170),Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 20),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 146)],[Empty,Empty,Empty,Empty,(Piece Black 17),Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,Empty,Empty,Empty,Empty,(Piece Black 69),Empty]] in
                gameFinished board `shouldBe` (False :: Bool)
        it "white no commander" $ do
            let board = [[Empty,Empty,(Piece White 164),Empty,Empty,(Piece White 56),Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 68),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 40),(Piece White 16),(Piece White 82),Empty,(Piece Black 130)],[Empty,(Piece White 81),(Piece Black 131),(Piece Black 40),Empty,Empty,(Piece White 168),Empty,Empty],[Empty,Empty,Empty,Empty,Empty,(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,(Piece White 96),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),(Piece Black 148),Empty,(Piece Black 170),Empty,(Piece Black 146),(Piece Black 84),Empty]] in
                gameFinished board `shouldBe` (True :: Bool)
        it "black no commander" $ do
            let board = [[Empty,Empty,Empty,Empty,(Piece White 170),Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 224),Empty,(Piece White 96),Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 41),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece White 24),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 84),Empty],[Empty,Empty,Empty,(Piece White 8),Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 21),Empty,(Piece Black 7),(Piece White 32),(Piece Black 224),Empty,Empty,Empty]] in
                gameFinished board `shouldBe` (True :: Bool)
        it "white only commander" $ do
            let board = [[Empty,Empty,(Piece Black 4),Empty,(Piece White 170),Empty,Empty,(Piece Black 8),Empty],[Empty,Empty,Empty,(Piece Black 74),Empty,(Piece Black 193),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 17),Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 21),Empty],[Empty,Empty,(Piece Black 130),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty,Empty,Empty]] in
                gameFinished board `shouldBe` (True :: Bool)
        it "black only commmaner" $ do
            let board = [[Empty,Empty,Empty,Empty,(Piece White 170),Empty,Empty,Empty,Empty],[Empty,(Piece White 164),Empty,(Piece White 56),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 56),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 34),Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,(Piece White 41),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 168),Empty],[Empty,Empty,Empty,Empty,(Piece White 16),Empty,Empty,Empty,Empty],[Empty,Empty,(Piece White 64),Empty,(Piece Black 85),Empty,Empty,(Piece White 4),Empty]] in
                gameFinished board `shouldBe` (True :: Bool)
        it "both commanders turned" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),(Piece White 56),(Piece White 85),(Piece White 56),(Piece White 41),(Piece White 84),Empty],[Empty,Empty,(Piece White 24),(Piece White 40),(Piece White 17),(Piece White 40),(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 16),(Piece White 16),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 1),(Piece Black 1),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,(Piece Black 3),(Piece Black 130),(Piece Black 17),(Piece Black 130),(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),(Piece Black 146),(Piece Black 131),(Piece Black 85),(Piece Black 131),(Piece Black 146),(Piece Black 69),Empty]] in
                gameFinished board `shouldBe` (False :: Bool)

gradeIsValidMove :: Spec
gradeIsValidMove = describe "IF Grade isValidMove 5FP" $ do
        it "rotation by 1 is possible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)
        it "try jump over white" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),Empty,(Piece White 170),Empty,Empty,(Piece White 84),Empty],[Empty,Empty,(Piece Black 21),Empty,(Piece White 17),(Piece White 40),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 17),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,(Piece White 56),(Piece White 40),Empty,Empty,Empty,(Piece White 41),Empty],[Empty,(Piece White 56),Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 48),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 131),Empty,(Piece Black 1),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 16),(Piece Black 170),Empty,(Piece Black 146),Empty,Empty]] in
                isValidMove board (Move (Pos 'c' 3) (Pos 'e' 5) 0) `shouldBe` (False :: Bool)
        it "try jump over black" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),Empty,(Piece White 170),Empty,Empty,(Piece White 84),Empty],[Empty,Empty,(Piece Black 21),Empty,(Piece White 17),(Piece White 40),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 17),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,(Piece White 56),(Piece White 40),Empty,Empty,Empty,(Piece White 41),Empty],[Empty,(Piece White 56),Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 48),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 131),Empty,(Piece Black 1),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 16),(Piece Black 170),Empty,(Piece Black 146),Empty,Empty]] in
                isValidMove board (Move (Pos 'e' 8) (Pos 'e' 6) 0) `shouldBe` (False :: Bool)
        it "black hits white" $ do
            let board = [[Empty,(Piece White 21),Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,(Piece White 168),(Piece White 170),Empty],[Empty,Empty,(Piece White 17),Empty,(Piece Black 17),(Piece White 64),Empty,Empty,Empty],[Empty,(Piece White 41),Empty,(Piece White 40),Empty,Empty,Empty,(Piece White 146),Empty],[Empty,(Piece White 56),Empty,(Piece Black 48),Empty,Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,Empty,(Piece Black 1),Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 131),Empty,Empty,(Piece Black 16),Empty,(Piece White 40),Empty],[Empty,Empty,(Piece Black 170),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 16),Empty,Empty,(Piece Black 146),Empty,Empty]] in
                isValidMove board (Move (Pos 'c' 8) (Pos 'c' 7) 0) `shouldBe` (True :: Bool)
        it "white hits black" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),(Piece White 56),(Piece White 170),(Piece White 56),(Piece White 41),(Piece White 84),Empty],[Empty,Empty,(Piece White 24),(Piece White 40),(Piece White 17),(Piece White 40),(Piece White 48),Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 16),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 16),(Piece Black 4),Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 3),(Piece Black 130),(Piece Black 17),(Piece Black 130),(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),(Piece Black 146),(Piece Black 131),(Piece Black 170),(Piece Black 131),(Piece Black 146),(Piece Black 69),Empty]] in
                isValidMove board (Move (Pos 'd' 3) (Pos 'd' 2) 0) `shouldBe` (True :: Bool)
        it "black tries jump over own and hits white" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 21),(Piece Black 68),(Piece White 17),Empty,Empty,(Piece White 170),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 1),Empty,(Piece White 40),Empty,Empty,Empty,(Piece White 146),Empty],[Empty,(Piece White 224),Empty,(Piece Black 48),Empty,Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 40),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 131),Empty,Empty,Empty,Empty,(Piece Black 1),Empty],[Empty,Empty,(Piece Black 37),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 64),Empty,Empty,Empty,(Piece Black 170),Empty]] in
                isValidMove board (Move (Pos 'c' 8) (Pos 'e' 8) 0) `shouldBe` (False :: Bool)
        it "white tries jump over black and hits black" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 21),(Piece Black 68),(Piece White 17),Empty,Empty,(Piece White 170),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 1),Empty,(Piece White 40),Empty,Empty,Empty,(Piece White 146),Empty],[Empty,(Piece White 224),Empty,(Piece Black 48),Empty,Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 40),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 131),Empty,Empty,Empty,Empty,(Piece Black 1),Empty],[Empty,Empty,(Piece Black 37),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 64),Empty,Empty,Empty,(Piece Black 170),Empty]] in
                isValidMove board (Move (Pos 'b' 8) (Pos 'd' 8) 0) `shouldBe` (False :: Bool)
        it "run into own piece black" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 68),Empty,(Piece White 17),Empty,Empty,(Piece White 170),Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece Black 1),Empty,Empty,Empty,Empty,Empty,(Piece White 146),Empty],[Empty,(Piece White 224),Empty,(Piece Black 48),Empty,Empty,(Piece White 48),Empty,Empty],[Empty,Empty,Empty,(Piece White 40),Empty,(Piece White 40),Empty,Empty,Empty],[(Piece Black 37),Empty,(Piece Black 131),Empty,Empty,Empty,Empty,(Piece Black 1),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 64),Empty,Empty,Empty,Empty,Empty]] in
                isValidMove board (Move (Pos 'a' 3) (Pos 'c' 3) 0) `shouldBe` (False :: Bool)
        it "run into own piece white" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 68),Empty,(Piece White 17),Empty,Empty,(Piece White 170),Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 146),Empty],[Empty,(Piece White 224),Empty,(Piece Black 48),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 40),(Piece White 40),Empty,(Piece White 48),Empty,Empty],[(Piece Black 37),Empty,(Piece Black 131),Empty,Empty,Empty,Empty,(Piece Black 1),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 64),Empty,Empty,Empty,Empty,Empty]] in
                isValidMove board (Move (Pos 'b' 8) (Pos 'b' 5) 0) `shouldBe` (False :: Bool)
        it "turn black shield" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 68),Empty,Empty,Empty,Empty,(Piece White 170),Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 146),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 34),(Piece White 40),Empty,Empty,Empty,Empty],[(Piece White 131),Empty,(Piece Black 48),Empty,Empty,Empty,Empty,(Piece Black 16),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 48),Empty,Empty,Empty,Empty,Empty]] in
                isValidMove board (Move (Pos 'h' 3) (Pos 'h' 3) 3) `shouldBe` (True :: Bool)
        it "turn white probe" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 68),Empty,Empty,Empty,Empty,(Piece White 170),Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 146),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 34),(Piece White 40),Empty,Empty,Empty,Empty],[(Piece White 131),Empty,(Piece Black 48),Empty,Empty,Empty,Empty,(Piece Black 16),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 48),Empty,Empty,Empty,Empty,Empty]] in
                isValidMove board (Move (Pos 'd' 4) (Pos 'd' 4) 5) `shouldBe` (True :: Bool)
        it "turn white lance" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,(Piece White 21),(Piece Black 68),Empty,Empty,Empty,Empty,(Piece White 170),Empty],[Empty,Empty,(Piece Black 21),Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece White 146),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 34),(Piece White 40),Empty,Empty,Empty,Empty],[(Piece White 131),Empty,(Piece Black 48),Empty,Empty,Empty,Empty,(Piece Black 16),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 170),Empty,Empty],[Empty,(Piece Black 69),Empty,(Piece White 48),Empty,Empty,Empty,Empty,Empty]] in
                isValidMove board (Move (Pos 'h' 6) (Pos 'h' 6) 1) `shouldBe` (True :: Bool)
        it "turn black commander" $ do
            let board = startBoard in
                isValidMove board (Move (Pos 'e' 1) (Pos 'e' 1) 3) `shouldBe` (True :: Bool)
        it "shield hits own and turns" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),(Piece White 56),(Piece White 170),(Piece White 56),(Piece White 41),(Piece White 84),Empty],[Empty,Empty,(Piece White 24),(Piece White 40),(Piece White 17),(Piece White 40),(Piece White 48),Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 16),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 16),(Piece Black 4),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,(Piece Black 3),(Piece Black 130),(Piece Black 17),(Piece Black 130),(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),(Piece Black 146),(Piece Black 131),(Piece Black 170),(Piece Black 131),(Piece Black 146),(Piece Black 69),Empty]] in
                isValidMove board (Move (Pos 'e' 3) (Pos 'f' 3) 7) `shouldBe` (False :: Bool)
        it "shield moves and turns" $ do
            let board = [[Empty,(Piece White 84),(Piece White 41),(Piece White 56),(Piece White 170),(Piece White 56),(Piece White 41),(Piece White 84),Empty],[Empty,Empty,(Piece White 24),(Piece White 40),(Piece White 17),(Piece White 40),(Piece White 48),Empty,Empty],[Empty,Empty,Empty,Empty,(Piece White 16),(Piece White 16),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece White 16),(Piece Black 4),(Piece Black 1),Empty,Empty,Empty],[Empty,Empty,(Piece Black 3),(Piece Black 130),(Piece Black 17),(Piece Black 130),(Piece Black 129),Empty,Empty],[Empty,(Piece Black 69),(Piece Black 146),(Piece Black 131),(Piece Black 170),(Piece Black 131),(Piece Black 146),(Piece Black 69),Empty]] in
                isValidMove board (Move (Pos 'f' 7) (Pos 'f' 6) 4) `shouldBe` (True :: Bool)


gradePossibleMoves :: Spec
gradePossibleMoves = describe "IF Grade possibleMoves 6FP" $ do
        -- shield
        it "shield rotation allowed" $ do
            possibleMoves (Pos 'g' 6) (Piece Black 4) `shouldContain` ([Move (Pos 'g' 6) (Pos 'g' 6) 3] :: [Move])
        it "shield move allowed" $ do
            possibleMoves (Pos 'd' 3) (Piece White 1) `shouldContain` ([Move (Pos 'd' 3) (Pos 'd' 4) 0] :: [Move])
        it "shield move out of board" $ do
            possibleMoves (Pos 'h' 1) (Piece Black 16) `shouldNotContain` ([Move (Pos 'h' 1) (Pos 'h' 0) 0] :: [Move])
        it "shield move wrong direction" $ do
            possibleMoves (Pos 'f' 8) (Piece White 32) `shouldNotContain` ([Move (Pos 'f' 8) (Pos 'g' 9) 0] :: [Move])
        it "shield rotate and move allowed" $ do
            possibleMoves (Pos 'a' 1) (Piece Black 4) `shouldContain` ([Move (Pos 'a' 1) (Pos 'b' 1) 7] :: [Move])
        it "shield completeness" $ do
            let correct = ["a8-a8-1","a8-a8-2","a8-a8-3","a8-a8-4","a8-a8-5","a8-a8-6","a8-a8-7","a8-b7-0","a8-b7-1","a8-b7-2","a8-b7-3","a8-b7-4","a8-b7-5","a8-b7-6","a8-b7-7"] in
                map show (possibleMoves (Pos 'a' 8) (Piece White 8)) `shouldMatchList` correct
        -- probe
        it "probe rotation allowed" $ do
            possibleMoves (Pos 'g' 2) (Piece White 129) `shouldContain` ([Move (Pos 'g' 2) (Pos 'g' 2) 2] :: [Move])
        it "probe move allowed" $ do
            possibleMoves (Pos 'd' 8) (Piece Black 65) `shouldContain` ([Move (Pos 'd' 8) (Pos 'b' 8) 0] :: [Move])
        it "probe move out of board" $ do
            possibleMoves (Pos 'd' 9) (Piece White 34) `shouldNotContain` ([Move (Pos 'd' 9) (Pos 'e' 10) 0] :: [Move])
        it "probe move wrong direction" $ do
            possibleMoves (Pos 'd' 5) (Piece Black 96) `shouldNotContain` ([Move (Pos 'd' 5) (Pos 'd' 4) 0] :: [Move])
        it "probe rotate and move NOT allowed" $ do
            possibleMoves (Pos 'd' 2) (Piece Black 3) `shouldNotContain` ([Move (Pos 'd' 2) (Pos 'd' 3) 1] :: [Move])
        it "probe completeness" $ do
            let correct = ["g7-g7-1","g7-g7-2","g7-g7-3","g7-g7-4","g7-g7-5","g7-g7-6","g7-g7-7","g7-f6-0","g7-e5-0","g7-f7-0","g7-e7-0"] in
                map show (possibleMoves (Pos 'g' 7) (Piece White 96)) `shouldMatchList` correct
        it "probe state does not change forbidded" $ do
            possibleMoves (Pos 'a' 5) (Piece Black 68) `shouldNotContain` ([Move (Pos 'a' 5) (Pos 'a' 5) 4] :: [Move])
        -- lance
        it "lance rotation allowed" $ do
            possibleMoves (Pos 'i' 6) (Piece Black 41) `shouldContain` ([Move (Pos 'i' 6) (Pos 'i' 6) 3] :: [Move])
        it "lance move allowed" $ do
            possibleMoves (Pos 'c' 6) (Piece White 7) `shouldContain` ([Move (Pos 'c' 6) (Pos 'f' 9) 0] :: [Move])
        it "lance move more than 3 steps" $ do
            possibleMoves (Pos 'e' 3) (Piece White 84) `shouldNotContain` ([Move (Pos 'e' 3) (Pos 'a' 3) 0] :: [Move])
        it "lance move wrong direction" $ do
            possibleMoves (Pos 'g' 3) (Piece Black 146) `shouldNotContain` ([Move (Pos 'g' 3) (Pos 'f' 2) 0] :: [Move])
        it "lance rotate and move NOT allowed" $ do
            possibleMoves (Pos 'c' 3) (Piece Black 56) `shouldNotContain` ([Move (Pos 'c' 3) (Pos 'c' 2) 2] :: [Move])
        it "lance completeness" $ do
            let correct = ["b8-b8-1","b8-b8-2","b8-b8-3","b8-b8-4","b8-b8-5","b8-b8-6","b8-b8-7","b8-c7-0","b8-d6-0","b8-e5-0","b8-a7-0","b8-a9-0"] in
                map show (possibleMoves (Pos 'b' 8) (Piece White 168)) `shouldMatchList` correct
        -- commander
        it "commander rotation allowed" $ do
            possibleMoves (Pos 'a' 9) (Piece Black 170) `shouldContain` ([Move (Pos 'a' 9) (Pos 'a' 9) 7] :: [Move])
        it "commander move allowed" $ do
            possibleMoves (Pos 'i' 8) (Piece Black 85) `shouldContain` ([Move (Pos 'i' 8) (Pos 'i' 7) 0] :: [Move])
        it "commander move wrong direction" $ do
            possibleMoves (Pos 'e' 9) (Piece White 170) `shouldNotContain` ([Move (Pos 'e' 9) (Pos 'e' 8) 0] :: [Move])
        it "commander rotate and move NOT allowed" $ do
            possibleMoves (Pos 'i' 8) (Piece Black 85) `shouldNotContain` ([Move (Pos 'i' 8) (Pos 'i' 7) 1] :: [Move])
        it "commander completeness" $ do
            let correct = ["d5-d5-1","d5-d5-3","d5-d5-5","d5-d5-7","d5-e6-0","d5-e4-0","d5-c4-0","d5-c6-0"] in
                map show (possibleMoves (Pos 'd' 5) (Piece White 170)) `shouldMatchList` correct
        it "commander state does not change forbidded" $ do
            possibleMoves (Pos 'a' 9) (Piece Black 170) `shouldNotContain` ([Move (Pos 'a' 9) (Pos 'a' 9) 2] :: [Move])
        -- others
        it "valid rotations" $ do
            rotationValid (possibleMoves (Pos 'd' 3) (Piece White 1)) `shouldBe` (False :: Bool)
            rotationValid (possibleMoves (Pos 'g' 2) (Piece White 129)) `shouldBe` (False :: Bool)
            rotationValid (possibleMoves (Pos 'e' 3) (Piece White 84)) `shouldBe` (False :: Bool)
            rotationValid (possibleMoves (Pos 'i' 8) (Piece Black 85)) `shouldBe` (False :: Bool)
        it "empty field" $ do
            possibleMoves (Pos 'a' 0) Empty `shouldBe` ([] :: [Move])


gradeListMoves :: Spec
gradeListMoves = describe "IF Grade listMoves 2FP" $ do
        it "finished board does not contain moves" $ do
            listMoves sampleBoard White `shouldBe` ([] :: [Move])
        it "not finished board contains moves white" $
            let board = [[Empty,Empty,Empty,(Piece Black 6),(Piece White 170),(Piece Black 64),(Piece Black 129),(Piece Black 131),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[(Piece Black 130),(Piece Black 131),Empty,Empty,(Piece Black 4),(Piece White 136),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 146),Empty,(Piece Black 170),Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 20),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 146)],[Empty,Empty,Empty,Empty,(Piece Black 17),Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,Empty,Empty,Empty,Empty,(Piece Black 69),Empty]] in do
                listMoves board White `shouldNotBe` ([] :: [Move])
        it "not finished board contains moves black" $
            let board = [[Empty,Empty,Empty,(Piece Black 6),(Piece White 170),(Piece Black 64),(Piece Black 129),(Piece Black 131),Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[(Piece Black 130),(Piece Black 131),Empty,Empty,(Piece Black 4),(Piece White 136),Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,(Piece Black 146),Empty,(Piece Black 170),Empty,Empty,Empty,Empty],[Empty,Empty,Empty,(Piece Black 20),Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Piece Black 146)],[Empty,Empty,Empty,Empty,(Piece Black 17),Empty,Empty,Empty,Empty],[Empty,(Piece Black 69),Empty,Empty,Empty,Empty,Empty,(Piece Black 69),Empty]] in do
                listMoves board Black `shouldNotBe` ([] :: [Move])
        it "all moves from start board white" $ do
            let correct = ["b9-b9-1","b9-b9-2","b9-b9-3","b9-b9-4","b9-b9-5","b9-b9-6","b9-b9-7","b9-b8-0","b9-b7-0","b9-b6-0","b9-a9-0","c8-c8-1","c8-c8-2","c8-c8-3","c8-c8-4","c8-c8-5","c8-c8-6","c8-c8-7","c8-c7-0","c8-c6-0","c9-c9-1","c9-c9-2","c9-c9-3","c9-c9-4","c9-c9-5","c9-c9-6","c9-c9-7","c9-b8-0","c9-a7-0","d7-d7-1","d7-d7-2","d7-d7-3","d7-d7-4","d7-d7-5","d7-d7-6","d7-d7-7","d7-d6-0","d7-d6-1","d7-d6-2","d7-d6-3","d7-d6-4","d7-d6-5","d7-d6-6","d7-d6-7","d8-d8-1","d8-d8-2","d8-d8-3","d8-d8-4","d8-d8-5","d8-d8-6","d8-d8-7","d8-c7-0","d8-b6-0","d9-d9-1","d9-d9-2","d9-d9-3","d9-d9-4","d9-d9-5","d9-d9-6","d9-d9-7","e7-e7-1","e7-e7-2","e7-e7-3","e7-e7-4","e7-e7-5","e7-e7-6","e7-e7-7","e7-e6-0","e7-e6-1","e7-e6-2","e7-e6-3","e7-e6-4","e7-e6-5","e7-e6-6","e7-e6-7","e8-e8-1","e8-e8-2","e8-e8-3","e8-e8-5","e8-e8-6","e8-e8-7","e9-e9-1","e9-e9-3","e9-e9-5","e9-e9-7","f7-f7-1","f7-f7-2","f7-f7-3","f7-f7-4","f7-f7-5","f7-f7-6","f7-f7-7","f7-f6-0","f7-f6-1","f7-f6-2","f7-f6-3","f7-f6-4","f7-f6-5","f7-f6-6","f7-f6-7","f8-f8-1","f8-f8-2","f8-f8-3","f8-f8-4","f8-f8-5","f8-f8-6","f8-f8-7","f8-g7-0","f8-h6-0","f9-f9-1","f9-f9-2","f9-f9-3","f9-f9-4","f9-f9-5","f9-f9-6","f9-f9-7","g8-g8-1","g8-g8-2","g8-g8-3","g8-g8-4","g8-g8-5","g8-g8-6","g8-g8-7","g8-g7-0","g8-g6-0","g9-g9-1","g9-g9-2","g9-g9-3","g9-g9-4","g9-g9-5","g9-g9-6","g9-g9-7","g9-h8-0","g9-i7-0","h9-h9-1","h9-h9-2","h9-h9-3","h9-h9-4","h9-h9-5","h9-h9-6","h9-h9-7","h9-i9-0","h9-h8-0","h9-h7-0","h9-h6-0"] in
                map show (listMoves startBoard White) `shouldMatchList` correct
        it "all moves from start board black" $ do
            let correct = ["b1-b1-1","b1-b1-2","b1-b1-3","b1-b1-4","b1-b1-5","b1-b1-6","b1-b1-7","b1-b2-0","b1-b3-0","b1-b4-0","b1-a1-0","c1-c1-1","c1-c1-2","c1-c1-3","c1-c1-4","c1-c1-5","c1-c1-6","c1-c1-7","c1-b2-0","c1-a3-0","c2-c2-1","c2-c2-2","c2-c2-3","c2-c2-4","c2-c2-5","c2-c2-6","c2-c2-7","c2-c3-0","c2-c4-0","d1-d1-1","d1-d1-2","d1-d1-3","d1-d1-4","d1-d1-5","d1-d1-6","d1-d1-7","d2-d2-1","d2-d2-2","d2-d2-3","d2-d2-4","d2-d2-5","d2-d2-6","d2-d2-7","d2-c3-0","d2-b4-0","d3-d3-1","d3-d3-2","d3-d3-3","d3-d3-4","d3-d3-5","d3-d3-6","d3-d3-7","d3-d4-0","d3-d4-1","d3-d4-2","d3-d4-3","d3-d4-4","d3-d4-5","d3-d4-6","d3-d4-7","e1-e1-1","e1-e1-3","e1-e1-5","e1-e1-7","e2-e2-1","e2-e2-2","e2-e2-3","e2-e2-5","e2-e2-6","e2-e2-7","e3-e3-1","e3-e3-2","e3-e3-3","e3-e3-4","e3-e3-5","e3-e3-6","e3-e3-7","e3-e4-0","e3-e4-1","e3-e4-2","e3-e4-3","e3-e4-4","e3-e4-5","e3-e4-6","e3-e4-7","f1-f1-1","f1-f1-2","f1-f1-3","f1-f1-4","f1-f1-5","f1-f1-6","f1-f1-7","f2-f2-1","f2-f2-2","f2-f2-3","f2-f2-4","f2-f2-5","f2-f2-6","f2-f2-7","f2-g3-0","f2-h4-0","f3-f3-1","f3-f3-2","f3-f3-3","f3-f3-4","f3-f3-5","f3-f3-6","f3-f3-7","f3-f4-0","f3-f4-1","f3-f4-2","f3-f4-3","f3-f4-4","f3-f4-5","f3-f4-6","f3-f4-7","g1-g1-1","g1-g1-2","g1-g1-3","g1-g1-4","g1-g1-5","g1-g1-6","g1-g1-7","g1-h2-0","g1-i3-0","g2-g2-1","g2-g2-2","g2-g2-3","g2-g2-4","g2-g2-5","g2-g2-6","g2-g2-7","g2-g3-0","g2-g4-0","h1-h1-1","h1-h1-2","h1-h1-3","h1-h1-4","h1-h1-5","h1-h1-6","h1-h1-7","h1-h2-0","h1-h3-0","h1-h4-0","h1-i1-0"] in
                map show (listMoves startBoard Black) `shouldMatchList` correct


-- Helpers
boardContains :: Board -> Cell -> Bool
boardContains board cell = let flattened = foldl (++) [] board
    in foldl (\a b ->  a || (b == cell)) False flattened

rotationValid :: [Move] -> Bool
rotationValid [] = False
rotationValid ((Move _ _ rot):xs) = rot >= 0 && rot <= 7 && rotationValid xs