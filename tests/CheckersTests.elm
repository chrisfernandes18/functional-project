module CheckersTests exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Logic
import Structs exposing (..)
import Test exposing (..)


testCheckers : Checkers
testCheckers =
    -- test game
    C (Board (Logic.newBoard 8) (BS 70 30 10 10)) Nothing Nothing B 0 Nothing


testBoardSpec : BoardSpec
testBoardSpec =
    BS 70 30 10 10



-- test game where one player has no pieces


testEndGame : Checkers
testEndGame =
    C
        (Board
            (Array.fromList
                [ Array.fromList [ E, Piece R (LL 0 1) Both, E, Piece R (LL 0 3) Both, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, Piece R (LL 2 3) Dec, E, E, E, E ]
                , Array.fromList [ E, E, Piece R (LL 3 2) Dec, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, E, E, Piece R (LL 7 6) Dec, E ]
                ]
            )
            (BS 70 30 360 98)
        )
        Nothing
        Nothing
        B
        0
        Nothing



-- another test end game where black has no moves


testEndGame2 : Checkers
testEndGame2 =
    C
        (Board
            (Array.fromList
                [ Array.fromList [ E, Piece R (LL 0 1) Both, E, E, E, E, E, E ]
                , Array.fromList [ E, E, Piece R (LL 1 2) Dec, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, Piece R (LL 2 5) Dec, E, Piece B (LL 2 7) Inc ]
                , Array.fromList [ E, E, E, E, E, E, Piece B (LL 3 6) Inc, E ]
                , Array.fromList [ E, E, E, E, E, Piece R (LL 4 5) Dec, E, Piece R (LL 4 7) Dec ]
                , Array.fromList [ E, E, E, E, Piece R (LL 5 4) Dec, E, Piece R (LL 5 6) Dec, E ]
                , Array.fromList [ E, E, E, E, E, Piece R (LL 6 5) Dec, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                ]
            )
            (BS 70 30 360 98)
        )
        Nothing
        Nothing
        B
        0
        Nothing


testEndGame3 : Checkers
testEndGame3 =
    C
        (Board
            (Array.fromList
                [ Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, Piece B (LL 1 4) Both, E, Piece B (LL 1 6) Inc, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, Piece B (LL 5 2) Inc, E, E, E, E, E ]
                , Array.fromList [ E, E, E, Piece B (LL 6 3) Inc, E, Piece B (LL 6 5) Inc, E, E ]
                , Array.fromList [ E, E, E, E, Piece B (LL 7 4) Both, E, E, E ]
                ]
            )
            (BS 70 30 360 79)
        )
        Nothing
        Nothing
        R
        0
        Nothing



-- false end game - move for black


testEndGameFalse : Checkers
testEndGameFalse =
    C
        (Board
            (Array.fromList
                [ Array.fromList [ E, Piece B (LL 0 1) Both, E, Piece R (LL 0 3) Both, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, Piece R (LL 2 3) Dec, E, E, E, E ]
                , Array.fromList [ E, E, Piece R (LL 3 2) Dec, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ E, E, E, E, E, E, E, E ]
                , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, E, E, Piece R (LL 7 6) Dec, E ]
                ]
            )
            (BS 70 30 360 98)
        )
        Nothing
        Nothing
        B
        0
        Nothing



{-
   boardRef
   , checkBot
   , endGame
   , equalColors
   , equalTiles
   , logicalToPhysical
   , makeBotMove
   , movePiece
   , newBoard
   , physicalToLogical
-}


suite : Test
suite =
    Test.describe "Test Suite for Checkers"
        [ Test.test "logicalToPhysical" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "physicalToLogical" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "equalColors" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "equalTiles" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "newBoard" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "boardRef" <|
            \() ->
                let
                    input =
                        LL 0 0

                    expected =
                        PL 10 10
                in
                input
                    |> Logic.logicalToPhysical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        ]
