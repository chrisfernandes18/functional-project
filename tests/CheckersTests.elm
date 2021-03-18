module CheckersTests exposing (..)

import Array exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Logic
import Structs exposing (..)
import Test exposing (..)


testBoardSpec : BoardSpec
testBoardSpec =
    -- test boardspec
    BS 70 30 10 10


testBoard : Array (Array Tile)
testBoard =
    -- test 2d array
    Logic.newBoard 8


testCheckers : Checkers
testCheckers =
    -- test game
    C (Board testBoard testBoardSpec) Nothing Nothing B 0 Nothing



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
                        PL 90 10

                    expected =
                        LL 0 1
                in
                input
                    |> Logic.physicalToLogical
                    |> (\func ->
                            func testBoardSpec
                       )
                    |> Expect.equal expected
        , Test.test "equalColors True" <|
            \() ->
                let
                    input1 =
                        B

                    input2 =
                        B

                    expected =
                        True
                in
                input1
                    |> Logic.equalColors
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "equalColors False" <|
            \() ->
                let
                    input1 =
                        B

                    input2 =
                        R

                    expected =
                        False
                in
                input1
                    |> Logic.equalColors
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "equalTiles True" <|
            \() ->
                let
                    input1 =
                        Piece B (LL 0 0) Inc

                    input2 =
                        Piece B (LL 0 0) Inc

                    expected =
                        True
                in
                input1
                    |> Logic.equalTiles
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "equalTiles False" <|
            \() ->
                let
                    input1 =
                        Piece B (LL 1 0) Inc

                    input2 =
                        Piece B (LL 0 0) Inc

                    expected =
                        False
                in
                input1
                    |> Logic.equalTiles
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "newBoard" <|
            \() ->
                let
                    input =
                        8

                    expected =
                        Array.fromList
                            [ Array.fromList [ E, Piece B (LL 0 1) Inc, E, Piece B (LL 0 3) Inc, E, Piece B (LL 0 5) Inc, E, Piece B (LL 0 7) Inc ]
                            , Array.fromList [ Piece B (LL 1 0) Inc, E, Piece B (LL 1 2) Inc, E, Piece B (LL 1 4) Inc, E, Piece B (LL 1 6) Inc, E ]
                            , Array.fromList [ E, Piece B (LL 2 1) Inc, E, Piece B (LL 2 3) Inc, E, Piece B (LL 2 5) Inc, E, Piece B (LL 2 7) Inc ]
                            , Array.fromList [ E, E, E, E, E, E, E, E ]
                            , Array.fromList [ E, E, E, E, E, E, E, E ]
                            , Array.fromList [ Piece R (LL 5 0) Dec, E, Piece R (LL 5 2) Dec, E, Piece R (LL 5 4) Dec, E, Piece R (LL 5 6) Dec, E ]
                            , Array.fromList [ E, Piece R (LL 6 1) Dec, E, Piece R (LL 6 3) Dec, E, Piece R (LL 6 5) Dec, E, Piece R (LL 6 7) Dec ]
                            , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, Piece R (LL 7 4) Dec, E, Piece R (LL 7 6) Dec, E ]
                            ]
                in
                input
                    |> Logic.newBoard
                    |> Expect.equal expected
        , Test.test "boardRef Empty" <|
            \() ->
                let
                    input1 =
                        testCheckers

                    input2 =
                        LL 0 0

                    expected =
                        E
                in
                input1
                    |> Logic.boardRef
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "boardRef Black Piece" <|
            \() ->
                let
                    input1 =
                        testCheckers

                    input2 =
                        LL 0 1

                    expected =
                        Piece B (LL 0 1) Inc
                in
                input1
                    |> Logic.boardRef
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "boardRef Red Piece" <|
            \() ->
                let
                    input1 =
                        testCheckers

                    input2 =
                        LL 7 6

                    expected =
                        Piece R (LL 7 6) Dec
                in
                input1
                    |> Logic.boardRef
                    |> (\func ->
                            func input2
                       )
                    |> Expect.equal expected
        , Test.test "movePiece non-capture valid move" <|
            \() ->
                let
                    input1 =
                        testCheckers

                    input2 =
                        LL 3 4

                    input3 =
                        Piece B (LL 2 3) Inc

                    expected =
                        Just
                            (C
                                (Board
                                    (Array.fromList
                                        [ Array.fromList [ E, Piece B (LL 0 1) Inc, E, Piece B (LL 0 3) Inc, E, Piece B (LL 0 5) Inc, E, Piece B (LL 0 7) Inc ]
                                        , Array.fromList [ Piece B (LL 1 0) Inc, E, Piece B (LL 1 2) Inc, E, Piece B (LL 1 4) Inc, E, Piece B (LL 1 6) Inc, E ]
                                        , Array.fromList [ E, Piece B (LL 2 1) Inc, E, E, E, Piece B (LL 2 5) Inc, E, Piece B (LL 2 7) Inc ]
                                        , Array.fromList [ E, E, E, E, Piece B (LL 3 4) Inc, E, E, E ]
                                        , Array.fromList [ E, E, E, E, E, E, E, E ]
                                        , Array.fromList [ Piece R (LL 5 0) Dec, E, Piece R (LL 5 2) Dec, E, Piece R (LL 5 4) Dec, E, Piece R (LL 5 6) Dec, E ]
                                        , Array.fromList [ E, Piece R (LL 6 1) Dec, E, Piece R (LL 6 3) Dec, E, Piece R (LL 6 5) Dec, E, Piece R (LL 6 7) Dec ]
                                        , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, Piece R (LL 7 4) Dec, E, Piece R (LL 7 6) Dec, E ]
                                        ]
                                    )
                                    (BS 70 30 10 10)
                                )
                                Nothing
                                Nothing
                                R
                                0
                                Nothing
                            )
                in
                input1
                    |> Logic.movePiece
                    |> (\func ->
                            func input2
                       )
                    |> (\func ->
                            func input3
                       )
                    |> Expect.equal expected
        , Test.test "movePiece non-capture invalid move" <|
            \() ->
                let
                    input1 =
                        testCheckers

                    input2 =
                        LL 4 5

                    input3 =
                        Piece B (LL 2 3) Inc

                    expected =
                        Nothing
                in
                input1
                    |> Logic.movePiece
                    |> (\func ->
                            func input2
                       )
                    |> (\func ->
                            func input3
                       )
                    |> Expect.equal expected
        , Test.test "movePiece valid Capture" <|
            \() ->
                let
                    input1 =
                        C
                            (Board
                                (Array.fromList
                                    [ Array.fromList [ E, Piece B (LL 0 1) Inc, E, Piece B (LL 0 3) Inc, E, Piece B (LL 0 5) Inc, E, Piece B (LL 0 7) Inc ]
                                    , Array.fromList [ Piece B (LL 1 0) Inc, E, Piece B (LL 1 2) Inc, E, Piece B (LL 1 4) Inc, E, Piece B (LL 1 6) Inc, E ]
                                    , Array.fromList [ E, Piece B (LL 2 1) Inc, E, E, E, Piece B (LL 2 5) Inc, E, Piece B (LL 2 7) Inc ]
                                    , Array.fromList [ E, E, E, E, Piece B (LL 3 4) Inc, E, E, E ]
                                    , Array.fromList [ E, E, E, Piece R (LL 4 3) Dec, E, E, E, E ]
                                    , Array.fromList [ Piece R (LL 5 0) Dec, E, E, E, Piece R (LL 5 4) Dec, E, Piece R (LL 5 6) Dec, E ]
                                    , Array.fromList [ E, Piece R (LL 6 1) Dec, E, Piece R (LL 6 3) Dec, E, Piece R (LL 6 5) Dec, E, Piece R (LL 6 7) Dec ]
                                    , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, Piece R (LL 7 4) Dec, E, Piece R (LL 7 6) Dec, E ]
                                    ]
                                )
                                (BS 70 30 493.5 82)
                            )
                            Nothing
                            Nothing
                            B
                            0
                            Nothing

                    input2 =
                        LL 5 2

                    input3 =
                        Piece B (LL 3 4) Inc

                    expected =
                        Just
                            (C
                                (Board
                                    (Array.fromList
                                        [ Array.fromList [ E, Piece B (LL 0 1) Inc, E, Piece B (LL 0 3) Inc, E, Piece B (LL 0 5) Inc, E, Piece B (LL 0 7) Inc ]
                                        , Array.fromList [ Piece B (LL 1 0) Inc, E, Piece B (LL 1 2) Inc, E, Piece B (LL 1 4) Inc, E, Piece B (LL 1 6) Inc, E ]
                                        , Array.fromList [ E, Piece B (LL 2 1) Inc, E, E, E, Piece B (LL 2 5) Inc, E, Piece B (LL 2 7) Inc ]
                                        , Array.fromList [ E, E, E, E, E, E, E, E ]
                                        , Array.fromList [ E, E, E, E, E, E, E, E ]
                                        , Array.fromList [ Piece R (LL 5 0) Dec, E, Piece B (LL 5 2) Inc, E, Piece R (LL 5 4) Dec, E, Piece R (LL 5 6) Dec, E ]
                                        , Array.fromList [ E, Piece R (LL 6 1) Dec, E, Piece R (LL 6 3) Dec, E, Piece R (LL 6 5) Dec, E, Piece R (LL 6 7) Dec ]
                                        , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, Piece R (LL 7 4) Dec, E, Piece R (LL 7 6) Dec, E ]
                                        ]
                                    )
                                    (BS 70 30 493.5 82)
                                )
                                Nothing
                                Nothing
                                R
                                0
                                Nothing
                            )
                in
                input1
                    |> Logic.movePiece
                    |> (\func ->
                            func input2
                       )
                    |> (\func ->
                            func input3
                       )
                    |> Expect.equal expected
        , Test.test "movePiece invalid Capture" <|
            \() ->
                let
                    input1 =
                        C
                            (Board
                                (Array.fromList
                                    [ Array.fromList [ E, Piece B (LL 0 1) Inc, E, Piece B (LL 0 3) Inc, E, Piece B (LL 0 5) Inc, E, Piece B (LL 0 7) Inc ]
                                    , Array.fromList [ Piece B (LL 1 0) Inc, E, Piece B (LL 1 2) Inc, E, Piece B (LL 1 4) Inc, E, Piece B (LL 1 6) Inc, E ]
                                    , Array.fromList [ E, Piece B (LL 2 1) Inc, E, E, E, E, E, Piece B (LL 2 7) Inc ]
                                    , Array.fromList [ E, E, E, E, Piece B (LL 3 4) Inc, E, E, E ]
                                    , Array.fromList [ E, E, E, Piece R (LL 4 3) Dec, E, E, E, E ]
                                    , Array.fromList [ Piece R (LL 5 0) Dec, E, Piece B (LL 5 2) Inc, E, E, E, Piece R (LL 5 6) Dec, E ]
                                    , Array.fromList [ E, Piece R (LL 6 1) Dec, E, Piece R (LL 6 3) Dec, E, Piece R (LL 6 5) Dec, E, Piece R (LL 6 7) Dec ]
                                    , Array.fromList [ Piece R (LL 7 0) Dec, E, Piece R (LL 7 2) Dec, E, Piece R (LL 7 4) Dec, E, Piece R (LL 7 6) Dec, E ]
                                    ]
                                )
                                (BS 70 30 493.5 82)
                            )
                            Nothing
                            Nothing
                            R
                            0
                            Nothing

                    input2 =
                        LL 4 3

                    input3 =
                        Piece R (LL 6 1) Dec

                    expected =
                        Nothing
                in
                input1
                    |> Logic.movePiece
                    |> (\func ->
                            func input2
                       )
                    |> (\func ->
                            func input3
                       )
                    |> Expect.equal expected
        , Test.test "endGame no black pieces on board" <|
            \() ->
                let
                    input =
                        testEndGame

                    expected =
                        True
                in
                input
                    |> Logic.endGame
                    |> Expect.equal expected
        , Test.test "endGame no red pieces on board" <|
            \() ->
                let
                    input =
                        testEndGame3

                    expected =
                        True
                in
                input
                    |> Logic.endGame
                    |> Expect.equal expected
        , Test.test "endGame black has no moves left" <|
            \() ->
                let
                    input =
                        testEndGame2

                    expected =
                        True
                in
                input
                    |> Logic.endGame
                    |> Expect.equal expected
        , Test.test "endGame black has valid moves left" <|
            \() ->
                let
                    input =
                        testEndGameFalse

                    expected =
                        False
                in
                input
                    |> Logic.endGame
                    |> Expect.equal expected
        ]
