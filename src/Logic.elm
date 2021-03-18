module Logic exposing
    ( boardRef
    , checkBot
    , endGame
    , equalColors
    , equalTiles
    , logicalToPhysical
    , makeBotMove
    , movePiece
    , newBoard
    , physicalToLogical
    , testCheckers
    , testEndGame
    , testEndGame2
    , testEndGame3
    , testEndGameFalse
    )

import Array exposing (Array)
import List
import Structs exposing (..)



{-------------------------- Helper Functions --------------------------}


equalColors : Color -> Color -> Bool
equalColors c1 c2 =
    case ( c1, c2 ) of
        ( B, B ) ->
            True

        ( R, R ) ->
            True

        _ ->
            False


equalTiles : Tile -> Tile -> Bool
equalTiles t1 t2 =
    case ( t1, t2 ) of
        ( Piece B (LL r1 c1) Inc, Piece B (LL r2 c2) Inc ) ->
            (r1 == r2) && (c1 == c2)

        ( Piece B (LL r1 c1) Dec, Piece B (LL r2 c2) Dec ) ->
            (r1 == r2) && (c1 == c2)

        ( Piece B (LL r1 c1) Both, Piece B (LL r2 c2) Both ) ->
            (r1 == r2) && (c1 == c2)

        ( Piece R (LL r1 c1) Inc, Piece R (LL r2 c2) Inc ) ->
            (r1 == r2) && (c1 == c2)

        ( Piece R (LL r1 c1) Dec, Piece R (LL r2 c2) Dec ) ->
            (r1 == r2) && (c1 == c2)

        ( Piece R (LL r1 c1) Both, Piece R (LL r2 c2) Both ) ->
            (r1 == r2) && (c1 == c2)

        _ ->
            False


changeColor : Color -> Color
changeColor col =
    case col of
        B ->
            R

        R ->
            B


newTile : Maybe Color -> Int -> Int -> Tile
newTile color row col =
    -- creates a new tile given a color and its location
    -- used in newBoard
    case color of
        Nothing ->
            E

        Just c ->
            Piece c
                (LL row col)
                (if row < 3 then
                    Inc

                 else
                    Dec
                )


newBoard : Int -> Array (Array Tile)
newBoard i =
    -- creates an initial board where all the pieces are set up correctly
    let
        res =
            Array.initialize i
                (\rowInd ->
                    let
                        rowEven =
                            modBy 2 rowInd
                    in
                    if rowInd == 3 || rowInd == 4 then
                        Array.initialize i (\_ -> E)

                    else if rowEven == 0 then
                        Array.initialize i
                            (\colInd ->
                                let
                                    colEven =
                                        modBy 2 colInd
                                in
                                if rowInd < 3 then
                                    if colEven == 0 then
                                        newTile Nothing rowInd colInd

                                    else
                                        newTile (Just B) rowInd colInd

                                else if colEven == 0 then
                                    newTile Nothing rowInd colInd

                                else
                                    newTile (Just R) rowInd colInd
                            )

                    else
                        Array.initialize i
                            (\colInd ->
                                let
                                    colEven =
                                        modBy 2 colInd
                                in
                                if rowInd < 3 then
                                    if colEven == 0 then
                                        newTile (Just B) rowInd colInd

                                    else
                                        newTile Nothing rowInd colInd

                                else if colEven == 0 then
                                    newTile (Just R) rowInd colInd

                                else
                                    newTile Nothing rowInd colInd
                            )
                )
    in
    res


withinBounds : LogicalLoc -> Bool
withinBounds (LL newRow newCol) =
    -- checks if the location is within the bounds of the board
    (newCol >= 0) && (newCol <= 7) && (newRow >= 0) && (newRow <= 7)


checkDirection : Move -> LogicalLoc -> LogicalLoc -> Bool
checkDirection m (LL curRow curCol) (LL newRow newCol) =
    -- checks if the piece is moving in the right direction
    case m of
        Inc ->
            ((newRow > curRow) && (curRow + 1) == newRow) && (((curCol + 1) == newCol) || ((curCol - 1) == newCol))

        Dec ->
            ((newRow < curRow) && (curRow - 1) == newRow) && (((curCol + 1) == newCol) || ((curCol - 1) == newCol))

        Both ->
            let
                incOk =
                    ((newRow > curRow) && (curRow + 1) == newRow) && (((curCol + 1) == newCol) || ((curCol - 1) == newCol))

                decOk =
                    ((newRow < curRow) && (curRow - 1) == newRow) && (((curCol + 1) == newCol) || ((curCol - 1) == newCol))
            in
            incOk || decOk


checkCapture : Checkers -> Move -> LogicalLoc -> LogicalLoc -> Bool
checkCapture c m (LL curRow curCol) (LL newRow newCol) =
    -- checks if the piece is trying to capture a piece or not
    case m of
        Inc ->
            if (newRow > curRow) && ((curRow + 2) == newRow) then
                let
                    dif =
                        curCol - newCol

                    tile =
                        boardRef c
                            (LL (curRow + 1)
                                (if dif < 0 then
                                    curCol + 1

                                 else
                                    curCol - 1
                                )
                            )

                    curtile =
                        boardRef c (LL curRow curCol)
                in
                case ( tile, curtile ) of
                    ( E, _ ) ->
                        False

                    ( Piece B _ _, Piece B _ _ ) ->
                        False

                    ( Piece R _ _, Piece R _ _ ) ->
                        False

                    _ ->
                        True

            else
                False

        Dec ->
            if (newRow < curRow) && (curRow - 2) == newRow then
                let
                    dif =
                        curCol - newCol

                    tile =
                        boardRef c
                            (LL (curRow - 1)
                                (if dif < 0 then
                                    curCol + 1

                                 else
                                    curCol - 1
                                )
                            )

                    curtile =
                        boardRef c (LL curRow curCol)
                in
                case ( tile, curtile ) of
                    ( E, _ ) ->
                        False

                    ( Piece B _ _, Piece B _ _ ) ->
                        False

                    ( Piece R _ _, Piece R _ _ ) ->
                        False

                    _ ->
                        True

            else
                False

        Both ->
            let
                incOk =
                    (newRow > curRow) && (curRow + 2) == newRow

                decOk =
                    (newRow < curRow) && (curRow - 2) == newRow
            in
            if incOk || decOk then
                let
                    dif =
                        curCol - newCol

                    tile =
                        boardRef c
                            (LL
                                (if newRow > curRow then
                                    curRow + 1

                                 else
                                    curRow - 1
                                )
                                (if dif < 0 then
                                    curCol + 1

                                 else
                                    curCol - 1
                                )
                            )

                    curtile =
                        boardRef c (LL curRow curCol)
                in
                case ( tile, curtile ) of
                    ( E, _ ) ->
                        False

                    ( Piece B _ _, Piece B _ _ ) ->
                        False

                    ( Piece R _ _, Piece R _ _ ) ->
                        False

                    _ ->
                        True

            else
                False


kingMe : LogicalLoc -> Tile -> Tile
kingMe (LL newRow newCol) t =
    -- Given the location the tile wants to move to
    -- returns either the new tile and if it is able
    -- to be kinged then it is made into a king
    -- otherwise just updates the location or returns
    -- empty if the tile was empty
    case t of
        E ->
            E

        Piece color (LL _ _) move ->
            case move of
                Inc ->
                    if newRow == 7 then
                        Piece color (LL newRow newCol) Both

                    else
                        Piece color (LL newRow newCol) move

                Dec ->
                    if newRow == 0 then
                        Piece color (LL newRow newCol) Both

                    else
                        Piece color (LL newRow newCol) move

                _ ->
                    Piece color (LL newRow newCol) move



{------------------------- Exposed Functions -------------------------}


logicalToPhysical : LogicalLoc -> BoardSpec -> PhysicalLoc
logicalToPhysical ll bs =
    -- Converts the location within data structures to
    -- where it should be in GUI
    case ( ll, bs ) of
        ( LL row col, BS cs _ mx my ) ->
            if not (withinBounds ll) then
                Debug.todo "logicalToPhysical: spot not on board"

            else
                PL (my + (cs * toFloat col)) (mx + (cs * toFloat row))


physicalToLogical : PhysicalLoc -> BoardSpec -> LogicalLoc
physicalToLogical pl bs =
    -- Converts the location on GUI to where it should be within
    -- data structures
    case ( pl, bs ) of
        ( PL x y, BS cs _ mx my ) ->
            LL (floor ((y - my) / cs)) (floor ((x - mx) / cs))


boardRef : Checkers -> LogicalLoc -> Tile
boardRef c (LL row col) =
    -- Takes the game and a location and returns the tile
    -- if its Empty or a Piece
    case c of
        C b _ _ _ _ _ ->
            case b of
                Board arr _ ->
                    case Array.get row arr of
                        Nothing ->
                            E

                        Just arrRow ->
                            case Array.get col arrRow of
                                Nothing ->
                                    E

                                Just t ->
                                    t


legalMove : Checkers -> LogicalLoc -> Tile -> Bool
legalMove c (LL newRow newCol) t =
    -- Given a board and a new location and the current
    -- piece that is selected returns if that piece can move
    case boardRef c (LL newRow newCol) of
        E ->
            case t of
                E ->
                    False

                Piece _ (LL curRow curCol) move ->
                    let
                        cap =
                            checkCapture c move (LL curRow curCol) (LL newRow newCol)

                        dir =
                            checkDirection move (LL curRow curCol) (LL newRow newCol)

                        bounds =
                            withinBounds (LL newRow newCol)
                    in
                    if cap == False then
                        (dir == True) && (bounds == True)

                    else
                        bounds == True

        _ ->
            False


movePiece : Checkers -> LogicalLoc -> Tile -> Maybe Checkers
movePiece c (LL newRow newCol) t =
    -- If the move is legal it makes the move happen and
    -- returns the new checkers game otherwise returns
    -- nothing
    if legalMove c (LL newRow newCol) t then
        case ( c, t ) of
            ( C (Board b bs) p1 p2 cp moves _, Piece _ (LL curRow curCol) move ) ->
                let
                    -- will king the piece if it's a king
                    newPiece =
                        kingMe (LL newRow newCol) t

                    -- delete current piece and put that updates row in board
                    leaveRow =
                        case Array.get curRow b of
                            Nothing ->
                                Array.empty

                            Just lArr ->
                                lArr

                    deletePiece =
                        Array.set curCol E leaveRow

                    newBoardInter =
                        Array.set curRow deletePiece b

                    -- add the new piece in the new row and put it in board
                    enterRow =
                        case Array.get newRow newBoardInter of
                            Nothing ->
                                Array.empty

                            Just eArr ->
                                eArr

                    addPiece =
                        Array.set newCol newPiece enterRow

                    newBoardFinal =
                        Array.set newRow addPiece newBoardInter
                in
                if checkCapture c move (LL curRow curCol) (LL newRow newCol) then
                    let
                        dif =
                            curCol - newCol

                        takeRow =
                            if newRow > curRow then
                                curRow + 1

                            else
                                curRow - 1

                        takeCol =
                            if dif < 0 then
                                curCol + 1

                            else
                                curCol - 1

                        takeBoardRow =
                            case Array.get takeRow newBoardFinal of
                                Nothing ->
                                    Array.empty

                                Just tArr ->
                                    tArr

                        takeBoardCol =
                            Array.set takeCol E takeBoardRow

                        board =
                            Array.set takeRow takeBoardCol newBoardFinal
                    in
                    Just (C (Board board bs) p1 p2 (changeColor cp) moves Nothing)

                else
                    Just (C (Board newBoardFinal bs) p1 p2 (changeColor cp) moves Nothing)

            _ ->
                Nothing

    else
        Nothing



-- returns whether the tile is of the given color


tileGivenColor : Tile -> Color -> Bool
tileGivenColor t c1 =
    case t of
        E ->
            False

        Piece c2 _ _ ->
            equalColors c1 c2



-- get all the tiles of a current player


getPlayerTiles : Array (Array Tile) -> Color -> Array Tile
getPlayerTiles board c =
    let
        playerTiles =
            \row -> Array.filter (\tile -> tileGivenColor tile c) row
    in
    Array.foldr (\row output -> Array.append (playerTiles row) output) Array.empty board



-- return the four potential inc. moves from r, c


getIncLL : Int -> Int -> List LogicalLoc
getIncLL r c =
    let
        incLeft =
            LL (r + 1) (c - 1)

        incRight =
            LL (r + 1) (c + 1)

        capIncLeft =
            LL (r + 2) (c - 2)

        capIncRight =
            LL (r + 2) (c + 2)
    in
    [ incLeft, incRight, capIncLeft, capIncRight ]



-- return the four potential dec. moves from r, c


getDecLL : Int -> Int -> List LogicalLoc
getDecLL r c =
    let
        decLeft =
            LL (r - 1) (c - 1)

        decRight =
            LL (r - 1) (c + 1)

        capDecLeft =
            LL (r - 2) (c - 2)

        capDecRight =
            LL (r - 2) (c + 2)
    in
    [ decLeft, decRight, capDecLeft, capDecRight ]



-- return the 8 potential moves from r, c


getBothLL : Int -> Int -> List LogicalLoc
getBothLL r c =
    getIncLL r c ++ getDecLL r c



-- taking current board and location of a piece, return valid moves


getLegalMoves : Checkers -> Tile -> List ( LogicalLoc, Tile )
getLegalMoves checkers tile =
    case tile of
        E ->
            []

        Piece _ (LL r c) move ->
            let
                llocLegal =
                    \lloc -> legalMove checkers lloc tile

                addTile =
                    \lloc -> ( lloc, tile )

                wrapList =
                    \lst -> List.map addTile (List.filter llocLegal lst)
            in
            case move of
                Inc ->
                    wrapList (getIncLL r c)

                Dec ->
                    wrapList (getDecLL r c)

                Both ->
                    wrapList (getBothLL r c)



-- given a board, get all legal moves for a player


getAllLegalMoves : Checkers -> List ( LogicalLoc, Tile )
getAllLegalMoves checkers =
    case checkers of
        C (Board board _) _ _ currPlayer _ _ ->
            let
                tiles =
                    Array.toList (getPlayerTiles board currPlayer)

                moves =
                    \tile -> getLegalMoves checkers tile
            in
            List.concatMap moves tiles



-- check that given player is robot


checkBot : Maybe Player -> Bool
checkBot p =
    case p of
        Just (Robot _ _) ->
            True

        _ ->
            False



-- given a board, select make a random move


makeBotMove : Checkers -> Maybe Checkers
makeBotMove checkers =
    let
        legalMoves =
            getAllLegalMoves checkers

        ( randomLLoc, randomTile ) =
            case List.head legalMoves of
                -- just pick first move for now
                Just ( l, t ) ->
                    ( l, t )

                Nothing ->
                    ( LL 0 0, E )
    in
    -- consider black player1, red player2
    if List.isEmpty legalMoves then
        Nothing

    else
        case checkers of
            -- assume only given with correct inputs
            C _ _ _ B _ _ ->
                movePiece checkers randomLLoc randomTile

            C _ _ _ R _ _ ->
                movePiece checkers randomLLoc randomTile


endGame : Checkers -> Bool
endGame checkers =
    case checkers of
        C (Board board _) _ _ currPlayer _ _ ->
            let
                -- rowContains? is True if one row has one element of curPlayer
                currentPlayerTile =
                    \tile -> tileGivenColor tile currPlayer

                rowContains =
                    \row -> not (Array.isEmpty (Array.filter currentPlayerTile row))

                containsArr =
                    Array.map rowContains board
            in
            if Array.foldr (||) False containsArr then
                List.isEmpty (getAllLegalMoves checkers)

            else
                True



-- no more pieces left


testCheckers : Checkers
testCheckers =
    -- test game
    C (Board (newBoard 8) (BS 70 30 10 10)) Nothing Nothing B 0 Nothing



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
