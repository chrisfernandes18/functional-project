module Logic exposing
    ( boardRef
    , legalMove
    , logicalToPhysical
    , movePiece
    , newBoard
    , physicalToLogical
    , testCheckers
    )

import Array exposing (Array)
import Structs exposing (..)



{-------------------------- Helper Functions --------------------------}


distancePL : PhysicalLoc -> PhysicalLoc -> Float
distancePL p1 p2 =
    -- checks the distance between two physical locations
    case ( p1, p2 ) of
        ( PL x1 y1, PL x2 y2 ) ->
            sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)


withinPiece : PhysicalLoc -> PhysicalLoc -> BoardSpec -> Bool
withinPiece p1 p2 bs =
    -- checks if two physical locations are indeed the same checker
    -- piece
    case bs of
        BS _ pr _ _ ->
            distancePL p1 p2 <= pr


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
                        Array.initialize i (\ind -> E)

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
            incOk && decOk


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
                                    curCol - 1

                                 else
                                    curCol + 1
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
                                    curCol - 1

                                 else
                                    curCol + 1
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
            if incOk && decOk then
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
                                    curCol - 1

                                 else
                                    curCol + 1
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

        Piece color (LL curRow curCol) move ->
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
                    t


setCurrentTile : Checkers -> LogicalLoc -> Checkers
setCurrentTile c ll =
    -- Sets the current tile slot within the Checkers struct
    -- given the location that was 'clicked'
    let
        tileSelection =
            boardRef c ll
    in
    case c of
        C b p1 p2 moves cp ct ->
            case tileSelection of
                E ->
                    C b p1 p2 moves cp Nothing

                Piece color cll move ->
                    C b p1 p2 moves cp (Just (Piece color cll move))



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
            LL (round ((y - my) / cs)) (round ((x - mx) / cs))


boardRef : Checkers -> LogicalLoc -> Tile
boardRef c (LL row col) =
    -- Takes the game and a location and returns the tile
    -- if its Empty or a Piece
    case c of
        C b p1 p2 moves cp ct ->
            case b of
                Board arr bs ->
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
    case ( c, t ) of
        ( C b p1 p2 moves cp ct, E ) ->
            False

        ( C b p1 p2 moves cp ct, Piece _ (LL curRow curCol) move ) ->
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


movePiece : Checkers -> LogicalLoc -> Tile -> Maybe Checkers
movePiece c (LL newRow newCol) t =
    -- If the move is legal it makes the move happen and
    -- returns the new checkers game otherwise returns
    -- nothing
    if legalMove c (LL newRow newCol) t then
        case ( c, t ) of
            ( C (Board b bs) p1 p2 moves cp ct, Piece color (LL curRow curCol) move ) ->
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
                                curCol - 1

                            else
                                curCol + 1

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
                    Just (C (Board board bs) p1 p2 moves cp ct)

                else
                    Just (C (Board newBoardFinal bs) p1 p2 moves cp ct)

            _ ->
                Nothing

    else
        Nothing


testCheckers : Checkers
testCheckers =
    -- test game
    C (Board (newBoard 8) (BS 70 30 10 10)) (Human "Christian" B) (Human "Angela" R) B 0 Nothing
