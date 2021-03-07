module Structs exposing (..)

import Array exposing (Array)


type Color
    = R
    | B


type LogicalLoc {- location internally row col -}
    = LL Int Int


type PhysicalLoc {- location in GUI x y -}
    = PL Int Int


type Move {- if a piece is moving positively or negatively -}
    = Inc
    | Dec
    | Both


type Tile
    = E
    | Piece Color LogicalLoc Move


type alias CellSize {- size of each square on board -} =
    Int


type alias PieceRadius {- radius of checkers piece -} =
    Int


type alias MarginSize {- amount of margin around board -} =
    Int


type BoardSpec
    = BS CellSize PieceRadius MarginSize


type Board
    = Board (Array (Array Tile)) BoardSpec


type Player
    = Human String Color
    | Robot String Color


type alias CurrPlayer =
    Color


type Checkers
    = C Board Player Player CurrPlayer {- number of Moves -} Int (Maybe Tile)
