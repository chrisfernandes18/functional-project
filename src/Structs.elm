module Structs exposing (..)

import Array exposing (Array)


type Color
    = R
    | B


type LogicalLoc {- location internally row col -}
    = LL Int Int


type PhysicalLoc {- location in GUI x y -}
    = PL Float Float


type Move {- if a piece is moving positively or negatively -}
    = Inc
    | Dec
    | Both


type Tile
    = E
    | Piece Color LogicalLoc Move


type alias CellSize {- size of each square on board -} =
    Float


type alias PieceRadius {- radius of checkers piece -} =
    Float


type alias MarginSizeX {- amount of margin around board -} =
    Float


type alias MarginSizeY {- amount of margin around board -} =
    Float


type alias BorderSize {- the border-size around board -} =
    Float


type BoardSpec
    = BS BorderSize CellSize PieceRadius MarginSizeX MarginSizeY


type Board
    = Board (Array (Array Tile)) BoardSpec


type Player
    = Human String Color
    | Robot String Color


type alias CurrPlayer =
    Color


type Checkers
    = C Board CurrPlayer {- number of Moves -} Int (Maybe Tile)
