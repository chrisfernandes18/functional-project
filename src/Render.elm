port module Render exposing (main)

-- Imports -----------------------------------------------------------

import Array
import Browser
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes as HAttrs
import Html.Events as HEvents
import Http exposing (..)
import Json.Decode as Decode
import Logic exposing (..)
import Structs exposing (..)
import Svg exposing (..)
import Svg.Attributes as SAttrs



----------------------------------------------------------------------


type alias Point =
    { x : Float, y : Float }



-- type Model
--    = M Checkers Point


type alias Model =
    { checkers : Checkers
    , point : Point
    , gameOver : Bool
    , player1 : Maybe Player
    , player2 : Maybe Player
    }


type Msg
    = Click Point
    | Offset (List Float)
      -- | SubmitForm
    | UpdatePlayer1 String
    | UpdatePlayer2 String


type alias Flags =
    ()



{--------------------------- Port Functions ---------------------------}


port recieveBoardOffset : (List Float -> msg) -> Sub msg


port requestBoardOffset : () -> Cmd msg



{-------------------------- Helper Functions --------------------------}


toTr : List (Html Msg) -> Html Msg
toTr hs =
    Html.tr [] hs



{------------------------- Exposed Functions -------------------------}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, requestBoardOffset () )


initModel : Model
initModel =
    { checkers = C (Board (newBoard 8) (BS 70 30 10 10)) Nothing Nothing B 0 Nothing
    , point = { x = 0, y = 0 }
    , gameOver = False
    , player1 = Just (Human "" B)
    , player2 = Just (Human "" R)
    }


boardToHTML : Board -> Maybe Tile -> Html Msg
boardToHTML b til =
    case b of
        Board arr (BS cs pr _ _) ->
            let
                curTile =
                    case til of
                        Nothing ->
                            E

                        Just t ->
                            t

                hheight =
                    HAttrs.style "height" (Debug.toString cs ++ "px")

                hwidth =
                    HAttrs.style "width" (Debug.toString cs ++ "px")

                sheight =
                    SAttrs.height (Debug.toString cs)

                swidth =
                    SAttrs.height (Debug.toString cs)

                convertedRows =
                    Array.indexedMap
                        (\row rowArr ->
                            let
                                evenRow =
                                    modBy 2 row == 0
                            in
                            Array.indexedMap
                                (\col tile ->
                                    let
                                        evenCol =
                                            modBy 2 col == 0
                                    in
                                    case tile of
                                        E ->
                                            if (evenRow && evenCol) || (not evenRow && not evenCol) then
                                                Html.td [ HAttrs.class "noPiece", hheight, hwidth ] []

                                            else
                                                Html.td [ hheight, hwidth ] []

                                        Piece color (LL c1 r1) move ->
                                            let
                                                equalTile =
                                                    equalTiles (Piece color (LL c1 r1) move) curTile
                                            in
                                            case color of
                                                B ->
                                                    if (evenRow && evenCol) || (not evenRow && not evenCol) then
                                                        Html.td [ HAttrs.class "noPiece", hheight, hwidth ] []

                                                    else
                                                        case move of
                                                            Both ->
                                                                Html.td
                                                                    [ hheight, hwidth ]
                                                                    [ svg
                                                                        [ swidth
                                                                        , sheight
                                                                        , SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs)
                                                                        , SAttrs.class "blackPieceKing"
                                                                        , SAttrs.id
                                                                            (if equalTile then
                                                                                "selected"

                                                                             else
                                                                                ""
                                                                            )
                                                                        ]
                                                                        [ circle
                                                                            [ SAttrs.cx (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.cy (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.r (String.fromFloat pr)
                                                                            ]
                                                                            []
                                                                        ]
                                                                    ]

                                                            _ ->
                                                                Html.td
                                                                    [ hheight, hwidth ]
                                                                    [ svg
                                                                        [ swidth
                                                                        , sheight
                                                                        , SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs)
                                                                        , SAttrs.class "blackPiece"
                                                                        , SAttrs.id
                                                                            (if equalTile then
                                                                                "selected"

                                                                             else
                                                                                ""
                                                                            )
                                                                        ]
                                                                        [ circle
                                                                            [ SAttrs.cx (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.cy (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.r (String.fromFloat pr)
                                                                            ]
                                                                            []
                                                                        ]
                                                                    ]

                                                R ->
                                                    if (evenRow && evenCol) || (not evenRow && not evenCol) then
                                                        Html.td [ HAttrs.class "noPiece", hheight, hwidth ] []

                                                    else
                                                        case move of
                                                            Both ->
                                                                Html.td
                                                                    [ hheight, hwidth ]
                                                                    [ svg
                                                                        [ swidth
                                                                        , sheight
                                                                        , SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs)
                                                                        , SAttrs.class "redPieceKing"
                                                                        , SAttrs.id
                                                                            (if equalTile then
                                                                                "selected"

                                                                             else
                                                                                ""
                                                                            )
                                                                        ]
                                                                        [ circle
                                                                            [ SAttrs.cx (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.cy (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.r (String.fromFloat pr)
                                                                            ]
                                                                            []
                                                                        ]
                                                                    ]

                                                            _ ->
                                                                Html.td
                                                                    [ hheight, hwidth ]
                                                                    [ svg
                                                                        [ swidth
                                                                        , sheight
                                                                        , SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs)
                                                                        , SAttrs.class "redPiece"
                                                                        , SAttrs.id
                                                                            (if equalTile then
                                                                                "selected"

                                                                             else
                                                                                ""
                                                                            )
                                                                        ]
                                                                        [ circle
                                                                            [ SAttrs.cx (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.cy (String.fromFloat (cs / 2.0))
                                                                            , SAttrs.r (String.fromFloat pr)
                                                                            ]
                                                                            []
                                                                        ]
                                                                    ]
                                )
                                rowArr
                        )
                        arr

                arrayLst =
                    Array.toList (Array.map toTr (Array.map Array.toList convertedRows))
            in
            Html.table [ HAttrs.style "margin" "auto", HAttrs.id "checkers" ] arrayLst


view : Model -> Html Msg
view model =
    case ( model.checkers, model.point ) of
        ( C (Board b (BS cs pr mx my)) _ _ cp _ ct, _ ) ->
            let
                endText =
                    if model.gameOver then
                        "Game over: "
                            ++ (case cp of
                                    R ->
                                        "Black Wins!"

                                    B ->
                                        "Red Wins!"
                               )

                    else
                        "Checkers"
            in
            Html.div
                [ HAttrs.style "text-align" "center" ]
                [ Html.h1
                    [ HAttrs.style "font-size" "50px" ]
                    [ Html.text endText ]
                , Html.div
                    []
                    [ boardToHTML (Board b (BS cs pr mx my)) ct ]
                , Html.form
                    [ HAttrs.id "players" ]
                    [ Html.br [] []
                    , Html.select
                        [ HAttrs.id "choice1", HEvents.onInput UpdatePlayer1 ]
                        [ Html.option [ HAttrs.value "human" ] [ Html.text "Human" ]
                        , Html.option [ HAttrs.value "bot" ] [ Html.text "Bot" ]
                        ]
                    , Html.select
                        [ HAttrs.id "choice2", HEvents.onInput UpdatePlayer2 ]
                        [ Html.option [ HAttrs.value "human" ] [ Html.text "Human" ]
                        , Html.option [ HAttrs.value "bot" ] [ Html.text "Bot" ]
                        ]
                    ]
                ]



-- given a maybe tile, returns a tile


unwrapTile : Maybe Tile -> Tile
unwrapTile mt =
    case mt of
        Nothing ->
            E

        Just t ->
            t



-- given a tile, returns a maybe tile


wrapTile : Tile -> Maybe Tile
wrapTile t =
    case t of
        E ->
            Nothing

        Piece c l m ->
            Just (Piece c l m)



-- given the current tile, new tile, and model, make move


moveTo : Tile -> Model -> Msg -> Model
moveTo nt model msg =
    case ( model.checkers, msg ) of
        ( C (Board b bs) p1 p2 cp moves ct, Click p ) ->
            let
                pl =
                    physicalToLogical (PL p.x p.y) bs

                unchanged =
                    { model
                        | checkers = C (Board b bs) p1 p2 cp moves ct
                        , point = p
                    }

                ctToNt =
                    { model
                        | checkers = C (Board b bs) p1 p2 cp moves (wrapTile nt)
                        , point = p
                    }

                curTile =
                    unwrapTile ct
            in
            case ( nt, ct ) of
                ( Piece color _ _, Nothing ) ->
                    if equalColors color cp then
                        ctToNt

                    else
                        unchanged

                ( Piece color _ _, _ ) ->
                    if equalColors color cp then
                        case movePiece (C (Board b bs) p1 p2 cp moves ct) pl curTile of
                            Nothing ->
                                unchanged

                            Just newC ->
                                { model | checkers = newC, point = p }

                    else
                        unchanged

                ( _, Nothing ) ->
                    ctToNt

                ( E, _ ) ->
                    case movePiece (C (Board b bs) p1 p2 cp moves ct) pl curTile of
                        Nothing ->
                            unchanged

                        Just newC ->
                            { model | checkers = newC, point = p }

        ( _, _ ) ->
            model



-- check whether we need to make a bot move


updateBotMove : Model -> Model
updateBotMove model =
    case model.checkers of
        C _ _ _ B _ _ ->
            if checkBot model.player1 then
                case makeBotMove model.checkers of
                    Nothing ->
                        model

                    Just c ->
                        { model | checkers = c }

            else
                model

        C _ _ _ R _ _ ->
            if checkBot model.player2 then
                case makeBotMove model.checkers of
                    Nothing ->
                        model

                    Just c ->
                        { model | checkers = c }

            else
                model



-- check whether the game has ended


gameEnded : Model -> Model
gameEnded model =
    if endGame model.checkers then
        { model | gameOver = True }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.checkers, model.point ) of
        ( C (Board b (BS cs pr xOld yOld)) p1 p2 cp moves ct, p ) ->
            case msg of
                Click pNew ->
                    let
                        pl =
                            physicalToLogical (PL pNew.x pNew.y) (BS cs pr xOld yOld)

                        newTile =
                            boardRef (C (Board b (BS cs pr xOld yOld)) p1 p2 cp moves ct) pl

                        curTile =
                            unwrapTile ct
                    in
                    if equalTiles newTile curTile then
                        ( { model
                            | checkers = C (Board b (BS cs pr xOld yOld)) p1 p2 cp moves Nothing
                            , point = pNew
                          }
                        , Cmd.none
                        )

                    else
                        ( gameEnded (updateBotMove (moveTo newTile model msg)), Cmd.none )

                -- update if game ended after move
                Offset (x :: y :: _) ->
                    ( { model
                        | checkers = C (Board b (BS cs pr x y)) p1 p2 cp moves ct
                        , point = p
                      }
                    , Cmd.none
                    )

                UpdatePlayer1 p1s ->
                    ( { model | player1 = playerStrToP p1s "" 1 }, Cmd.none )

                UpdatePlayer2 p2s ->
                    ( { model | player2 = playerStrToP p2s "" 2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- transform player string to player type
-- we will make the black player = player 1


playerStrToP : String -> String -> Int -> Maybe Player
playerStrToP s name num =
    case ( s, num ) of
        ( "human", 1 ) ->
            Just (Human name B)

        ( "human", 2 ) ->
            Just (Human name R)

        ( "bot", 1 ) ->
            Just (Robot name B)

        ( "bot", 2 ) ->
            Just (Robot name R)

        _ ->
            Nothing



-- should not happen but default Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onClick
            (Decode.map
                (\point -> Click point)
                (Decode.map2
                    (\x y -> { x = x, y = y })
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
                )
            )
        , recieveBoardOffset Offset
        ]
