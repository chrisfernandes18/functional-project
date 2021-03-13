port module Render exposing (main)

-- Imports -----------------------------------------------------------

import Array
import Browser
import Browser.Events
import Debug
import Html exposing (Html)
import Html.Attributes as HAttrs
import Html.Events as HEvents
import Json.Decode as Decode
import Logic exposing (..)
import Structs exposing (..)
import Svg exposing (..)
import Svg.Attributes as SAttrs



----------------------------------------------------------------------


type alias Point =
    { x : Float, y : Float }


type Model
    = M Checkers Point


type Msg
    = Click Point
    | Offset (List Float)
    | FormSubmitted
    | Info { p1 : String, p2 : String }


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
    M testCheckers { x = 0, y = 0 }


boardToHTML : Board -> Html Msg
boardToHTML b =
    case b of
        Board arr (BS cs pr _ _) ->
            let
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

                                        Piece color (LL _ _) move ->
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
                                                                        [ swidth, sheight, SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs), SAttrs.class "blackPieceKing" ]
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
                                                                        [ swidth, sheight, SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs), SAttrs.class "blackPiece" ]
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
                                                                        [ swidth, sheight, SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs), SAttrs.class "redPieceKing" ]
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
                                                                        [ swidth, sheight, SAttrs.viewBox ("0 0 " ++ String.fromFloat cs ++ " " ++ String.fromFloat cs), SAttrs.class "redPiece" ]
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
    case model of
        M (C (Board b (BS cs pr mx my)) p1 p2 cp moves ct) p ->
            let
                everything =
                    Debug.toString model
            in
            Html.div
                [ HAttrs.style "text-align" "center" ]
                [ Html.text everything
                , Html.div
                    []
                    [ boardToHTML (Board b (BS cs pr mx my)) ]
                , Html.form
                    [ HAttrs.id "players" ]
                    [ Html.input
                        [ HAttrs.type_ "text", HAttrs.name "player1", HAttrs.placeholder "Player 1" ]
                        []
                    , Html.input
                        [ HAttrs.type_ "text", HAttrs.name "player2", HAttrs.placeholder "Player 2" ]
                        []
                    , Html.br [] []
                    , Html.select
                        [ HAttrs.id "choice1" ]
                        [ Html.option [ HAttrs.value "human" ] [ Html.text "Human" ]
                        , Html.option [ HAttrs.value "robot" ] [ Html.text "Robot" ]
                        ]
                    , Html.select
                        [ HAttrs.id "choice2" ]
                        [ Html.option [ HAttrs.value "human" ] [ Html.text "Human" ]
                        , Html.option [ HAttrs.value "robot" ] [ Html.text "Robot" ]
                        ]
                    , Html.br [] []
                    , Html.input
                        [ HAttrs.type_ "submit" ]
                        []
                    ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( M (C (Board b bs) _ _ cp moves ct) p, Info rec ) ->
            ( M (C (Board b bs) (Just (Human rec.p1 B)) (Just (Human rec.p2 R)) cp moves ct) p, Cmd.none )

        ( M (C (Board b bs) p1 p2 cp moves ct) _, Click p ) ->
            let
                pl =
                    physicalToLogical (PL p.x p.y) bs

                nt =
                    case boardRef (C (Board b bs) p1 p2 cp moves ct) pl of
                        E ->
                            Nothing

                        Piece c l m ->
                            Just (Piece c l m)

                newTile =
                    case nt of
                        Nothing ->
                            E

                        Just t ->
                            t

                curTile =
                    case ct of
                        Nothing ->
                            E

                        Just t ->
                            t
            in
            if equalTiles newTile curTile then
                ( M (C (Board b bs) p1 p2 cp moves Nothing) p, Cmd.none )

            else
                case newTile of
                    Piece color _ _ ->
                        if equalColors color cp then
                            case ct of
                                Nothing ->
                                    ( M (C (Board b bs) p1 p2 cp moves nt) p, Cmd.none )

                                _ ->
                                    case movePiece (C (Board b bs) p1 p2 cp moves ct) pl curTile of
                                        Nothing ->
                                            ( M (C (Board b bs) p1 p2 cp moves ct) p, Cmd.none )

                                        Just newC ->
                                            ( M newC p, Cmd.none )

                        else
                            ( M (C (Board b bs) p1 p2 cp moves ct) p, Cmd.none )

                    E ->
                        case ct of
                            Nothing ->
                                ( M (C (Board b bs) p1 p2 cp moves nt) p, Cmd.none )

                            _ ->
                                case movePiece (C (Board b bs) p1 p2 cp moves ct) pl curTile of
                                    Nothing ->
                                        ( M (C (Board b bs) p1 p2 cp moves ct) p, Cmd.none )

                                    Just newC ->
                                        ( M newC p, Cmd.none )

        ( M (C (Board b (BS cs pr _ _)) p1 p2 cp moves ct) p, Offset (x :: y :: _) ) ->
            ( M (C (Board b (BS cs pr x y)) p1 p2 cp moves ct) p, Cmd.none )

        ( M (C (Board b bs) p1 p2 cp moves ct) p, _ ) ->
            ( M (C (Board b bs) p1 p2 cp moves ct) p, Cmd.none )


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
