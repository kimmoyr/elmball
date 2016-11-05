module App exposing (..)

import Html exposing (text, div)
import Element exposing (Element, color, layers)
import Collage exposing (collage, rect, filled, move)
import Color exposing (black, white)
import Time exposing (Time, inSeconds)
import Keyboard
import AnimationFrame


cfg =
    { gameWidth = 800
    , gameHeight = 600
    , gameHalfWidth = 400
    , gameHalfHeight = 300
    , paddleWidth = 100
    , paddleHeight = 15
    , paddleYOffset = 50
    , paddleSpeed = 200
    }


type alias Model =
    { paddleX : Float
    , leftDown : Bool
    , rightDown : Bool
    }


type Msg
    = NoOp
    | LeftDown
    | LeftUp
    | RightDown
    | RightUp
    | Tick Time


keyDownMsg : Keyboard.KeyCode -> Msg
keyDownMsg keyCode =
    case keyCode of
        37 ->
            LeftDown

        39 ->
            RightDown

        _ ->
            NoOp


keyUpMsg : Keyboard.KeyCode -> Msg
keyUpMsg keyCode =
    case keyCode of
        37 ->
            LeftUp

        39 ->
            RightUp

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs keyDownMsg
        , Keyboard.ups keyUpMsg
        , AnimationFrame.diffs Tick
        ]


updatePaddlePos : Time -> Model -> Model
updatePaddlePos dt model =
    let
        speed =
            if model.leftDown then
                -cfg.paddleSpeed
            else if model.rightDown then
                cfg.paddleSpeed
            else
                0

        leftLimit =
            -cfg.gameHalfWidth + cfg.paddleWidth / 2

        rightLimit =
            cfg.gameHalfWidth - cfg.paddleWidth / 2

        newPaddleX =
            model.paddleX
                + speed
                * inSeconds dt
                |> clamp leftLimit rightLimit
    in
        { model | paddleX = newPaddleX }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftDown ->
            ( { model | leftDown = True }, Cmd.none )

        LeftUp ->
            ( { model | leftDown = False }, Cmd.none )

        RightDown ->
            ( { model | rightDown = True }, Cmd.none )

        RightUp ->
            ( { model | rightDown = False }, Cmd.none )

        Tick dt ->
            ( updatePaddlePos dt model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { paddleX = 0
      , leftDown = False
      , rightDown = False
      }
    , Cmd.none
    )


background : Collage.Form
background =
    rect (toFloat cfg.gameWidth) (toFloat cfg.gameHeight) |> filled black


drawPaddle : Float -> Collage.Form
drawPaddle x =
    rect cfg.paddleWidth cfg.paddleHeight
        |> filled white
        |> move ( x, -cfg.gameHalfHeight + cfg.paddleYOffset )


view : Model -> Html.Html Msg
view model =
    let
        paddle =
            drawPaddle model.paddleX
    in
        div []
            [ collage cfg.gameWidth cfg.gameHeight [ background, paddle ] |> Element.toHtml
            ]
