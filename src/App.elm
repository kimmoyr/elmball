module App exposing (..)

import Html exposing (text, div)
import Element exposing (Element, color, layers)
import Collage exposing (collage, rect, filled, move, circle)
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
    , ballRadius = 5
    , ballInitialVelocity = ( 70, -200 )
    }


type alias Position =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Size =
    ( Float, Float )


type alias Model =
    { paddlePos : Position
    , leftDown : Bool
    , rightDown : Bool
    , ballPos : Position
    , ballVelocity : Velocity
    }


init : ( Model, Cmd Msg )
init =
    ( { paddlePos = ( 0, -cfg.gameHalfHeight + cfg.paddleYOffset )
      , leftDown = False
      , rightDown = False
      , ballPos = ( 0, 0 )
      , ballVelocity = cfg.ballInitialVelocity
      }
    , Cmd.none
    )


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


isBallTouchingRect : Position -> Position -> Size -> Bool
isBallTouchingRect ( ballX, ballY ) ( rectX, rectY ) ( w, h ) =
    let
        r =
            cfg.ballRadius

        rectLeft =
            rectX - w / 2

        rectRight =
            rectX + w / 2

        rectTop =
            rectY + h / 2

        rectBottom =
            rectY - h / 2

        closestX =
            clamp rectLeft rectRight ballX

        closestY =
            clamp rectBottom rectTop ballY

        distX =
            abs (ballX - closestX)

        distY =
            abs (ballY - closestY)
    in
        distX ^ 2 + distY ^ 2 <= r ^ 2


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
            fst model.paddlePos
                + speed
                * inSeconds dt
                |> clamp leftLimit rightLimit
    in
        { model | paddlePos = ( newPaddleX, snd model.paddlePos ) }


updateBall : Time -> Model -> Model
updateBall dt model =
    let
        touchingPaddle =
            isBallTouchingRect model.ballPos model.paddlePos ( cfg.paddleWidth, cfg.paddleHeight )

        ( vx, vy ) =
            if touchingPaddle then
                ( fst model.ballVelocity, -(snd model.ballVelocity) )
            else
                model.ballVelocity

        ( x, y ) =
            model.ballPos

        newX =
            x + vx * inSeconds dt

        newY =
            y + vy * inSeconds dt
    in
        { model
            | ballPos = ( newX, newY )
            , ballVelocity = ( vx, vy )
        }


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
            let
                newModel =
                    model
                        |> updatePaddlePos dt
                        |> updateBall dt
            in
                ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


background : Collage.Form
background =
    rect (toFloat cfg.gameWidth) (toFloat cfg.gameHeight)
        |> filled black


drawPaddle : Position -> Collage.Form
drawPaddle pos =
    rect cfg.paddleWidth cfg.paddleHeight
        |> filled white
        |> move pos


drawBall : Position -> Collage.Form
drawBall pos =
    circle cfg.ballRadius
        |> filled white
        |> move pos


view : Model -> Html.Html Msg
view model =
    let
        paddle =
            drawPaddle model.paddlePos

        ball =
            drawBall model.ballPos

        elements =
            [ background, paddle, ball ]
    in
        div []
            [ collage cfg.gameWidth cfg.gameHeight elements |> Element.toHtml
            ]
