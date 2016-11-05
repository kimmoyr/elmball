module App exposing (..)

import Html exposing (text, div)
import Element exposing (Element, color, layers)
import Collage exposing (Form, collage, group, rect, filled, move, circle, scale)
import Color exposing (rgba, black, white, red)
import Time exposing (Time, inSeconds)
import Keyboard
import AnimationFrame
import Window
import Task


cfg :
    { ballInitialPos : Velocity
    , ballInitialVelocity : Velocity
    , ballRadius : Float
    , gameHalfHeight : Float
    , gameHalfWidth : Float
    , gameWidth : Int
    , paddleHeight : Float
    , paddleSpeed : number
    , paddleWidth : Float
    , paddleYOffset : Float
    , gameHeight : Int
    , brickSize : Size
    , brickOffsetX : Float
    , brickOffsetY : Float
    }
cfg =
    { gameWidth = 800
    , gameHeight = 600
    , gameHalfWidth = 400
    , gameHalfHeight = 300
    , paddleWidth = 100
    , paddleHeight = 15
    , paddleYOffset = 50
    , paddleSpeed = 250
    , ballRadius = 5
    , ballInitialPos = ( 0, 0 )
    , ballInitialVelocity = ( 0, -300 )
    , brickSize = ( 40, 20 )
    , brickOffsetX = 61
    , brickOffsetY = 35
    }


type alias Position =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Size =
    ( Float, Float )


type alias Brick =
    { pos : Position
    , broken : Bool
    }


type GameState
    = NotStarted
    | Playing
    | Won
    | Lost


type alias Model =
    { paddlePos : Position
    , leftDown : Bool
    , rightDown : Bool
    , ballPos : Position
    , ballVelocity : Velocity
    , bricks : List Brick
    , windowSize : Window.Size
    , gameState : GameState
    }


b : Int -> Int -> Brick
b x y =
    Brick
        ( -cfg.gameHalfWidth + (toFloat x + 1) * cfg.brickOffsetX
        , cfg.gameHalfHeight - (toFloat y + 1) * cfg.brickOffsetY
        )
        False


paddleInitialPos : ( number, Float )
paddleInitialPos =
    ( 0, -cfg.gameHalfHeight + cfg.paddleYOffset )


initialBricks : List Brick
initialBricks =
    [ b 0 1
    , b 1 1
    , b 2 1
    , b 3 1
    , b 4 1
    , b 5 1
    , b 6 1
    , b 7 1
    , b 8 1
    , b 9 1
    , b 10 1
    , b 11 1
    , b 0 2
    , b 1 2
    , b 2 2
    , b 3 2
    , b 4 2
    , b 5 2
    , b 6 2
    , b 7 2
    , b 8 2
    , b 9 2
    , b 10 2
    , b 11 2
    , b 0 3
    , b 1 3
    , b 2 3
    , b 3 3
    , b 4 3
    , b 5 3
    , b 6 3
    , b 7 3
    , b 8 3
    , b 9 3
    , b 10 3
    , b 11 3
    ]


init : ( Model, Cmd Msg )
init =
    ( { paddlePos = ( 0, -cfg.gameHalfHeight + cfg.paddleYOffset )
      , leftDown = False
      , rightDown = False
      , ballPos = ( 0, 0 )
      , ballVelocity = ( 0, 0 )
      , windowSize = Window.Size cfg.gameWidth cfg.gameHeight
      , gameState = NotStarted
      , bricks = initialBricks
      }
    , Task.perform (always NoOp) WindowSize Window.size
    )


type Msg
    = NoOp
    | SpaceDown
    | LeftDown
    | LeftUp
    | RightDown
    | RightUp
    | Tick Time
    | WindowSize Window.Size


keyDownMsg : Keyboard.KeyCode -> Msg
keyDownMsg keyCode =
    case keyCode of
        32 ->
            SpaceDown

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
        , Window.resizes WindowSize
        ]


type CollisionSide
    = NoCollision
    | Left
    | Right
    | Top
    | Bottom


ballRectCollisionSide : Position -> Position -> Size -> CollisionSide
ballRectCollisionSide ( ballX, ballY ) ( rectX, rectY ) ( w, h ) =
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

        hit =
            distX ^ 2 + distY ^ 2 <= r ^ 2
    in
        if hit then
            if ballY <= rectBottom then
                Bottom
            else if ballY >= rectTop then
                Top
            else if ballX <= rectLeft then
                Left
            else
                Right
        else
            NoCollision


updateVelocity : CollisionSide -> Velocity -> Velocity
updateVelocity collision ( vx, vy ) =
    case collision of
        Top ->
            ( vx, -vy )

        Bottom ->
            ( vx, -vy )

        Left ->
            ( -vx, vy )

        Right ->
            ( -vx, vy )

        NoCollision ->
            ( vx, vy )


isBallTouchingRect : Position -> Position -> Size -> Bool
isBallTouchingRect ballPos rectPos rectSize =
    case ballRectCollisionSide ballPos rectPos rectSize of
        NoCollision ->
            False

        _ ->
            True


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


hitBrick : Position -> Brick -> ( Brick, CollisionSide )
hitBrick ballPos brick =
    let
        collisionSide =
            if brick.broken then
                NoCollision
            else
                ballRectCollisionSide ballPos brick.pos cfg.brickSize

        newBrick =
            case collisionSide of
                NoCollision ->
                    brick

                _ ->
                    { brick | broken = True }
    in
        ( newBrick, collisionSide )


hitBricks : Position -> List Brick -> List ( Brick, CollisionSide )
hitBricks ballPos =
    List.map (hitBrick ballPos)


updateBallVelocityOnPaddleHit : Position -> Position -> Velocity -> Velocity
updateBallVelocityOnPaddleHit ( paddleX, paddleY ) ( ballX, ballY ) ( vx, vy ) =
    let
        speed =
            sqrt (vx ^ 2 + vy ^ 2)

        posX =
            (ballX - paddleX) / (cfg.paddleWidth / 2)

        newVx =
            0.75 * posX * speed

        newVy =
            sqrt (speed ^ 2 - newVx ^ 2)
    in
        ( newVx, newVy )


updateBallAndBricks : Time -> Model -> Model
updateBallAndBricks dt model =
    let
        touchingPaddle =
            isBallTouchingRect model.ballPos model.paddlePos ( cfg.paddleWidth, cfg.paddleHeight )

        touchingCeiling =
            isBallTouchingRect model.ballPos ( 0, cfg.gameHalfHeight ) ( toFloat cfg.gameWidth, 0 )

        touchingLeftWall =
            isBallTouchingRect model.ballPos ( -cfg.gameHalfWidth, 0 ) ( 0, toFloat cfg.gameHeight )

        touchingRightWall =
            isBallTouchingRect model.ballPos ( cfg.gameHalfWidth, 0 ) ( 0, toFloat cfg.gameHeight )

        ballLost =
            snd model.ballPos < -cfg.gameHalfHeight

        brickHits =
            hitBricks model.ballPos model.bricks

        newBricks =
            List.map fst brickHits

        brickCollisionSides =
            List.map snd brickHits
                |> List.filter (\side -> side /= NoCollision)

        ( vx, vy ) =
            if ballLost then
                cfg.ballInitialVelocity
            else if not (List.isEmpty brickCollisionSides) then
                List.foldl updateVelocity model.ballVelocity brickCollisionSides
            else if touchingPaddle then
                updateBallVelocityOnPaddleHit model.paddlePos model.ballPos model.ballVelocity
            else if touchingCeiling then
                ( fst model.ballVelocity, -(snd model.ballVelocity) )
            else if touchingLeftWall || touchingRightWall then
                ( -(fst model.ballVelocity), snd model.ballVelocity )
            else
                model.ballVelocity

        ( x, y ) =
            if ballLost then
                cfg.ballInitialPos
            else
                model.ballPos

        newPaddlePos =
            if ballLost then
                paddleInitialPos
            else
                model.paddlePos

        newX =
            x + vx * inSeconds dt

        newY =
            y + vy * inSeconds dt
    in
        { model
            | ballPos = ( newX, newY )
            , ballVelocity = ( vx, vy )
            , bricks = newBricks
            , paddlePos = newPaddlePos
        }


startNewGame : Model -> Model
startNewGame model =
    { model
        | gameState = Playing
        , paddlePos = paddleInitialPos
        , ballPos = cfg.ballInitialPos
        , ballVelocity = cfg.ballInitialVelocity
        , bricks = initialBricks
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                SpaceDown ->
                    if model.gameState /= Playing then
                        startNewGame model
                    else
                        model

                LeftDown ->
                    { model | leftDown = True }

                LeftUp ->
                    { model | leftDown = False }

                RightDown ->
                    { model | rightDown = True }

                RightUp ->
                    { model | rightDown = False }

                Tick dt ->
                    model
                        |> updatePaddlePos dt
                        |> updateBallAndBricks dt

                WindowSize size ->
                    { model | windowSize = size }

                NoOp ->
                    model
    in
        ( newModel, Cmd.none )


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


drawBrick : Brick -> Collage.Form
drawBrick brick =
    let
        ( w, h ) =
            cfg.brickSize
    in
        rect w h
            |> filled red
            |> move brick.pos


emptyForm : Form
emptyForm =
    rect 0 0 |> filled (rgba 0 0 0 0)


displayFullScreen : Window.Size -> Form -> Element
displayFullScreen { width, height } content =
    let
        gameScale =
            min (toFloat width / toFloat cfg.gameWidth)
                (toFloat height / toFloat cfg.gameHeight)
    in
        collage width height [ content |> scale gameScale ]


view : Model -> Html.Html Msg
view model =
    let
        paddle =
            drawPaddle model.paddlePos

        ball =
            if model.gameState == Playing then
                drawBall model.ballPos
            else
                emptyForm

        bricks =
            List.filter (\brick -> not brick.broken) model.bricks
                |> List.map drawBrick

        elements =
            [ background, paddle, ball ] ++ bricks
    in
        div []
            [ displayFullScreen model.windowSize (group elements) |> Element.toHtml
            ]
