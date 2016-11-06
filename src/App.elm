module App exposing (..)

import Html exposing (text, div)
import Element exposing (Element, color, layers, centered, leftAligned, rightAligned)
import Collage exposing (Form, toForm, collage, group, rect, filled, outlined, defaultLine, move, circle, scale)
import Color exposing (Color, rgba, black, white)
import Text
import Time exposing (Time, inSeconds)
import Keyboard
import AnimationFrame
import Window
import Task
import List.Extra


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
    , lives : Int
    }
cfg =
    { gameWidth = 800
    , gameHeight = 600
    , gameHalfWidth = 400
    , gameHalfHeight = 300
    , paddleWidth = 100
    , paddleHeight = 15
    , paddleYOffset = 50
    , paddleSpeed = 500
    , ballRadius = 5
    , ballInitialPos = ( 0, 0 )
    , ballInitialVelocity = ( 5, -300 )
    , brickSize = ( 50, 20 )
    , brickOffsetX = 50
    , brickOffsetY = 20
    , lives = 3
    }


type alias Position =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Size =
    ( Float, Float )


type alias Brick =
    { pos : Position
    , color : Color
    , points : Int
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
    , lives : Int
    }


b : Int -> Int -> Color -> Int -> Brick
b x y color points =
    Brick
        ( -cfg.gameHalfWidth + (toFloat x) * cfg.brickOffsetX + (fst cfg.brickSize) / 2
        , cfg.gameHalfHeight - (toFloat y) * cfg.brickOffsetY - (snd cfg.brickSize) / 2
        )
        color
        points
        False


paddleInitialPos : ( number, Float )
paddleInitialPos =
    ( 0, -cfg.gameHalfHeight + cfg.paddleYOffset )


brickRow : Int -> Color -> Int -> List Brick
brickRow row color points =
    List.map (\column -> b column row color points) [0..15]


initialBricks : List Brick
initialBricks =
    (brickRow 3 Color.lightRed 5)
        ++ (brickRow 4 Color.lightOrange 4)
        ++ (brickRow 5 Color.lightYellow 3)
        ++ (brickRow 6 Color.lightGreen 2)
        ++ (brickRow 7 Color.lightBlue 1)


init : ( Model, Cmd Msg )
init =
    ( { paddlePos = ( 0, -cfg.gameHalfHeight + cfg.paddleYOffset )
      , leftDown = False
      , rightDown = False
      , ballPos = ( 0, 0 )
      , ballVelocity = ( 0, 0 )
      , windowSize = Window.Size cfg.gameWidth cfg.gameHeight
      , gameState = NotStarted
      , lives = cfg.lives
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


collisionSideToInt : CollisionSide -> number
collisionSideToInt side =
    case side of
        NoCollision ->
            0

        Left ->
            1

        Right ->
            2

        Top ->
            3

        Bottom ->
            4


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
    ballRectCollisionSide ballPos rectPos rectSize /= NoCollision


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
            (snd model.ballPos) + cfg.ballRadius / 2 >= cfg.gameHalfHeight

        touchingLeftWall =
            (fst model.ballPos) - cfg.ballRadius / 2 <= -cfg.gameHalfWidth

        touchingRightWall =
            (fst model.ballPos) + cfg.ballRadius / 2 >= cfg.gameHalfWidth

        ballLost =
            snd model.ballPos < -cfg.gameHalfHeight

        brickHits =
            hitBricks model.ballPos model.bricks

        newBricks =
            List.map fst brickHits

        brickCollisionSides =
            List.map snd brickHits
                |> List.filter (\side -> side /= NoCollision)
                |> List.Extra.uniqueBy collisionSideToInt

        ( oldVx, oldVy ) =
            model.ballVelocity

        ( vx, vy ) =
            if ballLost then
                cfg.ballInitialVelocity
            else if not (List.isEmpty brickCollisionSides) then
                List.foldl updateVelocity model.ballVelocity brickCollisionSides
            else if touchingPaddle then
                updateBallVelocityOnPaddleHit model.paddlePos model.ballPos model.ballVelocity
            else if touchingCeiling then
                ( oldVx, -(abs oldVy) )
            else if touchingLeftWall then
                ( abs oldVx, oldVy )
            else if touchingRightWall then
                ( -(abs oldVx), oldVy )
            else
                model.ballVelocity

        ( x, y ) =
            if ballLost then
                cfg.ballInitialPos
            else
                model.ballPos

        ( newPaddlePos, newLives ) =
            if ballLost then
                ( paddleInitialPos, model.lives - 1 )
            else
                ( model.paddlePos, model.lives )

        brokenBrickCount =
            model.bricks
                |> List.filter .broken
                |> List.length

        totalBrickCount =
            List.length model.bricks

        speedUpFactor =
            ((toFloat brokenBrickCount) / (toFloat totalBrickCount) + 1) ^ 2

        newX =
            x + vx * inSeconds dt * speedUpFactor

        newY =
            y + vy * inSeconds dt * speedUpFactor
    in
        { model
            | ballPos = ( newX, newY )
            , ballVelocity = ( vx, vy )
            , bricks = newBricks
            , paddlePos = newPaddlePos
            , lives = newLives
        }


startNewGame : Model -> Model
startNewGame model =
    { model
        | gameState = Playing
        , lives = 3
        , paddlePos = paddleInitialPos
        , ballPos = cfg.ballInitialPos
        , ballVelocity = cfg.ballInitialVelocity
        , bricks = initialBricks
    }


updateGameState : Model -> Model
updateGameState model =
    let
        newState =
            if model.gameState == Playing then
                if List.all .broken model.bricks then
                    Won
                else if model.lives == 0 then
                    Lost
                else
                    Playing
            else
                model.gameState

        newBallVelocity =
            if newState /= Playing then
                ( 0, 0 )
            else
                model.ballVelocity
    in
        { model | gameState = newState, ballVelocity = newBallVelocity }


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
                        |> updateGameState

                WindowSize size ->
                    { model | windowSize = size }

                NoOp ->
                    model
    in
        ( newModel, Cmd.none )


score : Model -> Int
score model =
    model.bricks
        |> List.filter .broken
        |> List.map .points
        |> List.sum


background : Form
background =
    rect (toFloat cfg.gameWidth) (toFloat cfg.gameHeight)
        |> filled black


drawPaddle : Position -> Form
drawPaddle pos =
    rect cfg.paddleWidth cfg.paddleHeight
        |> filled white
        |> move pos


drawBall : Position -> Form
drawBall pos =
    circle cfg.ballRadius
        |> filled white
        |> move pos


drawBrick : Brick -> Form
drawBrick brick =
    let
        ( w, h ) =
            cfg.brickSize

        fill =
            rect w h
                |> filled brick.color
                |> move brick.pos

        outline =
            rect (w - 1) (h - 1)
                |> outlined defaultLine
                |> move brick.pos
    in
        group [ fill, outline ]


txt : String -> Form
txt string =
    Text.fromString string
        |> Text.color white
        |> Text.height 30
        |> centered
        |> toForm


drawLivesText : Int -> Form
drawLivesText lives =
    toString lives
        |> Text.fromString
        |> Text.color white
        |> Text.height 15
        |> leftAligned
        |> toForm
        |> move ( -cfg.gameHalfWidth + 20, -cfg.gameHalfHeight + 20 )


drawScoreText : Int -> Form
drawScoreText score =
    toString score
        |> Text.fromString
        |> Text.color white
        |> Text.height 15
        |> rightAligned
        |> toForm
        |> move ( cfg.gameHalfWidth - 20, -cfg.gameHalfHeight + 20 )


drawStateText : GameState -> Form
drawStateText gameState =
    case gameState of
        NotStarted ->
            txt "Press SPACE to start"

        Playing ->
            txt ""

        Won ->
            txt "You Win!"

        Lost ->
            txt "Game Over!"


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

        stateText =
            drawStateText model.gameState

        livesText =
            drawLivesText model.lives

        scoreText =
            drawScoreText (score model)

        elements =
            [ background, paddle, ball, stateText, livesText, scoreText ] ++ bricks
    in
        div []
            [ displayFullScreen model.windowSize (group elements) |> Element.toHtml
            ]
