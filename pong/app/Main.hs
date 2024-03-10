module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


width, height, offset :: Int
width = 300
height = 300
offset = 100

ballRadius :: Float
ballRadius = 10

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game
data PongGame = Game
  { ballLoc :: (Float, Float),  -- ^ Pong ball (x, y) location.
    ballVel :: (Float, Float),  -- ^ Pong ball (x, y) velocity. 
    player1 :: Float,           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
    player2 :: Float           -- ^ Right player paddle height.
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0),
    ballVel = (30, -30),
    player1 = 0,
    player2 = -80
  }

-- color :: Color -> Picture -> Picture
-- pictures :: [Picture] -> Picture
-- translate :: Float -> Float -> Picture -> Picture
-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game = pictures [
    ball,
    walls,
    makeRectangle blue 120 $ player2 game,
    makeRectangle orange (-120) $ player1 game
    ]
    where
        -- ball drawing
        -- uncurry translate (1,2) Picture <==> translate 1 2 Picture
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
        ballColor = dark red

        -- walls in window
        wall :: Float -> Picture
        wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        -- function to draw a rectangle and its outline
        makeRectangle :: Color -> Float -> Float -> Picture
        makeRectangle clr x y = pictures [
            translate x y $ color clr $ rectangleSolid 25 85,
            translate x y $ color mainColor $ rectangleSolid 20 80
            ]
            where
                mainColor = white

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall sec game = game {ballLoc = (x', y')}
    where
        (x, y) = ballLoc game
        (velX, velY) = ballVel game

        x' = x + velX * sec
        y' = y + velY * sec

fps :: Int
fps = 60

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> PongGame -> PongGame
update _ sec = paddleBounce . wallBounce . moveBall sec

-- animate :: Display -> Color -> (Float -> Picture) -> IO ()
-- simulate --> https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss-Interface-Pure-Simulate.html#v:simulate
main :: IO ()
main = simulate window background fps initialState render update

type Radius = Float
type PosOfBall = (Float, Float)

collisionWalls :: PosOfBall -> Radius -> Bool
collisionWalls (_,y) radius = topWall || bottomWall
  where -- using fromIntegral to convert numbers from Int to Float type
    topWall = y - radius <= -fromIntegral height / 2
    bottomWall = y + radius >= fromIntegral height / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = ballRadius

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if collisionWalls (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy


paddleBounce :: PongGame -> PongGame
paddleBounce game = game {ballVel = vel}
    where
        (vx, vy) = ballVel game
        (x, y)   = ballLoc game
        (hitOrNot, paddleY) = paddleCollision game
        vel = if hitOrNot
            then (-vx, archvy)
            else (vx, vy)
        archvy = abs vx * tan (normalAngle + archedAngle)
        normalAngle = atan (vy / abs vx)
        archedAngle = (y - paddleY) / (85 * 0.7)

paddleCollision :: PongGame -> (Bool, Float)
paddleCollision game = (leftCollision || rightCollision, hit)
    where
        (ballX, ballY) = ballLoc game
        paddleY1 = player1 game
        paddleY2 = player2 game
        leftCollision =   ballX < 300 / (-2) + 1.5 * 25 + ballRadius
                          && ballY < paddleY1 + 85 / 2
                          && ballY > paddleY1 - 85 / 2
        rightCollision =  ballX > 300 / 2 - 1.5 * 25 - ballRadius
                          && ballY < paddleY2 + 85 / 2
                          && ballY > paddleY2 - 85 / 2
        hit = if leftCollision then paddleY1 else paddleY2