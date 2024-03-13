{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


width, height, offset :: Int
width = 300
height = 300
offset = 100

ballRadius :: Float
ballRadius = 10

paddleVelocity::Float
paddleVelocity = 150

data Movement = MovUp | MovDown | Stop deriving (Eq, Show)

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game
data PongGame = Game
  { ballLoc :: (Float, Float),  -- ^ Pong ball (x, y) location.
    ballVel :: (Float, Float),  -- ^ Pong ball (x, y) velocity. 
    player1 :: (Float, Movement),           -- ^ Left player paddle height.
    player2 :: (Float, Movement),           -- ^ Right player paddle height.
    suspended :: Bool,
    score1 :: Int,
    score2 :: Int
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0),
    ballVel = (60, -60),
    player1 = (0, Stop),
    player2 = (-80, Stop),
    suspended = True,
    score1 = 0,
    score2 = 0
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
    makeRectangle blue 120 $ fst $ player2 game,
    makeRectangle orange (-120) $ fst $ player1 game,
    scoreboard
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
                    
        scoreboard = translate (-50) (300 / 2 - 60) $ scale 0.3 0.3 $ color scorecolour $ text scoretext
        scoretext = if suspended game then (show $ score1 game) ++ " : " ++ (show $ score2 game) else ""
        scorecolour = red

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

movePaddles :: Float -> PongGame -> PongGame
movePaddles sec game = game { player1 = pl1, player2 = pl2}
      where
        pl1 = movePaddle sec ( player1 game )
        pl2 = movePaddle sec ( player2 game )

        movePaddle :: Float -> (Float, Movement) -> (Float, Movement)
        movePaddle sec (y, dir)  
          | dir == MovUp = let newy = y + sec * paddleVelocity in 
            if newy < 300 / 2 - 85 / 2 - 10 then (newy, dir) else (y, dir)
          | dir == MovDown = let newy = y - sec * paddleVelocity in 
            if newy > 300 / (-2) + 85 / 2 + 10 then (newy, dir) else (y, dir)
          | otherwise = (y, dir)

moveThings :: Float -> PongGame -> PongGame
moveThings sec = moveBall sec . movePaddles sec

fps :: Int
fps = 60

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update sec = detectDrop . paddleBounce . wallBounce . moveThings sec

-- animate :: Display -> Color -> (Float -> Picture) -> IO ()
-- simulate --> https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss-Interface-Pure-Simulate.html#v:simulate
-- play --> https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html#v:play
main :: IO ()
main = play window background fps initialState render handleKeys update

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
        paddleY1 = fst $ player1 game
        paddleY2 = fst $ player2 game
        leftCollision =   ballX < 300 / (-2) + 1.5 * 25 + ballRadius
                          && ballY < paddleY1 + 85 / 2
                          && ballY > paddleY1 - 85 / 2
        rightCollision =  ballX > 300 / 2 - 1.5 * 25 - ballRadius
                          && ballY < paddleY2 + 85 / 2
                          && ballY > paddleY2 - 85 / 2
        hit = if leftCollision then paddleY1 else paddleY2

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game = if suspended game then startNewGame game else game
--handleKeys (EventKey (Char 'p') _ _ _) game = if suspended game then game {ballVel = (0,0), suspended = not (suspended game)} else game
handleKeys (EventKey (Char 'w') Down _ _) game = game { player1 = (y, MovUp  ) } where y = fst $ player1 game
handleKeys (EventKey (Char 'w') Up _ _)   game = game { player1 = (y, Stop  ) } where y = fst $ player1 game
handleKeys (EventKey (Char 's') Down _ _) game = game { player1 = (y, MovDown) } where y = fst $ player1 game
handleKeys (EventKey (Char 's') Up _ _)   game = game { player1 = (y, Stop  ) } where y = fst $ player1 game
handleKeys (EventKey (SpecialKey KeyUp) Down _ _)   game = game { player2 = (y, MovUp  ) } where y = fst $ player2 game 
handleKeys (EventKey (SpecialKey KeyUp) Up _ _)     game = game { player2 = (y, Stop  ) } where y = fst $ player2 game
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2 = (y, MovDown) } where y = fst $ player2 game
handleKeys (EventKey (SpecialKey KeyDown) Up _ _)   game = game { player2 = (y, Stop  ) } where y = fst $ player2 game
handleKeys _ game = game

startNewGame :: PongGame -> PongGame
startNewGame game = game
  { 
    ballLoc = (0, 0),
    ballVel = (60, -60),
    suspended = False
  }

detectDrop :: PongGame -> PongGame
detectDrop game = if x > 300 / 2 - ballRadius && not susp
                  then game { score1 = 1 + score1 game , suspended = True }
                  else if x < 300 / (-2) + ballRadius && not susp
                    then game { score2 = 1 + score2 game , suspended = True }
                  else game
                  where 
                    susp = suspended game
                    (x, _) = ballLoc game