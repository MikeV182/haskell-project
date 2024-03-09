module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

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
  { ballLoc = (-10, 30),
    ballVel = (1, -3),
    player1 = 40,
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
    makeRectangle blue 120 $ player1 game,
    makeRectangle orange (-120) $ player2 game
    ]
    where
        -- ball drawing
        -- uncurry translate (1,2) Picture <==> translate 1 2 Picture
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 8
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

-- animate :: Display -> Color -> (Float -> Picture) -> IO ()
main :: IO ()
main = animate window background frame
    where
        frame :: Float -> Picture
        frame sec = render $ moveBall sec initialState