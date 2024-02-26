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

-- color :: Color -> Picture -> Picture
-- pictures :: [Picture] -> Picture
-- translate :: Float -> Float -> Picture -> Picture
drawing :: Picture
drawing = pictures [
    ball red 8,
    walls,
    makeRectangle blue 120 (-20),
    makeRectangle orange (-120) 40
    ]
    where
        -- ball drawing
        ball :: Color -> Float -> Picture
        ball clr rad = translate (-10) 40 $ color clr $ circleSolid rad

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


main :: IO ()
main = display window background drawing