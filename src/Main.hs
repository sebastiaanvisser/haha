module Main where

import Terminal
import Geometry
import Bitmap
import Plot

-- The picture.

geom :: (Floating u, Ord u, Enum u) => u -> Poly u
geom r = rotate r (Point 40 40) $ star (Point 40 40) 6 (r*0.5) r

geom2 :: (Floating u, Ord u, Enum u) => u -> Poly u
geom2 r = rotate r (Point 40 40) $ star (Point 40 40) 6 (r*2)   r

geom3 :: (Floating u, Ord u, Enum u) => u -> Poly u
geom3 r = rotate r (Point 40 40) $ star (Point 40 40) 6  r      r

myBmp :: Double -> Bitmap Double Pixel
myBmp r =
    drawPoly (rotate r (Point 40 40) lambda) (Pixel 'x' blue)
  $ empty

lambda = Poly [
    Point 25 22
  , Point 24 22
  , Point 25 15
  , Point 27 13
  , Point 33 13
  , Point 36 16
  , Point 37 18
  , Point 49 53
  , Point 52 57
  , Point 56 57
  , Point 58 55
  , Point 58 53
  , Point 60 53
  , Point 60 58
  , Point 59 61
  , Point 57 63
  , Point 55 64
  , Point 52 64
  , Point 50 63
  , Point 48 60
  , Point 41 41
  , Point 30 63
  , Point 22 63
  , Point 37 29
  , Point 36 25
  , Point 35 22
  , Point 34 20
  , Point 31 18
  , Point 28 19
  , Point 25 21
  , Point 25 22
  ]





-- The renderer.

main :: IO ()
main = loop 0.0

loop :: Double -> IO ()
loop r = do
  putStr $ move 1 (1::Int)
  putStrLn $ plot (myBmp r)
  loop (r + 0.1)

plot :: Bitmap Double Pixel -> String
plot = string False screen (Point 1 1) " " "" . list 0.5 view
  where
    screen = Rect (Point 1 1) (Point 80 40)
    view   = Rect (Point 1 1) (Point 80 80)


