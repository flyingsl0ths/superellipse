{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Bifunctor (bimap)
import Raylib.Core (clearBackground, closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawLine)
import Raylib.Core.Text (drawText)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (black, rayWhite, red)

type Point = (Float, Float)

-- Roundness
n :: Float
n = 4

-- Determines the width
a :: Float
a = 1

-- Determines the height
b :: Float
b = 1

superEllipse :: Float -> (Float, Float)
superEllipse t = (x, y)
 where
  x = (abs . cos $ t) ** (2 / n) * a * (sign $ cos t)
  y = (abs . sin $ t) ** (2 / n) * b * (sign $ sin t)

-- Onlys gets the angle, requires conversion to x and y
-- superEllipse' :: Float -> Float
-- superEllipse' t = (left + right) ** (negate $ n / 1)
--  where
--   left = (abs . cos $ t / a) ** n
--   right = (abs . sin $ t / a) ** n

sign :: Float -> Float
sign w
  | w < 0 = -1
  | w > 0 = 1
  | otherwise = 0

getPoints :: Float -> Point -> Point -> [Point]
getPoints pointsToDraw (ox, oy) (width, height) = iter 0 []
 where
  twoPi = (pi * 2)
  angle = twoPi / pointsToDraw
  iter i acc
    | i <= twoPi =
        let ps' = bimap ((ox +) . (* width)) ((oy +) . (* height)) $ superEllipse i
         in iter (i + angle) (ps' : acc)
    | otherwise = acc

startup :: IO WindowResources
startup = do
  window <- initWindow 600 450 "Super Ellipse"
  setTargetFPS 60
  return window

ps :: [Point]
ps = getPoints 800 (300, 450 / 2) (200, 150)

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  let drawLine' (x1, y1) (x2, y2) = drawLine (floor x1) (floor y1) (floor x2) (floor y2) rayWhite
   in drawing
        ( do
            clearBackground black
            sequence_ $ zipWith drawLine' ps (drop 1 ps)
            let first = last ps
            let last' = head ps
            drawLine' first last'
            drawText "Lame'" 300 (450 `div` 2 + 30) 50 red
        )
        >> return window

shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
