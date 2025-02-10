{-# LANGUAGE TemplateHaskell #-}

module Main where

import Raylib.Core (
    clearBackground,
    closeWindow,
    initWindow,
    setTargetFPS,
    windowShouldClose,
 )
import Raylib.Core.Text (drawText)
import Raylib.Util (
    WindowResources,
    drawing,
    raylibApplication,
 )
import Raylib.Util.Colors (black, rayWhite)

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
    x = (abs . cos $ t) ** (n / 2) * a * (sign $ cos t)
    y = (abs . sin $ t) ** (n / 2) * b * (sign $ sin t)

superEllipse' :: Float -> Float
superEllipse' t = (left + right) ** (negate $ n / 1)
  where
    left = (abs . cos $ t / a) ** n
    right = (abs . sin $ t / a) ** n

sign :: Float -> Float
sign w
    | w < 0 = -1
    | w > 0 = 1
    | otherwise = 0

startup :: IO WindowResources
startup = do
    window <- initWindow 600 450 "Super Ellipse"
    setTargetFPS 60
    return window

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
    drawing
        ( do
            clearBackground black
            drawText "Basic raylib window" 30 40 18 rayWhite
        )
        >> return window

shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
