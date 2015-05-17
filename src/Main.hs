{-# LANGUAGE TemplateHaskell #-}
module Main where

import WinForms.Interface (startHost)
import WinForms.Types
import WinForms.Controls
import WinForms.Controls.Base

main :: IO ()
main = do
    form <- newForm
    pb   <- newPictureBox
    controls form >>= add pb
    bmp  <- newBitmap 500 500
    image pb #= bmp
    red  <- newSolidBrush $ color 255 0 0 255
    graphicsFromImage bmp >>= drawString "Hello world" (font "Consolas" 12) red (pointF 0 0)
    refresh form
