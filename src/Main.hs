{-# LANGUAGE TemplateHaskell #-}
module Main where

import WinForms.Interface (startHost)
import WinForms.Types
import WinForms.Controls
import WinForms.Controls.Base

main :: IO ()
main = do
    form <- newForm
    btn  <- newButton
    controls form >>= add btn
    text btn #= "Test"
    test <- text btn
    return ()
