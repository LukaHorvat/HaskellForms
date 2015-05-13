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
    cols <- get $ controls form
    add btn cols
    evt <- click btn
    handle evt (\_ args -> putStrLn "test")
