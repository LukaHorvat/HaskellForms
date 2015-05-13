{-# LANGUAGE TemplateHaskell #-}
module Main where

import WinForms.Interface (startHost)
import WinForms.Types
import WinForms.Controls
import WinForms.Controls.Base

main :: IO ()
main = do
    form <- newForm
    button <- newButton
    --addChildControl form button
    --backColor button #= color 255 0 0 255
    --col <- get $ backColor button
    cs <- get $ controls form
    add cs button
    --print col
    print cs
