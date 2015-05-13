{-# LANGUAGE TemplateHaskell #-}
module Main where

import WinForms.Interface (startHost)
import WinForms.Types
import WinForms.Controls
import WinForms.Controls.Base

main :: IO ()
main = do
    form <- newForm
    typ  <- getType form
    name <- toString typ
    print name
    typTyp  <- getType typ
    typName <- toString typTyp
    print typName
