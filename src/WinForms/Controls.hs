{-# LANGUAGE TemplateHaskell, DataKinds #-}
module WinForms.Controls where

import WinForms.Types
import WinForms.Interface
import WinForms.Controls.Base

(#=) :: Marshal a => Property GettableSettable a -> a -> IO ()
(Property _ setter) #= val = setter val

get :: Marshal a => Property any a -> IO a
get (Property getter _) = getter

makeControl "Form"
makeControl "Button"
makeShared "ControlCollection"

location :: Control a => a -> Property GettableSettable Point
location = property "Location"

backColor :: Control a => a -> Property GettableSettable Color
backColor = property "BackColor"

controls :: Control a => a -> Property Gettable ControlCollection
controls = property "Controls"

add :: Control a => ControlCollection -> a -> IO ()
add col con = do
    NoResponse <- invoke col "Add" [toValue con]
    return ()
