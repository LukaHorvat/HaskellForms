{-# LANGUAGE TemplateHaskell, DataKinds, MultiParamTypeClasses, FlexibleContexts #-}
module WinForms.Controls where

import WinForms.Types
import WinForms.Interface
import WinForms.Controls.Base

(#=) :: Marshal a => Property GettableSettable a -> a -> IO ()
(Property _ setter) #= val = setter val

get :: Marshal a => Property any a -> IO a
get (Property getter _) = getter

makeShared "ControlCollection"
makeShared "Type"
makeInstantiable "Form"
makeInstantiable "Button"

toString :: Shared a => a -> IO String
toString a = invokeMarshal a "ToString" []

getType :: Shared a => a -> IO Type
getType a = invokeMarshal a "GetType" []

class Shared a => ControlC a where
    location :: a -> Property GettableSettable Point
    location = property "Location"

    backColor :: a -> Property GettableSettable Color
    backColor = property "BackColor"

    controls :: a -> Property Gettable ControlCollection
    controls = property "Controls"

instance ControlC Form
instance ControlC Button

add :: ControlC a => ControlCollection -> a -> IO ()
add col con = invokeVoid col "Add" [toValue con]
