{-# LANGUAGE TemplateHaskell, DataKinds, MultiParamTypeClasses, FlexibleContexts #-}
module WinForms.Controls where

import WinForms.Types
import WinForms.Interface
import WinForms.Controls.Base

(#=) :: Marshal a => Property GettableSettable a -> a -> IO ()
(Property _ setter) #= val = setter val

get :: Marshal a => Property any a -> IO a
get (Property getter _) = getter

handle :: (Marshal a, Marshal b) => Event a b -> (a -> b -> IO ()) -> IO ()
handle = addHandler

makeShared "EventArgs"
makeShared "ControlCollection"
makeShared "Type"
makeInstantiable "Form"
makeInstantiable "Button"

toString :: Shared a => a -> IO String
toString = invokeMarshal "ToString" []

getType :: Shared a => a -> IO Type
getType = invokeMarshal "GetType" []

class Shared a => ControlC a where
    location :: a -> Property GettableSettable Point
    location = property "Location"

    backColor :: a -> Property GettableSettable Color
    backColor = property "BackColor"

    controls :: a -> Property Gettable ControlCollection
    controls = property "Controls"

    click :: a -> IO (Event a EventArgs)
    click = event "Click"

instance ControlC Form
instance ControlC Button

add :: ControlC a => a -> ControlCollection -> IO ()
add con = invokeVoid "Add" [toValue con]
