{-# LANGUAGE TemplateHaskell, DataKinds, MultiParamTypeClasses, FlexibleContexts,
             OverlappingInstances, ConstraintKinds, FlexibleInstances #-}
module WinForms.Controls where

import WinForms.Types
import WinForms.Interface
import WinForms.Controls.Base

(#=) :: (Marshal a, Subclass a b) => Setter a -> b -> IO ()
(Setter setter) #= val = setter $ cast val

handle :: (Marshal a, Marshal b) => (b -> IO ()) -> Event a b -> IO ()
handle f = handleWithTarget (\_ b -> f b)

handleWithTarget :: (Marshal a, Marshal b) => (a -> b -> IO ()) -> Event a b -> IO ()
handleWithTarget = flip addHandler

makeShared "EventArgs"
makeShared "ControlCollection"
makeShared "Type"
makeShared "Graphics"
makeShared "Control"
makeShared "Image"
makeShared "Bitmap"
makeShared "Brush"
makeShared "SolidBrush"
makeInstantiable "Form"
makeInstantiable "Button"
makeInstantiable "PictureBox"

toString :: Shared a => a -> IO String
toString = invokeMarshal "ToString" []

getType :: Shared a => a -> IO Type
getType = invokeMarshal "GetType" []

class Shared a => ControlC a where
    location :: FromGettableSettable p => a -> p Point
    location = getSet . property "Location"

    backColor :: FromGettableSettable p => a -> p Color
    backColor = getSet . property "BackColor"

    controls :: FromGettable p => a -> p ControlCollection
    controls = get . property "Controls"

    click :: a -> IO (Event a EventArgs)
    click = event "Click"

    backgroundImage :: FromGettableSettable p => a -> p Image
    backgroundImage = getSet . property "BackgroundImage"

    refresh :: a -> IO ()
    refresh = invokeVoid "Refresh" []

    text :: FromGettableSettable p => a -> p String
    text = getSet . property "Text"

instance ControlC a => Subclass Control a
instance ControlC Form
instance ControlC Button
instance ControlC PictureBox

class Shared a => ImageC a
instance ImageC a => Subclass Image a
instance ImageC Bitmap
image :: FromGettableSettable p => PictureBox -> p Image
image = getSet . property "Image"

add :: ControlC a => a -> ControlCollection -> IO ()
add con = invokeVoid "Add" [toValue con]

graphicsFromImage :: ImageC i => i -> IO Graphics
graphicsFromImage img = invokeStaticMarshal "System.Drawing.Graphics" "FromImage" [toValue img]

newBitmap :: Int -> Int -> IO Bitmap
newBitmap width height = invokeStaticMarshal "System.Drawing.Bitmap" "new" [toValue width, toValue height]

class Shared a => GraphicsC a where
    drawString :: BrushC b => String -> Font -> b -> PointF -> a -> IO ()
    drawString str fnt br p = invokeVoid "DrawString" [toValue str, toValue fnt, toValue br, toValue p]

instance GraphicsC a => Subclass Graphics a
instance GraphicsC Graphics

class Shared a => BrushC a
instance BrushC a => Subclass Brush a
instance BrushC SolidBrush

newSolidBrush :: Color -> IO SolidBrush
newSolidBrush = invokeStaticMarshal "System.Drawing.SolidBrush" "new" . return . toValue

--TODO: Handle contructors that take parameters as static functions with the name "new"
