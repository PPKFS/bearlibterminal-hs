{-# LANGUAGE PatternSynonyms #-}

module BearLibTerminal.Keycodes where

import GHC.Generics

newtype Keycode = Keycode Int
  deriving newtype (Eq, Ord, Show, Enum, Bounded)
  deriving stock (Generic)

pattern TkA :: Keycode
pattern TkA= Keycode 0x04

pattern TkB :: Keycode
pattern TkB= Keycode 0x05

pattern TkC :: Keycode
pattern TkC= Keycode 0x06

pattern TkD :: Keycode
pattern TkD= Keycode 0x07

pattern TkE :: Keycode
pattern TkE= Keycode 0x08

pattern TkF :: Keycode
pattern TkF= Keycode 0x09

pattern TkG :: Keycode
pattern TkG= Keycode 0x0A

pattern TkH :: Keycode
pattern TkH= Keycode 0x0B

pattern TkI :: Keycode
pattern TkI= Keycode 0x0C

pattern TkJ :: Keycode
pattern TkJ= Keycode 0x0D

pattern TkK :: Keycode
pattern TkK= Keycode 0x0E

pattern TkL :: Keycode
pattern TkL= Keycode 0x0F

pattern TkM :: Keycode
pattern TkM= Keycode 0x10

pattern TkN :: Keycode
pattern TkN= Keycode 0x11

pattern TkO :: Keycode
pattern TkO= Keycode 0x12

pattern TkP :: Keycode
pattern TkP= Keycode 0x13

pattern TkQ :: Keycode
pattern TkQ= Keycode 0x14

pattern TkR :: Keycode
pattern TkR= Keycode 0x15

pattern TkS :: Keycode
pattern TkS= Keycode 0x16

pattern TkT :: Keycode
pattern TkT= Keycode 0x17

pattern TkU :: Keycode
pattern TkU= Keycode 0x18

pattern TkV :: Keycode
pattern TkV= Keycode 0x19

pattern TkW :: Keycode
pattern TkW= Keycode 0x1A

pattern TkX :: Keycode
pattern TkX= Keycode 0x1B

pattern TkY :: Keycode
pattern TkY= Keycode 0x1C

pattern TkZ :: Keycode
pattern TkZ= Keycode 0x1D

pattern Tk1 :: Keycode
pattern Tk1= Keycode 0x1E

pattern Tk2 :: Keycode
pattern Tk2= Keycode 0x1F

pattern Tk3 :: Keycode
pattern Tk3= Keycode 0x20

pattern Tk4 :: Keycode
pattern Tk4= Keycode 0x21

pattern Tk5 :: Keycode
pattern Tk5= Keycode 0x22

pattern Tk6 :: Keycode
pattern Tk6= Keycode 0x23

pattern Tk7 :: Keycode
pattern Tk7= Keycode 0x24

pattern Tk8 :: Keycode
pattern Tk8= Keycode 0x25

pattern Tk9 :: Keycode
pattern Tk9= Keycode 0x26

pattern Tk0 :: Keycode
pattern Tk0= Keycode 0x27

pattern TkReturn :: Keycode
pattern TkReturn= Keycode 0x28

pattern TkEnter :: Keycode
pattern TkEnter= Keycode 0x28

pattern TkEscape :: Keycode
pattern TkEscape= Keycode 0x29

pattern TkBackspace :: Keycode
pattern TkBackspace= Keycode 0x2A

pattern TkTab :: Keycode
pattern TkTab= Keycode 0x2B

pattern TkSpace :: Keycode
pattern TkSpace= Keycode 0x2C

pattern TkMinus :: Keycode
pattern TkMinus= Keycode 0x2D

pattern TkEquals :: Keycode
pattern TkEquals= Keycode 0x2E

pattern TkLbracket :: Keycode
pattern TkLbracket= Keycode 0x2F

pattern TkRbracket :: Keycode
pattern TkRbracket= Keycode 0x30

pattern TkBackslash :: Keycode
pattern TkBackslash= Keycode 0x31

pattern TkSemicolon :: Keycode
pattern TkSemicolon= Keycode 0x33

pattern TkApostrophe :: Keycode
pattern TkApostrophe= Keycode 0x34

pattern TkGrave :: Keycode
pattern TkGrave= Keycode 0x35

pattern TkComma :: Keycode
pattern TkComma= Keycode 0x36

pattern TkPeriod :: Keycode
pattern TkPeriod= Keycode 0x37

pattern TkSlash :: Keycode
pattern TkSlash= Keycode 0x38

pattern TkF1 :: Keycode
pattern TkF1= Keycode 0x3A

pattern TkF2 :: Keycode
pattern TkF2= Keycode 0x3B

pattern TkF3 :: Keycode
pattern TkF3= Keycode 0x3C

pattern TkF4 :: Keycode
pattern TkF4= Keycode 0x3D

pattern TkF5 :: Keycode
pattern TkF5= Keycode 0x3E

pattern TkF6 :: Keycode
pattern TkF6= Keycode 0x3F

pattern TkF7 :: Keycode
pattern TkF7= Keycode 0x40

pattern TkF8 :: Keycode
pattern TkF8= Keycode 0x41

pattern TkF9 :: Keycode
pattern TkF9= Keycode 0x42

pattern TkF10 :: Keycode
pattern TkF10= Keycode 0x43

pattern TkF11 :: Keycode
pattern TkF11= Keycode 0x44

pattern TkF12 :: Keycode
pattern TkF12= Keycode 0x45

pattern TkPause :: Keycode
pattern TkPause= Keycode 0x48

pattern TkInsert :: Keycode
pattern TkInsert= Keycode 0x49

pattern TkHome :: Keycode
pattern TkHome= Keycode 0x4A

pattern TkPageup :: Keycode
pattern TkPageup= Keycode 0x4B

pattern TkDelete :: Keycode
pattern TkDelete= Keycode 0x4C

pattern TkEnd :: Keycode
pattern TkEnd= Keycode 0x4D

pattern TkPagedown :: Keycode
pattern TkPagedown= Keycode 0x4E

pattern TkRight :: Keycode
pattern TkRight= Keycode 0x4F

pattern TkLeft :: Keycode
pattern TkLeft= Keycode 0x50

pattern TkDown :: Keycode
pattern TkDown= Keycode 0x51

pattern TkUp :: Keycode
pattern TkUp= Keycode 0x52

pattern TkKpDivide :: Keycode
pattern TkKpDivide= Keycode 0x54

pattern TkKpMultiply :: Keycode
pattern TkKpMultiply= Keycode 0x55

pattern TkKpMinus :: Keycode
pattern TkKpMinus= Keycode 0x56

pattern TkKpPlus :: Keycode
pattern TkKpPlus= Keycode 0x57

pattern TkKpEnter :: Keycode
pattern TkKpEnter= Keycode 0x58

pattern TkKp1 :: Keycode
pattern TkKp1= Keycode 0x59

pattern TkKp2 :: Keycode
pattern TkKp2= Keycode 0x5A

pattern TkKp3 :: Keycode
pattern TkKp3= Keycode 0x5B

pattern TkKp4 :: Keycode
pattern TkKp4= Keycode 0x5C

pattern TkKp5 :: Keycode
pattern TkKp5= Keycode 0x5D

pattern TkKp6 :: Keycode
pattern TkKp6= Keycode 0x5E

pattern TkKp7 :: Keycode
pattern TkKp7= Keycode 0x5F

pattern TkKp8 :: Keycode
pattern TkKp8= Keycode 0x60

pattern TkKp9 :: Keycode
pattern TkKp9= Keycode 0x61

pattern TkKp0 :: Keycode
pattern TkKp0= Keycode 0x62

pattern TkKpPeriod :: Keycode
pattern TkKpPeriod= Keycode 0x63

pattern TkShift :: Keycode
pattern TkShift= Keycode 0x70

pattern TkControl :: Keycode
pattern TkControl= Keycode 0x71

pattern TkAlt :: Keycode
pattern TkAlt= Keycode 0x72

pattern TkMouseLeft :: Keycode
pattern TkMouseLeft= Keycode 0x80

pattern TkMouseRight :: Keycode
pattern TkMouseRight= Keycode 0x81

pattern TkMouseMiddle :: Keycode
pattern TkMouseMiddle= Keycode 0x82

pattern TkMouseX1 :: Keycode
pattern TkMouseX1= Keycode 0x83

pattern TkMouseX2 :: Keycode
pattern TkMouseX2= Keycode 0x84

pattern TkMouseMove :: Keycode
pattern TkMouseMove= Keycode 0x85

pattern TkMouseScroll :: Keycode
pattern TkMouseScroll= Keycode 0x86

pattern TkMouseX :: Keycode
pattern TkMouseX= Keycode 0x87

pattern TkMouseY :: Keycode
pattern TkMouseY= Keycode 0x88

pattern TkMousePixelX :: Keycode
pattern TkMousePixelX= Keycode 0x89

pattern TkMousePixelY :: Keycode
pattern TkMousePixelY= Keycode 0x8A

pattern TkMouseWheel :: Keycode
pattern TkMouseWheel= Keycode 0x8B

pattern TkMouseClicks :: Keycode
pattern TkMouseClicks= Keycode 0x8C

{-
 * If key was released instead of pressed, it's code will be OR'ed with TkKEYRELEASED:
 * a) pressed 'A' :: 0x04
 * b) released 'A' :: 0x04|VKKEYRELEASED == Keycode 0x104
-}
pattern TkKeyReleased :: Keycode
pattern TkKeyReleased = Keycode 0x100

{-
 * Virtual key-codes for internal terminal states/variables.
 * These can be accessed via terminalstate function.
-}
pattern TkWidth :: Keycode
pattern TkWidth= Keycode 0xC0

pattern TkHeight :: Keycode
pattern TkHeight= Keycode 0xC1

pattern TkCellWidth :: Keycode
pattern TkCellWidth= Keycode 0xC2

pattern TkCellHeight :: Keycode
pattern TkCellHeight= Keycode 0xC3

pattern TkColor :: Keycode
pattern TkColor= Keycode 0xC4

pattern TkBkcolor :: Keycode
pattern TkBkcolor= Keycode 0xC5

pattern TkLayer :: Keycode
pattern TkLayer= Keycode 0xC6

pattern TkComposition :: Keycode
pattern TkComposition= Keycode 0xC7

pattern TkChar :: Keycode
pattern TkChar= Keycode 0xC8

pattern TkWchar :: Keycode
pattern TkWchar= Keycode 0xC9

pattern TkEvent :: Keycode
pattern TkEvent= Keycode 0xCA

pattern TkFullscreen :: Keycode
pattern TkFullscreen= Keycode 0xCB

pattern TkClose :: Keycode
pattern TkClose= Keycode 0xE0

pattern TkResized :: Keycode
pattern TkResized= Keycode 0xE1