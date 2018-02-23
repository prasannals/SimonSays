module GameConfig where


boxHeight :: Int
boxHeight = 400

boxWidth :: Int
boxWidth = 400

parentHeight :: Int
parentHeight = 800

parentWidth :: Int
parentWidth = 800

-- delay in milliseconds
buttonClickDelay :: Number
buttonClickDelay = 250.0

buttonPressedDispTime :: Number
buttonPressedDispTime = 400.0
--
-- box1Color :: String
-- box1Color = "#FF9800" -- orange
--
-- box2Color :: String
-- box2Color = "#b71c1c" -- red
--
-- box3Color :: String
-- box3Color = "#009688" -- teal
--
-- box4Color :: String
-- box4Color = "#00BCD4" -- cyan


box1Color :: String
box1Color = "#D32F2F" -- red

box2Color :: String
box2Color = "#1B5E20" -- green

box3Color :: String
box3Color = "#0D47A1" -- blue

box4Color :: String
box4Color = "#4E342E" --  brown

----- on click, lighter shade of same base color

box1ClickedColor :: String
box1ClickedColor = "#FFCDD2"

box2ClickedColor :: String
box2ClickedColor = "#C8E6C9"

box3ClickedColor :: String
box3ClickedColor = "#BBDEFB"

box4ClickedColor :: String
box4ClickedColor = "#D7CCC8"


htmlBackground :: String
htmlBackground = "#ffffff"
