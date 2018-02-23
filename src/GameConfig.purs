module GameConfig where


boxHeight :: Int
boxHeight = 300

boxWidth :: Int
boxWidth = 300

parentHeight :: Int
parentHeight = 600

parentWidth :: Int
parentWidth = 600

-- delay in milliseconds
buttonClickDelay :: Number
buttonClickDelay = 1000.0

buttonPressedDispTime :: Number
buttonPressedDispTime = 1000.0

box1Color :: String
box1Color = "#FF9800" -- orange

box2Color :: String
box2Color = "#b71c1c" -- red

box3Color :: String
box3Color = "#009688" -- teal

box4Color :: String
box4Color = "#00BCD4" -- cyan

----- on click, lighter shade of same base color

box1ClickedColor :: String
box1ClickedColor = "#FFCC80"

box2ClickedColor :: String
box2ClickedColor = "#f44336"

box3ClickedColor :: String
box3ClickedColor = "#B2DFDB"

box4ClickedColor :: String
box4ClickedColor = "#B2EBF2"


htmlBackground :: String
htmlBackground = "#ffffff"
