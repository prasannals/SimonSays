module Main where

import Collision
import Data.Array
import Data.String
import GameConfig
import Halogen.VDom.Types
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.Eff.Random (randomInt)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (group)
import Data.Array (length)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import FRP as F
import FRP.Behavior (behavior)
import FRP.Event as E
import FRP.Event.Mouse as M
import FRP.Event.Time (animationFrame, withTime)
import Halogen.VDom.Types (graft)
import Neon.Operator ((%))
import Partial.Unsafe (unsafePartial)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent

type BoxPattern = {pattern :: Array Int, done :: Int, correctGuesses :: Int}
type Timer = {set :: Boolean, time :: Number, duration :: Number}
type MyState = {clickable :: Boolean, b1Color :: String, b2Color :: String, b3Color :: String, b4Color :: String, clicked :: Boolean,
                clickedBox :: Int, lastClickTime :: Number, boxPattern :: BoxPattern, delayTimer :: Timer, score :: Int, gameOver :: Boolean}

widget :: forall a . MyState  -> VDom Attr a
widget state = frameLayout
              [ id_ "root"
              , height (show parentHeight)
              , width (show parentWidth)
              , background htmlBackground
              ]
              [
                linearLayout[
                   height (show boxHeight)
                   , width (show boxWidth)
                   , background state.b1Color
                   , id_ "box1"
                   , onMouseDown (Some click)
                   , margin (posToMarginStr ((parentWidth / 2) - boxWidth) ((parentHeight / 2) - boxHeight))
                 ][]
                 ,
                 linearLayout[
                   height (show boxHeight)
                   , width (show boxWidth)
                   , background state.b2Color
                   , id_ "box2"
                   , onMouseDown (Some click)
                   , margin (posToMarginStr (parentWidth / 2) ((parentHeight / 2) - boxHeight))
                 ][]
                 ,
                linearLayout[
                   height (show boxHeight)
                   , width (show boxWidth)
                   , background state.b3Color
                   , id_ "box3"
                   , onMouseDown (Some click)
                   , margin (posToMarginStr (parentWidth / 2) (parentHeight / 2))
                 ][]
                 ,
                 linearLayout[
                   height (show boxHeight)
                   , width (show boxWidth)
                   , background state.b4Color
                   , id_ "box4"
                   , onMouseDown (Some click)
                   , margin (posToMarginStr ((parentWidth / 2) - boxWidth) (parentHeight / 2))
                 ][]
                , textView[
                    id_ "outView"
                    , height "200"
                    , width "500"
                    , text $ scoreMsg state.score state.gameOver
                    , margin (posToMarginStr 650 100)
                    , textSize "40"
                  ]

              ]

scoreMsg :: Int -> Boolean -> String
scoreMsg score gameOver = "Score : " <> (show score) <> "\n" <> if gameOver then "Game Over" else "Game on!"

posToMarginStr :: Int -> Int -> String
posToMarginStr x y = (show x) <> ", " <> (show y) <> " , 0, 0"


frameUpdate x = do
  U.getState

getBoxNum x y = if (x > ((parentWidth / 2) - boxWidth)) && (x < ((parentWidth /2) + boxWidth) ) && (y > ((parentHeight/ 2) - boxHeight)) && (y < ((parentHeight/ 2) + boxHeight))
                then if (x < (parentWidth / 2))
                  then if (y < (parentHeight / 2))
                    then 1
                    else 4
                  else if (y < (parentHeight / 2))
                    then 2
                    else 3
                else -1

selectNextBox = randomInt 1 4

clickBox boxNum time = do
  _ <- case boxNum of
    1 -> U.updateState "b1Color" box1ClickedColor
    2 -> U.updateState "b2Color" box2ClickedColor
    3 -> U.updateState "b3Color" box3ClickedColor
    4 -> U.updateState "b4Color" box4ClickedColor
    _ -> U.getState
  _ <- case boxNum of
    1 -> U.updateState "clickedBox" 1
    2 -> U.updateState "clickedBox" 2
    3 -> U.updateState "clickedBox" 3
    4 -> U.updateState "clickedBox" 4
    _ -> U.getState
  if (boxNum == 1) || (boxNum == 2) || (boxNum == 3) || (boxNum == 4)
    then do
      _ <- U.updateState "clicked" true
      _ <- U.updateState "clickable" false
      U.updateState "lastClickTime" time
    else U.getState


-- mOnMouseDown :: { value :: a, pos :: Nullable { x :: Int, y :: Int } -> Eff
evalPosition val time = do
  (st::MyState) <- U.getState
  if st.clickable
    then do
      let boxNum = getBoxNum pos.x pos.y
      if boxNum == (unsafePartial $ fromJust $ (st.boxPattern.pattern !! st.boxPattern.correctGuesses) )
        then do -- correctGuess
          _ <- clickBox boxNum time -- click box
          U.updateState "boxPattern" {pattern : st.boxPattern.pattern, done : st.boxPattern.done , correctGuesses : (st.boxPattern.correctGuesses + 1)}
        else if (not (boxNum == -1))
          then do
            U.updateState "gameOver" true
          else pure st


    else clickBox (-1) time where
  pos = unsafePartial $ fromJust $ toMaybe val.pos

mOnMouseDown val = do
  (st::MyState) <- U.getState
  if (not st.gameOver)
    then evalPosition val.value val.time
    else pure st

resetBoxColor box = do
  case box of
    1 -> U.updateState "b1Color" box1Color
    2 -> U.updateState "b2Color" box2Color
    3 -> U.updateState "b3Color" box3Color
    4 -> U.updateState "b4Color" box4Color
    _ -> U.getState

showState :: MyState -> String
showState s = showTimer s.delayTimer

showTimer :: Timer -> String
showTimer t = (show t.set) <> ", " <> (show t.time) <> ", " <> (show t.duration)

mOnAnim a = do
  (state::MyState) <- U.getState

  if (state.delayTimer.set) && (not state.gameOver)
    then if ((state.delayTimer.time + state.delayTimer.duration) <= a.time)
          then do
            log $ (show $ (state.delayTimer.time + state.delayTimer.duration)) <> " <= "<> (show a.time ) <> "\n After delay : " <> (showState state)
            U.updateState "delayTimer" {set: false, time : 0.0, duration : 0.0}
          else pure state
    else do
      st <- if (state.clicked)  && (not state.gameOver)
        then if (time - state.lastClickTime) > buttonPressedDispTime
          then do
            bReset <- resetBoxColor state.clickedBox
            log $ showState bReset
            tSet <- U.updateState "delayTimer" {set : true, time : a.time, duration : buttonClickDelay}
            log $ showState tSet
            _ <- U.updateState "clicked" false
            _ <- U.updateState "clickable" true
            _ <- U.updateState "clickedBox" (-1)
            U.updateState "lastClickTime" 0.0
          else U.getState
        else U.getState
      if ( (length $ st.boxPattern.pattern) == st.boxPattern.done ) && (not st.delayTimer.set) && (not state.gameOver)-- pattern already displayed to user
        then if st.boxPattern.correctGuesses == (length st.boxPattern.pattern) -- user has guessed everything right
          then do -- add new element into array. reset correctGuesses and done
            newBox <- selectNextBox
            nb <- U.updateState "boxPattern" {pattern : st.boxPattern.pattern <> [newBox], done : 0, correctGuesses : 0 }
            U.updateState "score" ((length nb.boxPattern.pattern) - 1)
          else  -- look for user input. Should only get user input under this condition. basically, we're waiting for users input here
            pure st
        else if (not st.clicked) && (not st.delayTimer.set) && (not state.gameOver)
                then do -- still more left to be displayed in pattern
                  let boxNum = unsafePartial $ fromJust $ (st.boxPattern.pattern !! st.boxPattern.done)
                  log "clicking box to display pattern"
                  _ <- clickBox boxNum time
                  U.updateState "boxPattern" {pattern : st.boxPattern.pattern, done : (st.boxPattern.done + 1), correctGuesses : st.boxPattern.correctGuesses }
                else pure st
      where
        time = a.time


listen = do
  let mDownEvent = withTime (M.withPosition M.down)
  let mUpEvent = withTime (M.withPosition M.up)
  let anim = withTime animationFrame
  box1 <- U.signal "box1" ""

  _ <- mDownEvent `E.subscribe` mOnMouseDown
  _ <- mUpEvent `E.subscribe` (\_ -> do log "MouseUp")

  _ <- anim `E.subscribe` mOnAnim
  U.patch widget (frameUpdate <$> box1.behavior) (anim)

resetState = do
  _ <- U.updateState "b1Color" box1Color
  _ <- U.updateState "b2Color" box2Color
  _ <- U.updateState "b3Color" box3Color
  _ <- U.updateState "b4Color" box4Color
  _ <- U.updateState "clicked" false
  _ <- U.updateState "clickedBox" (-1)
  _ <- U.updateState "lastClickTime" 0.0
  firstBox <- selectNextBox
  _ <- U.updateState "boxPattern" {pattern : [firstBox], done : 0, correctGuesses : 0}
  _ <- U.updateState "delayTimer" {set : false, time : 0.0, duration : 0.0}
  _ <- U.updateState "score" 0
  _ <- U.updateState "gameOver" false
  U.updateState "clickable" true  --- TODO can clickable be replaced with clicked? is clickable redundant?

-- type BoxPattern = {pattern :: Array Int, done :: Int, correctGuesses :: Int}
main = do
  --- Init State {} empty record--
  U.initializeState
  --- Update State ----
  state <- resetState
  ---  global  key value pair in your "state" (which is also global)
  ---- Render Widget ---
  U.render (widget state) listen
  pure unit
