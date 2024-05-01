module BrickMain where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Main (App (..), defaultMain, neverShowCursor, resizeOrQuit)
import Brick.Types (BrickEvent (..), EventM, Widget, get, put)
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (str, vBox, withAttr, (<+>))
import Brick.Widgets.Table (renderTable, table)
import Dictionary (Trie, buildDictionary, contains)
import qualified Graphics.Vty as V

data MyState = MyState
  { currentLetters :: [Char],
    playedLetters :: [Char],
    selectedLetter :: Char,
    dictionary :: Trie,
    score :: Int
  }
  deriving (Eq, Show)

data AppEvent = Quit | SelectLetter Char | OtherEvent

startState :: MyState
startState =
  MyState
    { currentLetters = ['A' .. 'G'],
      playedLetters = [],
      selectedLetter = 'A',
      dictionary = emptyTrie,
      score = 0
    }

arrowKey :: V.Key -> Int
arrowKey V.KRight = 1
arrowKey V.KLeft = -1
arrowKey _ = 0

handleEvent :: BrickEvent () () -> EventM () St ()
handleEvent (VtyEvent (V.EvKey V.KEnter _)) = do
  s <- get
  let i = case _inputMode s of
        Arrowing -> _arrowCell s
  let s' = play i s
  let s'' = checkBoard s'
  put (s'' {_textCell = ""})
  play :: Int -> St -> St

play i s
  | _playable s = case move (_grid s) i (_curPlayer s) of
      Nothing -> s {_statusMessage = "Invalid Move!"}
      Just g ->
        s
          { _grid = g,
            _curPlayer = next (_curPlayer s),
            _statusMessage = ""
          }
  | otherwise = s

handleEvent bevent@(VtyEvent (V.EvKey (V.KChar 'q') _)) =
  resizeOrQuit bevent
handleEvent (VtyEvent (V.EvKey (V.KChar c) _))
  | isDigit c = do
      s <- get
      if _playable s
        then
          put
            ( s
                { _textCell = _textCell s ++ [c],
                  _inputMode = Typing
                }
            )
        else put s
handleEvent (VtyEvent (V.EvKey key _))
  | key `elem` [V.KUp, V.KRight, V.KDown, V.KLeft] = do
      s <- get
      let offset = arrowKey key
      let newAC = (_arrowCell s + offset) `mod` (size * size)
      put $
        s
          { _arrowCell = newAC,
            _inputMode = Arrowing
          }
  | otherwise = return ()
handleEvent _ = return ()

app :: App St () ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

run :: IO ()
run = do
  _ <- defaultMain app initialSt
  return ()