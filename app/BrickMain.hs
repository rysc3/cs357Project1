import Brick.Main (App(..), defaultMain, resizeOrQuit, suspendAndResume')
import Brick.Types (Widget, BrickEvent(..), EventM, get, put, CursorLocation(..), Location(..))
import Brick.Widgets.Core (str, clickable, (<+>), (<=>), vBox, withAttr, showCursor, translateBy)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Border (border, hBorder)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(..), Modifier(..), Event(..), Button(..))