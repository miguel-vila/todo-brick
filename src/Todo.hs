{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Todo where

import Lens.Micro ((^.))
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Data.Vector as Vec
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Types
    ( Widget
    )
import Brick.Widgets.Core
    ( (<+>)
    , str
    , txt
    , vLimit
    , hLimit
    , vBox
    , withAttr
    )
import Brick.Util (fg, on)
import Data.Text(Text)

type TodoItem = String

data State = State { todos :: L.List () TodoItem
                   , editTodo :: E.Editor Text ()
                   , editorOpened :: Bool
                   } 

initialState :: State
initialState = State (L.list () (Vec.fromList ["foo","bar"]) 1)
                     (E.editorText () (Just 2) "hola")
                     False

listDrawElement :: Bool -> TodoItem -> Widget ()
listDrawElement sel a =
    let selStr s = case sel of
                    True  -> withAttr customAttr (str s)
                    False -> str s  
    in C.hCenter $ selStr a

drawUI :: State -> [Widget ()]
drawUI (State l e openEditor) = [ui]
    where
        todos = L.renderList listDrawElement True l
        render lines = txt $ mconcat lines
        edit = E.renderEditor render True e
        content = C.vCenter $ vBox [ if openEditor then edit else todos
                                   , C.hCenter $ str "Press Esc to exit."
                                   ]
        ui = B.borderWithLabel (str "TODOS") $
             hLimit 25 $
             vLimit 15 $
             content

appEvent :: State -> T.BrickEvent () e -> T.EventM () (T.Next State)
appEvent st @ (State l _ openEditor) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> M.continue (st { editorOpened = not (editorOpened st) })
        V.EvKey V.KEsc []   -> M.halt st

        ev -> L.handleListEvent ev l >>= (\l -> M.continue (st { todos = l } ) )

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]
        

theApp :: M.App State e ()
theApp =
    M.App {   M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return
            , M.appAttrMap = const theMap
            }

                   
todoApp  :: IO ()
todoApp = 
    void $ M.defaultMain theApp initialState