{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Todo where

import Lens.Micro
import Lens.Micro.TH
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Data.Vector as Vec
import qualified Data.Text.Zipper as Tz
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
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
import Safe(headMay)

data Name = Todos
          | Edit
          deriving (Ord, Show, Eq)

type TodoItem = String

data State = State { _todos        :: L.List Name TodoItem
                   , _editTodo     :: E.Editor String Name
                   , _focusRing    :: F.FocusRing Name
                   } 

makeLenses ''State

initialTodos :: [TodoItem]
initialTodos = ["foo","bar"]

initialState :: State
initialState = State (L.list Todos (Vec.fromList initialTodos) 1)
                     (E.editor Edit Nothing "")
                     (F.focusRing [Todos, Edit])

listDrawElement :: Bool -> TodoItem -> Widget Name
listDrawElement sel a =
    let selStr s = case sel of
                    True  -> withAttr customAttr (str s)
                    False -> str s  
    in C.hCenter $ selStr a

selectedItem :: State -> Maybe TodoItem 
selectedItem st = snd <$> L.listSelectedElement (st^.todos)

editorOpened :: State -> Bool
editorOpened st = F.focusGetCurrent (st^.focusRing) == Just Edit

drawUI :: State -> [Widget Name]
drawUI st = [ui]
    where
        todosW = F.withFocusRing (st^.focusRing) (L.renderList listDrawElement) (st^.todos)
        render lines = txt $ mconcat lines
        edit = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editTodo)
        content = C.vCenter $ vBox [ if editorOpened st then edit else todosW
                                   , C.hCenter $ str "Press Esc to exit."
                                   , C.hCenter $ str (maybe "NO" id (selectedItem st))
                                   ]
        ui = B.borderWithLabel (str "TODOS") $
             hLimit 25 $
             vLimit 15 $
             content

insertTodoFromEditor :: State -> State
insertTodoFromEditor st = 
    let todo = unlines $ E.getEditContents (st^.editTodo)
    in st & todos                     %~ (L.listInsert 0 todo)
          & (todos . L.listSelectedL) %~ (const $ Just 0) -- select the top one after adding
          & editTodo                  %~ (E.applyEdit Tz.clearZipper)

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent st (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> 
            let st' = if editorOpened st then insertTodoFromEditor st else st 
            in  M.continue $ st' & focusRing %~ F.focusNext
        V.EvKey V.KEsc []   -> M.halt st
        ev -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
                Just Todos -> T.handleEventLensed st todos    L.handleListEvent   ev
                Just Edit  -> T.handleEventLensed st editTodo E.handleEditorEvent ev
                Nothing    -> return st
                

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]
        
appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App State e Name
theApp =
    M.App {   M.appDraw = drawUI
            , M.appChooseCursor = appCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return
            , M.appAttrMap = const theMap
            }

                   
todoApp  :: IO ()
todoApp = 
    void $ M.defaultMain theApp initialState