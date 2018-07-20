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

import Control.Monad.IO.Class

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
          | DoneTodos
          | Edit
          | Instructions
          deriving (Ord, Show, Eq)

type TodoItem = String

type Index = Int

data State = State { _todos        :: L.List Name TodoItem
                   , _doneTodos    :: L.List Name TodoItem
                   , _editTodo     :: E.Editor String Name
                   , _focusRing    :: F.FocusRing Name
                   , _editingTodo  :: Bool
                   , _lastFocus    :: Name
                   } 

makeLenses ''State

initialTodos :: [TodoItem]
initialTodos = ["foo","bar"]

createEditor :: String -> E.Editor String Name
createEditor = E.editor Edit (Just 1)

initialState :: State
initialState = State (L.list Todos (Vec.fromList initialTodos) 1)
                     (L.list DoneTodos Vec.empty 1)
                     (createEditor "")
                     (F.focusRing [Todos, Edit, DoneTodos, Instructions])
                     False
                     Todos

listDrawElement :: Bool -> TodoItem -> Widget Name
listDrawElement sel a =
    let selStr = case sel of
                    True  -> (withAttr customAttr) . str
                    False -> str
    in C.hCenter $ selStr a

selectedItem :: State -> Maybe TodoItem 
selectedItem st = snd <$> L.listSelectedElement (st^.todos)

editorOpened :: State -> Bool
editorOpened st = F.focusGetCurrent (st^.focusRing) == Just Edit

centeredText :: String -> Widget a
centeredText = C.hCenter . str

instrs :: [ Widget a ]
instrs = centeredText <$> 
        [ "Press Enter to add a new todo."
        , "Press x to mark a todo as done."
        , "Press e to edit a todo."
        , "Press d to delete a todo."
        , "Press i to exit this menu."
        ]

drawUI :: State -> [Widget Name]
drawUI st = [ui]
    where
        todosWidget = F.withFocusRing (st^.focusRing) (L.renderList listDrawElement)
        render lines = txt $ mconcat lines
        edit = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editTodo)
        centerContent = case F.focusGetCurrent (st^.focusRing) of
            Just Todos -> todosWidget (st^.todos)
            Nothing    -> todosWidget (st^.todos)
            Just DoneTodos       -> todosWidget (st^.doneTodos)
            Just Instructions    -> C.vCenter $ vBox instrs
            Just Edit            -> edit
        footerElems = case F.focusGetCurrent (st^.focusRing) of 
                        Just Edit         -> [ centeredText "Press Enter when you are done"   ]
                        Just Instructions -> [] 
                        Just Todos        -> 
                            case Vec.null (st^.(todos.(L.listElementsL))) of
                                True  -> [ centeredText "Press Enter to create a new todo" ]
                                False -> [ centeredText "Press i to see the instructions" ]
                        _                 -> [ centeredText "Press i to see the instructions" ] 
        content = C.vCenter $ vBox $ centerContent : footerElems 
        ui = B.borderWithLabel (str "TODOS") $
                hLimit 35 $
                vLimit 25 $
                content

insertTodoFromEditor :: State -> State
insertTodoFromEditor st = 
    let todo = unlines $ E.getEditContents (st^.editTodo)
    in st & todos                     %~ (L.listInsert 0 todo)
          & (todos . L.listSelectedL) .~ (Just 0) -- select the top one after adding
          & editTodo                  %~ (E.applyEdit Tz.clearZipper)

markSelectedAsDone :: State -> State
markSelectedAsDone st = maybe st insertIntoDoneList selectedItem
    where selectedItem =
            L.listSelectedElement (st^.todos)
          insertIntoDoneList (index,todo) =
            st & doneTodos %~ (L.listInsert 0 todo)
               &     todos %~ (L.listRemove index )

applyEditsToSelectedTodo :: State -> State
applyEditsToSelectedTodo st =
    let editedTodo = unlines $ E.getEditContents (st^.editTodo)
    in  st & todos %~ (L.listModify (const editedTodo))

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent st (T.VtyEvent ev) =
    case (ev, F.focusGetCurrent (st^.focusRing)) of
        (V.EvKey V.KEnter [], Just Todos) -> 
            M.continue $ st & focusRing %~ (F.focusSetCurrent Edit) 
                            & editTodo  .~ (createEditor "")
        (V.EvKey V.KEnter [], Just Edit ) -> 
            let st' = case (st^.editingTodo) of
                        True  -> applyEditsToSelectedTodo st 
                        False -> insertTodoFromEditor st
            in  M.continue $ st' & focusRing %~ (F.focusSetCurrent Todos)
        (V.EvKey (V.KChar 'x') [], Just Todos) ->
            M.continue $ markSelectedAsDone st
        (V.EvKey (V.KChar 'e') [], Just Todos) ->
            M.continue $ maybe st openEditor (selectedItem st)
            where selectedItem st =
                    L.listSelectedElement (st^.todos)
                  openEditor (index,todo) =
                    st & editingTodo .~ True
                       & editTodo    .~ (createEditor todo)
                       & editTodo    %~ (E.applyEdit Tz.gotoEOL) -- @TODO there are two mods on `editTodo` is there a better way?
                       & focusRing   %~ (F.focusSetCurrent Edit)
        (V.EvKey (V.KChar 'i') [], Just currentFocus) | currentFocus /= Edit -> M.continue $
            case currentFocus of
                Instructions -> st & focusRing %~ (F.focusSetCurrent (st^.lastFocus)) 
                current      -> st & lastFocus .~ current
                                   & focusRing %~ (F.focusSetCurrent Instructions)
        (V.EvKey (V.KChar 'd') [], Just Todos) ->
            M.continue $ maybe st deleteItem (selectedItem st)
            where selectedItem st =
                    L.listSelectedElement (st^.todos)
                  deleteItem (index,_) =
                    st & todos %~ (L.listRemove index)
        -- *V.EvKey (V.KChar 'a' [], Just Todos) ->
        --     M.continue
        (V.EvKey V.KEsc [], _           ) -> M.halt st
        _                         ->
            M.continue =<< case F.focusGetCurrent (st^.focusRing) of
                Just Todos -> T.handleEventLensed st todos    L.handleListEvent   ev
                Just Edit  -> T.handleEventLensed st editTodo E.handleEditorEvent ev
                Nothing    -> return st                
appEvent st ev =
    M.continue st


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
