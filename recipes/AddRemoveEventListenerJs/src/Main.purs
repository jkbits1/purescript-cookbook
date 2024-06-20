module AddRemoveEventListenerJs.Main where

import Prelude

-- import Data.Set ()

import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Document
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, screenX, screenY)
import Web.UIEvent.MouseEvent.EventTypes as METypes

-- chess defs
data Colour = Black | White

data Piece = Rook | Knight | Bishop | King | Queen | Pawn

data File = A | B | C | D | E | F | G | H

data Row  = One | Two | Three | Four | Five | Six | Seven | Eight

data Pos = Pos Piece Colour File Row 

newPiece = Pos Rook Black A One

main :: Effect Unit
main = do
  -- get the document
  -- :: Effect Window
  win <- window
  -- :: Effect HTMLDocument
  doc <- document win

  -- get the buttons
  let docAsParent = Document.toParentNode doc
  -- :: Effect (Maybe Element)
  mbButtonAddEL     <- querySelector (QuerySelector "#addListenerButton") docAsParent
  mbButtonRemoveEL  <- querySelector (QuerySelector "#removeListenerButton") docAsParent
  mbButtonMain      <- querySelector (QuerySelector "#mainButton") docAsParent
  mbButtonRemove2nd <- querySelector (QuerySelector "#removeSecondApproach") docAsParent
  mbButtonTest1     <- querySelector (QuerySelector "#testButton1") docAsParent
  mbButtonTest2     <- querySelector (QuerySelector "#testButton2") docAsParent

  case mbButtonAddEL, mbButtonRemoveEL, mbButtonMain, mbButtonRemove2nd, mbButtonTest1, mbButtonTest2 of
    Just bAddEL, Just bRemoveEL, Just bMain, Just bRemove2nd, Just bButtonTest1, Just bButtonTest2 -> do
      let
        -- make it possible to add event listeners to these elements
        -- by changing their types from `Element` to `EventTarget`
        buttonAddEL     = Element.toEventTarget bAddEL
        buttonRemoveEL  = Element.toEventTarget bRemoveEL
        buttonMain      = Element.toEventTarget bMain
        buttonRemove2nd = Element.toEventTarget bRemove2nd
        buttonTest1     = Element.toEventTarget bButtonTest1
        buttonTest2     = Element.toEventTarget bButtonTest2

      -- In this first approach, we will add an event listener,
      -- and store a reference to that listener, so that we can
      -- remove it at a later time.
      -- We will only add/remove a single listener in this approach.

      -- :: Effect (Ref (Maybe EventListener))
      ref <- Ref.new Nothing
      
      -- create the event listener
      -- :: Effect EventListener
      addMainListener <- eventListener \_ -> do
        -- :: Effect (Maybe EventListener)
        mbListener <- Ref.read ref
        case mbListener of
          Nothing -> do
            -- :: Effect Unit
            log "Adding new listener."
            -- :: Effect EventListener
            printMessageListener <- eventListener printMessage
            -- :: Effect Unit
            addEventListener
              METypes.click
              printMessageListener
              false -- use bubble phase, not capture phase
              buttonMain

            -- now store the listener inside the ref
            -- :: Effect Unit
            Ref.write (Just printMessageListener) ref
          Just _ -> do
            -- :: Effect Unit
            log "Listener already added. Not reinstalling listener."

      -- add the event listener created above to dom
      -- :: Effect Unit
      addEventListener METypes.click addMainListener false buttonAddEL

      -- create event listener
      -- :: Effect EventListener
      removeMainListener <- eventListener \_ -> do
        -- :: Effect (Maybe EventListener)
        mbListener <- Ref.read ref
        -- :: Effect Unit
        case mbListener of
          Just listener -> do
            -- :: Effect Unit
            log "Removing listener from the main button."
            -- :: Effect Unit
            removeEventListener
              METypes.click
              listener          -- EventListener
              false             -- use bubble phase, not capture phase
              buttonMain

            -- :: Effect Unit
            Ref.write Nothing ref
          Nothing -> do
            -- :: Effect Unit
            log "No listener is xc installed. Click the 'Add Listener' button to add it."

      -- :: Effect Unit
      addEventListener METypes.click removeMainListener false buttonRemoveEL

      -- In this approach, we use a better approach to add an event listener
      -- where we don't need to store a reference to the listener since
      -- it is still in scope.
      -- :: Effect (Effect Unit)
      removeListener <- addBetterListener METypes.click false buttonMain \_ -> do
        -- :: Effect Unit
        log $
          "Better listener: this is a better way to add/remove \
          \an event listener."

      -- add event listener created above to dom
      -- :: Effect EventListener
      remove2ndApproachListener <- eventListener \_ -> do
        -- :: Effect Unit
        log "Now removing event listener using better approach"
        -- :: Effect Unit
        removeListener

      -- :: Effect Unit
      addEventListener METypes.click remove2ndApproachListener false buttonRemove2nd

    -- With the new buttons, these underscores become unwieldy
    -- and hard to count. 
    -- :: Effect Unit
    _, _, _, _, _, _ -> do
      -- :: Effect Unit
      log $
        "Could not get all the buttons. Please open an issue for this \
        \recipe on the PureScript cookbook."

printMessage :: Event -> Effect Unit
printMessage e = do
  let
    mouseEvent :: MouseEvent
    mouseEvent = unsafeCoerce e
  -- :: Effect Unit
  log "Triggered a mouse click event handler."
  -- :: Effect Unit
  log $ i "Screen X: " (show (screenX mouseEvent))
    "\n\
    \Screen Y: "
    (show (screenY mouseEvent))

-- | Intended usage:
-- | ```
-- | foo = do
-- |   removeEventListener <- addBetterListener click false button \e -> do
-- |      log "Run code when button is clicked..."
-- |   -- when you don't need it anymore, just run the Effect
-- |   removeEventListener -- much simpler
-- | ```
addBetterListener
  :: forall a
   . EventType
  -> Boolean
  -> EventTarget
  -> (Event -> Effect a)
  -> Effect (Effect Unit)
addBetterListener type_ useCaptureRatherThanBubble target listener = do
  -- :: Effect EventListener
  evListener <- eventListener listener
  -- :: Effect Unit
  addEventListener type_ evListener useCaptureRatherThanBubble target
  -- :: Effect (Effect Unit)
  pure $
    -- :: Effect Unit
    removeEventListener type_ evListener useCaptureRatherThanBubble target
