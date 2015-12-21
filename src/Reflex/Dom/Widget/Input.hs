{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts, DataKinds, GADTs, ScopedTypeVariables, FlexibleInstances, RecursiveDo, TemplateHaskell #-}
module Reflex.Dom.Widget.Input (module Reflex.Dom.Widget.Input, def, (&), (.~)) where

import Prelude

import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic

import Reflex
import Reflex.Host.Class
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLSelectElement
import GHCJS.DOM.EventM
import GHCJS.DOM.File
import GHCJS.DOM.FileList
import Data.Bool (bool)
import Data.Monoid
import Data.Map as Map
import Control.Lens
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Safe
import Data.Dependent.Sum (DSum (..))

data TextInput t
   = TextInput { _textInput_value :: Dynamic t String
               , _textInput_input :: Event t String
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_element :: HTMLInputElement
               }

data TextInputConfig t
   = TextInputConfig { _textInputConfig_inputType :: String
                     , _textInputConfig_initialValue :: String
                     , _textInputConfig_setValue :: Event t String
                     , _textInputConfig_attributes :: Dynamic t (Map String String)
                     }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig { _textInputConfig_inputType = "text"
                        , _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_attributes = constDyn mempty
                        }

-- | Create an input whose value is a string.  By default, the "type" attribute is set to "text", but it can be changed using the _textInputConfig_inputType field.  Note that only types for which the value is always a string will work - types whose value may be null will not work properly with this widget.
textInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
textInput (TextInputConfig inputType initial eSetValue dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" inputType) dAttrs
  liftIO $ htmlInputElementSetValue e initial
  performEvent_ $ fmap (liftIO . htmlInputElementSetValue e) eSetValue
  eChange <- wrapDomEvent e elementOninput $ liftIO $ htmlInputElementGetValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> False]
    unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e elementOnkeypress getKeyEvent
  eKeydown <- wrapDomEvent e elementOnkeydown getKeyEvent
  eKeyup <- wrapDomEvent e elementOnkeyup getKeyEvent
  dValue <- holdDyn initial $ leftmost [eSetValue, eChange]
  return $ TextInput dValue eChange eKeypress eKeydown eKeyup dFocus e

textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

data TextAreaConfig t
   = TextAreaConfig { _textAreaConfig_initialValue :: String
                    , _textAreaConfig_setValue :: Event t String
                    , _textAreaConfig_attributes :: Dynamic t (Map String String)
                    }

instance Reflex t => Default (TextAreaConfig t) where
  def = TextAreaConfig { _textAreaConfig_initialValue = ""
                       , _textAreaConfig_setValue = never
                       , _textAreaConfig_attributes = constDyn mempty
                       }

data TextArea t
   = TextArea { _textArea_value :: Dynamic t String
              , _textArea_input :: Event t String
              , _textArea_element :: HTMLTextAreaElement
              , _textArea_hasFocus :: Dynamic t Bool
              , _textArea_keypress :: Event t Int
              }

textArea :: MonadWidget t m => TextAreaConfig t -> m (TextArea t)
textArea (TextAreaConfig initial eSet attrs) = do
  e <- liftM castToHTMLTextAreaElement $ buildEmptyElement "textarea" attrs
  liftIO $ htmlTextAreaElementSetValue e initial
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> False]
    unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  performEvent_ $ fmap (liftIO . htmlTextAreaElementSetValue e) eSet
  f <- holdDyn False eChangeFocus
  ev <- wrapDomEvent e elementOninput $ liftIO $ htmlTextAreaElementGetValue e
  v <- holdDyn initial $ leftmost [eSet, ev]
  eKeypress <- wrapDomEvent e elementOnkeypress getKeyEvent
  return $ TextArea v ev e f eKeypress

data CheckboxConfig t
    = CheckboxConfig { _checkboxConfig_setValue :: Event t Bool
                     , _checkboxConfig_attributes :: Dynamic t (Map String String)
                     }

instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty
                       }

data Checkbox t
   = Checkbox { _checkbox_value :: Dynamic t Bool
              , _checkbox_change :: Event t Bool
              }

--TODO: Make attributes possibly dynamic
-- | Create an editable checkbox
--   Note: if the "type" or "checked" attributes are provided as attributes, they will be ignored
checkbox :: MonadWidget t m => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
  attrs <- mapDyn (\c -> Map.insert "type" "checkbox" $ (if checked then Map.insert "checked" "checked" else Map.delete "checked") c) (_checkboxConfig_attributes config)
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
  eClick <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
  performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ _checkboxConfig_setValue config
  dValue <- holdDyn checked $ leftmost [_checkboxConfig_setValue config, eClick]
  return $ Checkbox dValue eClick

checkboxView :: MonadWidget t m => Dynamic t (Map String String) -> Dynamic t Bool -> m (Event t Bool)
checkboxView dAttrs dValue = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" "checkbox") dAttrs
  eClicked <- wrapDomEvent e elementOnclick $ do
    preventDefault
    liftIO $ htmlInputElementGetChecked e
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ liftIO $ htmlInputElementSetChecked e True
  performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ updated dValue
  return eClicked

data FileInput t
   = FileInput { _fileInput_value :: Dynamic t [File]
               , _fileInput_element :: HTMLInputElement
               }

data FileInputConfig t
   = FileInputConfig { _fileInputConfig_attributes :: Dynamic t (Map String String)
                     }

instance Reflex t => Default (FileInputConfig t) where
  def = FileInputConfig { _fileInputConfig_attributes = constDyn mempty
                        }

fileInput :: MonadWidget t m => FileInputConfig t -> m (FileInput t)
fileInput (FileInputConfig dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" "file") dAttrs
  eChange <- wrapDomEvent e elementOnchange $ liftIO $ do
    Just files <- htmlInputElementGetFiles e
    len <- fileListGetLength files
    mapM (liftM (fromMaybe (error "fileInput: fileListItem returned null")) . fileListItem files) $ init [0..len]
  dValue <- holdDyn [] eChange
  return $ FileInput dValue e

data Dropdown t k
    = Dropdown { _dropdown_value :: Dynamic t k
               , _dropdown_change :: Event t k
               }

data DropdownConfig t k
   = DropdownConfig { _dropdownConfig_setValue :: Event t k
                    , _dropdownConfig_attributes :: Dynamic t (Map String String)
                    }

instance (Reflex t, Ord k, Show k, Read k) => Default (DropdownConfig t k) where
  def = DropdownConfig { _dropdownConfig_setValue = never
                       , _dropdownConfig_attributes = constDyn mempty
                       }

--TODO: We should allow the user to specify an ordering instead of relying on the ordering of the Map
--TODO: Get rid of Show k and Read k by indexing the possible values ourselves
-- | Create a dropdown box
--   The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
dropdown :: forall k t m. (MonadWidget t m, Ord k, Show k, Read k) => k -> Dynamic t (Map k String) -> DropdownConfig t k -> m (Dropdown t k)
dropdown k0 options (DropdownConfig setK attrs) = do
  (eRaw, _) <- elDynAttr' "select" attrs $ do
    optionsWithDefault <- mapDyn (`Map.union` (k0 =: "")) options
    listWithKey optionsWithDefault $ \k v -> do
      elAttr "option" ("value" =: show k <> if k == k0 then "selected" =: "selected" else mempty) $ dynText v
  let e = castToHTMLSelectElement $ _el_element eRaw
  performEvent_ $ fmap (liftIO . htmlSelectElementSetValue e . show) setK
  eChange <- wrapDomEvent e elementOnchange $ do
    kStr <- liftIO $ htmlSelectElementGetValue e
    return $ readMay kStr
  let readKey opts mk = fromMaybe k0 $ do
        k <- mk
        guard $ Map.member k opts
        return k
  dValue <- combineDyn readKey options =<< holdDyn (Just k0) (leftmost [eChange, fmap Just setK])
  return $ Dropdown dValue (attachDynWith readKey options eChange)

data ButtonGroup t a
   = ButtonGroup { _buttonGroup_value :: Dynamic t (Maybe a)
                 , _buttonGroup_change :: Event t (Maybe a)
                 , _buttonGroup_element :: El t
                 , _buttonGroup_children :: Dynamic t (Map Int (El t))
                 }

data ButtonGroupConfig t a
   = ButtonGroupConfig { _buttonGroupConfig_parentType :: String
                       , _buttonGroupConfig_parentTag :: String
                       , _buttonGroupConfig_initialValue :: Maybe a
                       , _buttonGroupConfig_setValue :: Event t (Maybe a)
                       , _buttonGroupConfig_attributes :: Dynamic t (Map String String)
                       }

instance Reflex t => Default (ButtonGroupConfig t a) where
  def = ButtonGroupConfig { _buttonGroupConfig_parentType = ""
                          , _buttonGroupConfig_parentTag = "div"
                          , _buttonGroupConfig_initialValue = Nothing
                          , _buttonGroupConfig_setValue = never
                          , _buttonGroupConfig_attributes = constDyn mempty
                          }

buttonGroup :: (MonadWidget t m, Eq a) => (Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (), El t)) -> Dynamic t (Map Int a) -> ButtonGroupConfig t a -> m (ButtonGroup t a)
buttonGroup drawBtn dynBtns (ButtonGroupConfig pType pTag iVal setV dAtts) = do
  dAtts' <- mapDyn (Map.insert "type" pType) dAtts
  (parent, (dynV, internV, child)) <- elDynAttr' pTag dAtts' $ mdo
    pb <- getPostBuild
    let externSet = attachWith revLookup (current dynBtns) setV
        initSet   = attachWith revLookup (current dynBtns) (iVal <$ pb)
        internSet = leftmost [initSet, clickSelEvts]
        internVal = attachWith (\m k -> k >>= flip Map.lookup m)
                              (current dynBtns)
                              internSet
    dynK     <- holdDyn Nothing $ leftmost [internSet, externSet]
    dynBtns' <- mapDyn (Map.mapKeys Just) dynBtns
    (clickSelEvts, children) <- selectViewListWithKey_' dynK dynBtns' drawBtn
    dynSelV <- combineDyn (\k m -> k >>= flip Map.lookup m) dynK dynBtns
    return (dynSelV, internVal, children)
  return (ButtonGroup { _buttonGroup_value = dynV
                      , _buttonGroup_change = internV
                      , _buttonGroup_element = parent
                      , _buttonGroup_children = child
                      })
 
revLookup :: Eq a => Map Int a -> Maybe a -> Maybe Int
revLookup _ Nothing = Nothing
revLookup m (Just v) = listToMaybe . Map.keys $ Map.filter (== v) m

selectViewListWithKey_' :: forall t m a v.(MonadWidget t m) => Dynamic t (Maybe Int) -> Dynamic t (Map (Maybe Int) v) -> (Maybe Int -> Dynamic t v -> Dynamic t Bool -> m (Event t a, El t)) -> m (Event t (Maybe Int), Dynamic t (Map Int (El t)))
selectViewListWithKey_' selection vals mkChild = do
  let selectionDemux = demux selection
  selectChildAndEl <- listWithKey vals $ \k v -> do
    selected <- getDemuxed selectionDemux k
    (selectSelf, selfEl) <- mkChild k v selected
    return $ (fmap (const k) selectSelf, selfEl)
  selectChild <- mapDyn (Map.map fst) selectChildAndEl
  els <- mapDyn (Map.mapKeys fromJust 
                . Map.mapMaybeWithKey (\k v -> maybe Nothing (const (Just v)) k)
                . Map.map snd) selectChildAndEl
  selEvents <- liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild
  return (selEvents, els)

radioButtons :: (MonadWidget t m, Eq a) => Dynamic t String -> Dynamic t [(a, String)] -> ButtonGroupConfig t a -> m (ButtonGroup t a)
radioButtons dynName dynElems bgConfig0 = do
  btns <- forDyn dynElems $ \choiceElems ->
    Map.fromList $ zip [1..] (Prelude.map fst choiceElems)
  buttonGroup handleOne btns bgConfig0 
    {_buttonGroupConfig_parentTag = parentTag}
  where
    inpPTag   = _buttonGroupConfig_parentTag bgConfig0
    parentTag = if Prelude.null inpPTag then "table" else inpPTag
    handleOne _ dynV dynChecked = do
      (row, clicks) <- el' "tr" $ do
        txt <- combineDyn (\v m -> fromMaybe "" $ Prelude.lookup v m) dynV dynElems
        let aux nm chk = "type" =: "radio" <> "name" =: nm 
                      <> bool mempty ("checked" =: "checked") chk
        btnAttrs <- combineDyn aux dynName dynChecked
        (b,_) <- el "td" $ elDynAttr' "input" btnAttrs $ return ()
        _     <- el "td" $ dynText txt
        let e = castToHTMLInputElement (_el_element b)
        performEvent ((\b -> liftIO (htmlInputElementSetChecked e b)) <$> updated dynChecked)
        return (domEvent Click b)
      return (clicks, row)

liftM concat $ mapM makeLenses
  [ ''TextAreaConfig
  , ''TextArea
  , ''TextInputConfig
  , ''TextInput
  , ''DropdownConfig
  , ''Dropdown
  , ''CheckboxConfig
  , ''Checkbox
  , ''ButtonGroup
  , ''ButtonGroupConfig
  ]

instance HasAttributes (TextAreaConfig t) where
  type Attrs (TextAreaConfig t) = Dynamic t (Map String String)
  attributes = textAreaConfig_attributes

instance HasAttributes (TextInputConfig t) where
  type Attrs (TextInputConfig t) = Dynamic t (Map String String)
  attributes = textInputConfig_attributes

instance HasAttributes (DropdownConfig t k) where
  type Attrs (DropdownConfig t k) = Dynamic t (Map String String)
  attributes = dropdownConfig_attributes

instance HasAttributes (CheckboxConfig t) where
  type Attrs (CheckboxConfig t) = Dynamic t (Map String String)
  attributes = checkboxConfig_attributes

instance HasAttributes (ButtonGroupConfig t a) where
  type Attrs (ButtonGroupConfig t a) = Dynamic t (Map String String)
  attributes = buttonGroupConfig_attributes

class HasSetValue a where
  type SetValue a :: *
  setValue :: Lens' a (SetValue a)

instance HasSetValue (TextAreaConfig t) where
  type SetValue (TextAreaConfig t) = Event t String
  setValue = textAreaConfig_setValue

instance HasSetValue (TextInputConfig t) where
  type SetValue (TextInputConfig t) = Event t String
  setValue = textInputConfig_setValue

instance HasSetValue (DropdownConfig t k) where
  type SetValue (DropdownConfig t k) = Event t k
  setValue = dropdownConfig_setValue

instance HasSetValue (CheckboxConfig t) where
  type SetValue (CheckboxConfig t) = Event t Bool
  setValue = checkboxConfig_setValue

instance HasSetValue (ButtonGroupConfig t a) where
  type SetValue (ButtonGroupConfig t a) = Event t (Maybe a)
  setValue = buttonGroupConfig_setValue

class HasValue a where
  type Value a :: *
  value :: a -> Value a

instance HasValue (TextArea t) where
  type Value (TextArea t) = Dynamic t String
  value = _textArea_value

instance HasValue (TextInput t) where
  type Value (TextInput t) = Dynamic t String
  value = _textInput_value

instance HasValue (FileInput t) where
  type Value (FileInput t) = Dynamic t [File]
  value = _fileInput_value

instance HasValue (Dropdown t k) where
  type Value (Dropdown t k) = Dynamic t k
  value = _dropdown_value

instance HasValue (Checkbox t) where
  type Value (Checkbox t) = Dynamic t Bool
  value = _checkbox_value

instance HasValue (ButtonGroup t a) where
  type Value (ButtonGroup t a) = Dynamic t (Maybe a)
  value = _buttonGroup_value

{-
type family Controller sm t a where
  Controller Edit t a = (a, Event t a) -- Initial value and setter
  Controller View t a = Dynamic t a -- Value (always)

type family Output sm t a where
  Output Edit t a = Dynamic t a -- Value (always)
  Output View t a = Event t a -- Requested changes

data CheckboxConfig sm t
   = CheckboxConfig { _checkbox_input :: Controller sm t Bool
                    , _checkbox_attributes :: Attributes
                    }

instance Reflex t => Default (CheckboxConfig Edit t) where
  def = CheckboxConfig (False, never) mempty

data Checkbox sm t
  = Checkbox { _checkbox_output :: Output sm t Bool
             }

data StateMode = Edit | View

--TODO: There must be a more generic way to get this witness and allow us to case on the type-level StateMode
data StateModeWitness (sm :: StateMode) where
  EditWitness :: StateModeWitness Edit
  ViewWitness :: StateModeWitness View

class HasStateModeWitness (sm :: StateMode) where
  stateModeWitness :: StateModeWitness sm

instance HasStateModeWitness Edit where
  stateModeWitness = EditWitness

instance HasStateModeWitness View where
  stateModeWitness = ViewWitness
-}

