{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Components (
      ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ReactView, defineView, defineControllerView, viewWithKey
    , Element, Prop, Container, Leaf, Leaf'
    , GenContainer, OtherInput, Link, Label, Button
    , Clickable, HasValue (..)
    , reactRender
    , pageContainer_, pageHeader_, section_
    , h1_
    , div_, p_, span_
    , a_, href, inNewTab, onClick
    , table_, thead_, tbody_, tr_, th_, td_
    , ul_, li_
    , tabs_, tab_, linkTab_, pager_
    , form_, formGroup_, formRow_, formUnlabelledRow_, inputGroup_, inputAddon_
    , TextInput, input_, textarea_, placeholder
    , WordInput, wordInput_
    , Checkbox, checkbox_
    , button_, disabled
    , Flavour (..), alert_
    , JSString, jsPack, jsUnpack, elemStr, elemText
    , IconType (..), iconMeaning_
    , htmlId, className, reactKey, title
    , style, width, displayTable, displayCell, textColour
    , marginTop, marginLow, marginLeft, marginRight, marginX, marginY
    , padTop, padLow, padLeft, padRight, padX, padY
    , AriaRole (..), ariaRole, ariaHidden, ariaSelected, ariaDisabled, ariaLabel, ariaControls
    ) where

import Components.TH (mkContainer, mkRawElem)
import Components.Types

import Data.Monoid ((<>))
import qualified Data.JSString as JSS
import Data.JSString.Text (textToJSString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHCJS.Marshal (FromJSVal, ToJSVal (toJSVal))
import GHCJS.Marshal.Pure (pToJSVal)
import qualified JavaScript.Object as O
import qualified JavaScript.Object.Internal as O
import React.Flux (
      ReactView, defineView, defineControllerView
    , ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ($=), (&=)
    , reactRender)
import qualified React.Flux as F

strProp :: JSString -> JSString -> Prop a
strProp n = Prop . (n $=)
{-# INLINE strProp #-}

jsProp :: (ToJSVal v) => JSString -> v -> Prop a
jsProp n = Prop . (n &=)
{-# INLINE jsProp #-}

-- NOTE: These aren't inverses of each other. This is intentional, if cheeky.
jsUnpack :: Text -> JSString
jsUnpack = textToJSString

jsPack :: String -> JSString
jsPack = JSS.pack

viewWithKey :: (Typeable a) => ReactView a -> JSString -> a -> Element
viewWithKey v k ps = F.viewWithSKey v k ps mempty

elemStr :: JSString -> Element
elemStr = F.elemJSString

elemText :: Text -> Element
elemText = F.elemText

htmlId :: JSString -> Prop a
htmlId = strProp "id"

reactKey :: JSString -> Prop a
reactKey = strProp "key"

className :: JSString -> Prop a
className = strProp "className"

data AriaRole = Alert | Button | Presentation | Search | TabList | Tab
              deriving (Eq, Show)

ariaRoleStr :: AriaRole -> JSString
ariaRoleStr Alert        = "alert"
ariaRoleStr Button       = "button"
ariaRoleStr Presentation = "presentation"
ariaRoleStr Search       = "search"
ariaRoleStr TabList      = "tablist"
ariaRoleStr Tab          = "tab"

ariaRole :: AriaRole -> Prop a
ariaRole = strProp "role" . ariaRoleStr

ariaHidden :: Bool -> Prop a
ariaHidden = jsProp "aria-hidden"

ariaSelected :: Bool -> Prop a
ariaSelected = jsProp "aria-selected"

ariaDisabled :: Bool -> Prop a
ariaDisabled = jsProp "aria-disabled"

ariaLabel :: JSString -> Prop a
ariaLabel = strProp "aria-label"

ariaControls :: JSString -> Prop a
ariaControls = strProp "aria-controls"

title :: JSString -> Prop a
title = strProp "title"

newtype Style = Style { _unStyle :: [(JSString, JSString)] -> [(JSString, JSString)] }

instance ToJSVal Style where
    toJSVal (Style ssf) = do
        o@(O.Object ov) <- O.create
        mapM_ (\(n, v) -> O.unsafeSetProp n (pToJSVal v) o) $ ssf []
        pure ov

instance Monoid Style where
    mempty = Style id
    (Style a) `mappend` (Style b) = Style $ b . a
    {-# INLINE mappend #-}

style :: Style -> Prop a
style s = Prop $ "style" &= s

txtStyle :: JSString -> JSString -> Style
txtStyle k v = Style ((k, v) :)

marginY :: JSString -> Style
marginY v = marginTop v <> marginLow v

marginTop :: JSString -> Style
marginTop = txtStyle "marginTop"

marginLow :: JSString -> Style
marginLow = txtStyle "marginBottom"

marginX :: JSString -> Style
marginX v = marginLeft v <> marginRight v

marginLeft :: JSString -> Style
marginLeft = txtStyle "marginLeft"

marginRight :: JSString -> Style
marginRight = txtStyle "marginRight"

padY :: JSString -> Style
padY v = padTop v <> padLow v

padTop :: JSString -> Style
padTop = txtStyle "paddingTop"

padLow :: JSString -> Style
padLow = txtStyle "paddingBottom"

padX :: JSString -> Style
padX v = padLeft v <> padRight v

padLeft :: JSString -> Style
padLeft = txtStyle "paddingLeft"

padRight :: JSString -> Style
padRight = txtStyle "paddingRight"

width :: JSString -> Style
width = txtStyle "width"

displayTable :: Style
displayTable = txtStyle "display" "table"

displayCell :: Style
displayCell = txtStyle "display" "table-cell"

textColour :: JSString -> Style
textColour = txtStyle "color"

data Button
data GenContainer
data TextInput
data WordInput
data Checkbox
data OtherInput
data Label
data Link
data Decorative

class Clickable a
instance Clickable Button
instance Clickable Link

onClick :: (Clickable a) => (F.Event -> F.MouseEvent -> F.ViewEventHandler) -> Prop a
onClick = Prop . F.onClick

class HasValue a where
    type Value a :: *

    value :: Value a -> Prop a
    default value :: (ToJSVal (Value a)) => Value a -> Prop a
    value = jsProp "value"

    onChange :: (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    default onChange :: (FromJSVal (Value a)) => (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ F.target evt "value"

instance HasValue TextInput where
    type Value TextInput = Text

instance HasValue WordInput where
    type Value WordInput = Word
    -- Otherwise display will be as Int
    value = strProp "value" . jsPack . show

instance HasValue Checkbox where
    type Value Checkbox = Bool
    value = jsProp "checked"
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ F.target evt "checked"

$(mkRawElem ''Button "button")
$(mkRawElem ''GenContainer "table")
$(mkRawElem ''TextInput "textarea")

$(concat <$> traverse (mkContainer ''GenContainer)
    ["h1", "ul", "li", "section", "form", "tr", "td", "th", "thead", "tbody", "p", "nav"])
$(mkContainer ''Label "label")
$(mkContainer ''Link "a")

htmlDiv :: RawElem GenContainer
htmlDiv = present "div"

htmlSpan :: RawElem GenContainer
htmlSpan = present "span"

pageHeader_ :: Container GenContainer
pageHeader_ = containery $ h1 `preset` className "page-header"

pageContainer_ :: Container GenContainer
pageContainer_ = containery $ htmlDiv `preset` className "container"

div_ :: Container GenContainer
div_ = containery htmlDiv

span_ :: Container GenContainer
span_ = containery htmlSpan

data IconType = IconAlert
              | IconDownload
              deriving (Show, Eq)

iconClass :: IconType -> JSString
iconClass IconAlert    = "glyphicon-alert"
iconClass IconDownload = "glyphicon-download"

iconMeaning_ :: IconType -> JSString -> Leaf Decorative
iconMeaning_ typ msg = leafy $ \(ps :: [Prop Decorative]) ->
    present "span" ps $ span_ [className $ "glyphicon " <> iconClass typ, ariaHidden True] mempty
                     <> span_ [className "sr-only"] (elemStr msg)

table_ :: Container GenContainer
table_ = containery $ table `preset` className "table"

tabs_ :: Container GenContainer
tabs_ = containery $ \ps -> nav_ . ul_ (className "nav nav-tabs" : ariaRole TabList : ps)

tab_ :: Bool -> Container GenContainer
tab_ active = containery . act $ li `preset` ariaRole Presentation
  where
    act | active    = (`preset` className "active")
        | otherwise = id

linkTab_ :: Bool -> Container Link
linkTab_ active = containery $ \ps -> tab_ active . a_ (ariaRole Tab : ariaSelected active : ps)

pager_ :: JSString -> Container GenContainer
pager_ lbl = containery $ \ps -> nav_ [ariaLabel lbl] . ul_ (className "pager" : ps)

formRow_ :: JSString -> JSString -> Container GenContainer
formRow_ id_ lbl = containery $ \ps -> formGroup_ (reactKey id_ : ps) . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemStr lbl

formUnlabelledRow_ :: Container GenContainer
formUnlabelledRow_ = containery $ htmlDiv `preset` className "col-md-offset-2 col-md-10"

href :: JSString -> Prop Link
href = strProp "href"

inNewTab :: Prop Link
inNewTab = strProp "target" "_blank"

for :: JSString -> Prop Label
for = strProp "htmlFor"

formGroup_ :: Container GenContainer
formGroup_ = containery $ htmlDiv `preset` className "form-group"

inputGroup_ :: Container GenContainer
inputGroup_ = containery $ htmlDiv `preset` className "input-group"

inputAddon_ :: Container GenContainer
inputAddon_ = containery $ htmlSpan `preset` className "input-group-addon"

button_ :: Container Button
button_ = containery $ button `preset` className "btn btn-default"

disabled :: Bool -> Prop Button
disabled = jsProp "disabled"

formControlInput :: Prop a -> Leaf a
formControlInput pr = leafy $ flip (present "input" `preset` className "form-control" `preset` pr) mempty

input_ :: Leaf TextInput
input_ = formControlInput (strProp "type" "text" :: Prop TextInput)

wordInput_ :: Leaf WordInput
wordInput_ = formControlInput (strProp "type" "number" :: Prop WordInput)

checkbox_ :: Leaf Checkbox
checkbox_ = formControlInput (strProp "type" "checkbox" :: Prop Checkbox)

textarea_ :: Leaf TextInput
textarea_ = leafy $ flip (textarea `preset` className "form-control") mempty

placeholder :: JSString -> Prop TextInput
placeholder = strProp "placeholder"

data Flavour = Success | Info | Warning | Danger deriving (Show, Eq)

flavourSuffix :: Flavour -> JSString
flavourSuffix Success = "success"
flavourSuffix Info    = "info"
flavourSuffix Warning = "warning"
flavourSuffix Danger  = "danger"

alert_ :: Flavour -> Container GenContainer
alert_ flav = containery $ htmlDiv `preset` className ("alert alert-" <> flavourSuffix flav) `preset` ariaRole Alert
