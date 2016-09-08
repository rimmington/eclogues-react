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
    , a_, href, onClick
    , table_, thead_, tbody_, tr_, th_, td_
    , ul_, li_
    , tabs_, tab_
    , form_, formGroup_, formRow_, formUnlabelledRow_, inputGroup_, inputAddon_
    , TextInput, input_, textarea_, placeholder
    , WordInput, wordInput_
    , Checkbox, checkbox_
    , button_, disabled
    , Flavour (..), alert_
    , JSString, jsPack, jsUnpack, elemStr, elemText
    , htmlId, className, reactKey
    , style, width, displayTable, displayCell
    , marginTop, marginLow, marginLeft, marginRight, marginX, marginY
    , ariaHidden
    ) where

import Components.TH (mkContainer, mkRawElem)
import Components.Types

import Data.Bool (bool)
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

htmlRole :: JSString -> Prop a
htmlRole = strProp "role"

ariaHidden :: Bool -> Prop a
ariaHidden = jsProp "aria-hidden"

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

width :: JSString -> Style
width = txtStyle "width"

displayTable :: Style
displayTable = txtStyle "display" "table"

displayCell :: Style
displayCell = txtStyle "display" "table-cell"

data Button
data GenContainer
data TextInput
data WordInput
data Checkbox
data OtherInput
data Label
data Link

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
    ["h1", "ul", "li", "section", "form", "tr", "td", "th", "thead", "tbody", "p"])
$(mkContainer ''Label "label")
$(mkContainer ''Link "a")

htmlDiv :: RawElem GenContainer
htmlDiv = present "div"

htmlSpan :: RawElem GenContainer
htmlSpan = present "span"

pageHeader_ :: Container GenContainer
pageHeader_ = h1 [className "page-header"]

pageContainer_ :: Container GenContainer
pageContainer_ = htmlDiv [className "container"]

div_ :: Container GenContainer
div_ = htmlDiv []

span_ :: Container GenContainer
span_ = htmlSpan []

table_ :: Container GenContainer
table_ = table [className "table"]

tabs_ :: Element -> Element
tabs_ = F.nav_ . F.ul_ ["className" $= "nav nav-tabs"]

tab_ :: Bool -> Element -> Element
tab_ active = li_ $ htmlRole "presentation" : bool [] [className "active"] active

formRow_ :: JSString -> JSString -> Element -> Element
formRow_ id_ lbl = formGroup_ [reactKey id_] . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemStr lbl

formUnlabelledRow_ :: Element -> Element
formUnlabelledRow_ = div_ [className "col-md-offset-2 col-md-10"]

href :: JSString -> Prop Link
href = strProp "href"

for :: JSString -> Prop Label
for = strProp "htmlFor"

formGroup_ :: Container GenContainer
formGroup_ = htmlDiv [className "form-group"]

inputGroup_ :: Container GenContainer
inputGroup_ = htmlDiv [className "input-group"]

inputAddon_ :: Element -> Element
inputAddon_ = span_ [className "input-group-addon"]

button_ :: Container Button
button_ = button [className "btn btn-default"]

disabled :: Bool -> Prop Button
disabled = jsProp "disabled"

formControlInput :: [Prop a] -> Leaf a
formControlInput ps1 = leafy $ flip (present "input" (className "form-control" : ps1)) mempty

input_ :: Leaf TextInput
input_ = formControlInput ([] :: [Prop TextInput])

wordInput_ :: Leaf WordInput
wordInput_ = formControlInput ([strProp "type" "number"] :: [Prop WordInput])

checkbox_ :: Leaf Checkbox
checkbox_ = formControlInput ([strProp "type" "checkbox"] :: [Prop Checkbox])

textarea_ :: Leaf TextInput
textarea_ = leafy $ flip (textarea [className "form-control"]) mempty

placeholder :: JSString -> Prop TextInput
placeholder = strProp "placeholder"

data Flavour = Success | Info | Warning | Danger deriving (Show, Eq)

flavourSuffix :: Flavour -> JSString
flavourSuffix Success = "success"
flavourSuffix Info    = "info"
flavourSuffix Warning = "warning"
flavourSuffix Danger  = "danger"

alert_ :: Flavour -> Element -> Element
alert_ flav = div_ [className $ "alert alert-" <> flavourSuffix flav, htmlRole "alert"]
