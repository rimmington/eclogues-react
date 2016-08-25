{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Components (
      ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ReactView, defineView, defineControllerView, viewWithKey
    , Element, Prop, Container, Leaf
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
    , elemText
    , htmlId, className, classNames, reactKey
    , style, width, displayTable, displayCell
    , marginTop, marginLow, marginLeft, marginRight, marginX, marginY
    , ariaHidden
    ) where

import Components.TH (mkContainer, mkRawElem)
import Components.Types

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import GHCJS.Marshal (FromJSVal)
import React.Flux (
      ReactView, defineView, defineControllerView, viewWithKey
    , ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ($=), (@=)
    , reactRender)
import qualified React.Flux as F

txtProp :: Text -> Text -> Prop a
txtProp n = Prop . (n $=)

jsonProp :: (Aeson.ToJSON v) => Text -> v -> Prop a
jsonProp n = Prop . (n @=)

empty :: Element
empty = pure ()

elemText :: String -> Element
elemText = F.elemText

htmlId :: Text -> Prop a
htmlId = txtProp "id"

reactKey :: Text -> Prop a
reactKey = txtProp "key"

className :: Text -> Prop a
className = txtProp "className"

classNames :: [(Text, Bool)] -> Prop a
classNames = Prop . F.classNames

htmlRole :: Text -> Prop a
htmlRole = txtProp "role"

ariaHidden :: Bool -> Prop a
ariaHidden = jsonProp "aria-hidden"

newtype Style = Style { _unStyle :: [Aeson.Pair] -> [Aeson.Pair] }

instance Monoid Style where
    mempty = Style id
    (Style a) `mappend` (Style b) = Style $ b . a
    {-# INLINE mappend #-}

style :: Style -> Prop a
style (Style psf) = Prop $ "style" @= Aeson.object (psf [])

txtStyle :: Text -> Text -> Style
txtStyle k v = Style ((k, Aeson.String v) :)

marginY :: Text -> Style
marginY v = marginTop v <> marginLow v

marginTop :: Text -> Style
marginTop = txtStyle "marginTop"

marginLow :: Text -> Style
marginLow = txtStyle "marginBottom"

marginX :: Text -> Style
marginX v = marginLeft v <> marginRight v

marginLeft :: Text -> Style
marginLeft = txtStyle "marginLeft"

marginRight :: Text -> Style
marginRight = txtStyle "marginRight"

width :: Text -> Style
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
    default value :: (Aeson.ToJSON (Value a)) => Value a -> Prop a
    value = jsonProp "value"

    onChange :: (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    default onChange :: (FromJSVal (Value a)) => (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ F.target evt "value"

instance HasValue TextInput where
    type Value TextInput = Text
    value = txtProp "value"

instance HasValue WordInput where
    type Value WordInput = Word

instance HasValue Checkbox where
    type Value Checkbox = Bool
    value = jsonProp "checked"
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
tab_ active = li_ [htmlRole "presentation", classNames [("active", active)]]

formRow_ :: Text -> String -> Element -> Element
formRow_ id_ lbl = formGroup_ [reactKey id_] . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemText lbl

formUnlabelledRow_ :: Element -> Element
formUnlabelledRow_ = div_ [className "col-md-offset-2 col-md-10"]

href :: Text -> Prop Link
href = txtProp "href"

for :: Text -> Prop Label
for = txtProp "htmlFor"

formGroup_ :: Container GenContainer
formGroup_ = htmlDiv [className "form-group"]

inputGroup_ :: Container GenContainer
inputGroup_ = htmlDiv [className "input-group"]

inputAddon_ :: Element -> Element
inputAddon_ = span_ [className "input-group-addon"]

button_ :: Container Button
button_ = button [className "btn btn-default"]

disabled :: Bool -> Prop Button
disabled = jsonProp "disabled"

formControlInput :: [Prop a] -> Leaf a
formControlInput ps1 ps2 = present "input" (className "form-control" : ps1) ps2 empty

input_ :: Leaf TextInput
input_ = formControlInput []

wordInput_ :: Leaf WordInput
wordInput_ = formControlInput [txtProp "type" "number"]

checkbox_ :: Leaf Checkbox
checkbox_ = formControlInput [txtProp "type" "checkbox"]

textarea_ :: Leaf TextInput
textarea_ ps = textarea [className "form-control"] ps empty

placeholder :: Text -> Prop TextInput
placeholder = txtProp "placeholder"

data Flavour = Success | Info | Warning | Danger deriving (Show, Eq)

flavourSuffix :: Flavour -> Text
flavourSuffix Success = "success"
flavourSuffix Info    = "info"
flavourSuffix Warning = "warning"
flavourSuffix Danger  = "danger"

alert_ :: Flavour -> Element -> Element
alert_ flav = div_ [className $ "alert alert-" <> flavourSuffix flav, htmlRole "alert"]
