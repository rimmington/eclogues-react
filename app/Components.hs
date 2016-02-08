{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Components (
      ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ReactView, defineView, defineControllerView, viewWithKey
    , Element, Prop
    , GenContainer, Input, OtherInput, Link, Label, Button
    , Clickable, HasValue
    , reactRender
    , pageContainer_, pageHeader_, section_
    , h1_
    , div_, p_, span_
    , a_, href, onClick
    , table_, thead_, tbody_, tr_, th_, td_
    , tabs_, tab_
    , form_, formGroup_, formRow_, formUnlabelledRow_, inputGroup_, inputAddon_
    , input_, textarea_, onChange, newValue, value, jsonValue, inputType
    , button_, disabled
    , Flavour (..), alert_
    , elemText
    , htmlId, className, classNames, reactKey
    , style, marginTop
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

reactKey :: (Aeson.ToJSON v) => v -> Prop a
reactKey = jsonProp "key"

className :: Text -> Prop a
className = txtProp "className"

classNames :: [(Text, Bool)] -> Prop a
classNames = Prop . F.classNames

role :: Text -> Prop a
role = txtProp "role"

newtype Style = Style { _unStyle :: [Aeson.Pair] }
              deriving (Monoid)

style :: Style -> Prop a
style (Style ps) = Prop $ "style" @= Aeson.object ps

marginTop :: Text -> Style -> Style
marginTop v (Style ps) = Style $ ("marginTop", Aeson.String v) : ps

data Button
data GenContainer
data Input
data OtherInput
data Label
data Link

class Clickable a
instance Clickable Button
instance Clickable Link

onClick :: (Clickable a) => (F.Event -> F.MouseEvent -> F.ViewEventHandler) -> Prop a
onClick = Prop . F.onClick

class HasValue a
instance HasValue Input
instance HasValue OtherInput

onChange :: (HasValue a) => (F.Event -> F.ViewEventHandler) -> Prop a
onChange = Prop . F.onChange

value :: (HasValue a) => Text -> Prop a
value = txtProp "value"

jsonValue :: (HasValue a, Aeson.ToJSON v) => v -> Prop a
jsonValue = jsonProp "value"

$(mkRawElem ''Button "button")
$(mkRawElem ''GenContainer "table")
$(mkRawElem ''Input "input")
$(mkRawElem ''OtherInput "textarea")

$(concat <$> traverse (mkContainer ''GenContainer)
    ["h1", "li", "section", "form", "tr", "td", "th", "thead", "tbody", "p"])
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
tab_ active = li_ [role "presentation", classNames [("active", active)]]

formRow_ :: Text -> String -> Element -> Element
formRow_ id_ lbl = formGroup_ . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemText lbl

formUnlabelledRow_ :: Element -> Element
formUnlabelledRow_ = div_ [className "col-md-offset-2 col-md-10"]

href :: Text -> Prop Link
href = txtProp "href"

for :: Text -> Prop Label
for = txtProp "htmlFor"

formGroup_ :: Element -> Element
formGroup_ = div_ [className "form-group"]

inputGroup_ :: Element -> Element
inputGroup_ = div_ [className "input-group"]

inputAddon_ :: Element -> Element
inputAddon_ = span_ [className "input-group-addon"]

button_ :: Container Button
button_ = button [className "btn btn-default"]

disabled :: Bool -> Prop Button
disabled = jsonProp "disabled"

input_ :: Leaf Input
input_ ps = input [className "form-control"] ps empty

textarea_ :: Leaf OtherInput
textarea_ ps = textarea [className "form-control"] ps empty

inputType :: Text -> Prop Input
inputType = txtProp "type"

newValue :: (FromJSVal val) => F.Event -> val
newValue evt = F.target evt "value"

data Flavour = Success | Info | Warning | Danger deriving (Show, Eq)

flavourSuffix :: Flavour -> Text
flavourSuffix Success = "success"
flavourSuffix Info    = "info"
flavourSuffix Warning = "warning"
flavourSuffix Danger  = "danger"

alert_ :: Flavour -> Element -> Element
alert_ flav = div_ [className $ "alert alert-" <> flavourSuffix flav, role "alert"]
