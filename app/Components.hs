{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Components (
      ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ReactView, defineView, defineControllerView, viewWithKey
    , Element
    , Prop, GenContainer, Input, Link, Label, Button
    , reactRender
    , pageContainer_, pageHeader_, section_
    , h1_
    , div_, p_
    , a_, href, onClick
    , table_, thead_, tbody_, tr_, th_, td_
    , tabs_, tab_
    , form_, formGroup_, formRow_, formUnlabelledRow_
    , input_, textarea_, onChange, newValue, value, jsonValue, inputType
    , button_, disabled
    , elemText
    , htmlId, className, classNames, reactKey
    , style, marginTop
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Coerce (coerce)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHCJS.Marshal (FromJSRef)
import React.Flux (
      ReactView, defineView, defineControllerView, viewWithKey
    , ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ($=), (@=)
    , reactRender)
import React.Flux.Internal (el)
import qualified React.Flux as F

type ElementM  a = F.ReactElementM F.ViewEventHandler a
type Element     = ElementM ()
type Container a = forall i r. (Construct a i r) => i -> r
type Leaf      a = [Prop a] -> Element
type RawElem   a = forall i r. (Construct a i r) => [Prop a] -> i -> r

newtype Prop a = Prop { _unProp :: F.PropertyOrHandler F.ViewEventHandler }

class (i ~ PropOrElement a r, r ~ ElementOrFun a i) => Construct a i r where
    type PropOrElement a r :: *
    type ElementOrFun  a i :: *
    present :: String -> [Prop a] -> i -> r

instance (x ~ ()) => Construct a [Prop a] (ElementM x -> ElementM x) where
    type PropOrElement a (ElementM x -> ElementM x) = [Prop a]
    type ElementOrFun  a [Prop a]                   = (Element -> Element)
    present elemName ps = el elemName . coerce . (++ ps)

instance (x ~ ()) => Construct a (ElementM x) (ElementM x) where
    type PropOrElement a (ElementM x) = Element
    type ElementOrFun  a (ElementM x) = Element
    present elemName ps = el elemName (coerce ps)

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

class Clickable a

onClick :: (Clickable a) => (F.Event -> F.MouseEvent -> F.ViewEventHandler) -> Prop a
onClick = Prop . F.onClick

data GenContainer

h1 :: RawElem GenContainer
h1 = present "h1"

htmlDiv :: RawElem GenContainer
htmlDiv = present "div"

li :: RawElem GenContainer
li = present "li"

table :: RawElem GenContainer
table = present "table"

section :: RawElem GenContainer
section = present "section"

form :: RawElem GenContainer
form = present "form"

tr :: RawElem GenContainer
tr = present "tr"

td :: RawElem GenContainer
td = present "td"

th :: RawElem GenContainer
th = present "th"

thead :: RawElem GenContainer
thead = present "thead"

tbody :: RawElem GenContainer
tbody = present "tbody"

p :: RawElem GenContainer
p = present "p"

h1_ :: Container GenContainer
h1_ = h1 []

pageHeader_ :: Container GenContainer
pageHeader_ = h1 [className "page-header"]

pageContainer_ :: Container GenContainer
pageContainer_ = htmlDiv [className "container"]

div_ :: Container GenContainer
div_ = htmlDiv []

li_ :: Container GenContainer
li_ = li []

table_ :: Container GenContainer
table_ = table [className "table"]

tabs_ :: Element -> Element
tabs_ = F.nav_ . F.ul_ ["className" $= "nav nav-tabs"]

tab_ :: Bool -> Element -> Element
tab_ active = li_ [role "presentation", classNames [("active", active)]]

form_ :: Container GenContainer
form_ = form []

formRow_ :: Text -> String -> Element -> Element
formRow_ id_ lbl = formGroup_ . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemText lbl

formUnlabelledRow_ :: Element -> Element
formUnlabelledRow_ = div_ [className "col-md-offset-2 col-md-10"]

section_ :: Container GenContainer
section_ = section []

tr_ :: Container GenContainer
tr_ = tr []

td_ :: Container GenContainer
td_ = td []

th_ :: Container GenContainer
th_ = th []

thead_ :: Container GenContainer
thead_ = thead []

tbody_ :: Container GenContainer
tbody_ = tbody []

p_ :: Container GenContainer
p_ = p []

data Link
instance Clickable Link

a :: RawElem Link
a = present "a"

a_ :: Container Link
a_ = a []

href :: Text -> Prop Link
href = txtProp "href"

data Label

for :: Text -> Prop Label
for = txtProp "htmlFor"

label :: RawElem Label
label = present "label"

label_ :: Container Label
label_ = label []

formGroup_ :: Element -> Element
formGroup_ = div_ [className "form-group"]

data Button
instance Clickable Button

button :: RawElem Button
button = present "button"

button_ :: Container Button
button_ = button [className "btn btn-default"]

disabled :: Bool -> Prop Button
disabled = jsonProp "disabled"

data Input

input :: RawElem Input
input = present "input"

textarea :: RawElem Input
textarea = present "textarea"

input_ :: Leaf Input
input_ ps = input [className "form-control"] ps empty

textarea_ :: Leaf Input
textarea_ ps = textarea [className "form-control"] ps empty

value :: Text -> Prop Input
value = txtProp "value"

jsonValue :: (Aeson.ToJSON a) => a -> Prop Input
jsonValue = jsonProp "value"

inputType :: Text -> Prop Input
inputType = txtProp "type"

onChange :: (F.Event -> F.ViewEventHandler) -> Prop Input
onChange = Prop . F.onChange

newValue :: (FromJSRef val) => F.Event -> val
newValue evt = F.target evt "value"
