{-# LANGUAGE OverloadedStrings #-}

module Components (
      ElementM, Handler
    , pageContainer_, pageHeader_
    , table_
    , tabs_, tab_
    , formGroup_, formRow_, formUnlabelledRow_, button_, input_, textarea_
    , module React.Flux
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import React.Flux hiding (table_, button_, input_, textarea_)
import qualified React.Flux as F

type ElementM  = ReactElementM ViewEventHandler ()
type Handler   = PropertyOrHandler ViewEventHandler
type Container = [Handler] -> ElementM -> ElementM
type Leaf      = [Handler] -> ElementM

pageHeader_ :: ElementM -> ElementM
pageHeader_ = h1_ ["className" $= "page-header"]

pageContainer_ :: ElementM -> ElementM
pageContainer_ = div_ ["className" $= "container"]

table_ :: ElementM -> ElementM
table_ = F.table_ ["className" $= "table"]

tabs_ :: ElementM -> ElementM
tabs_ = F.nav_ . F.ul_ ["className" $= "nav nav-tabs"]

tab_ :: Bool -> Container
tab_ active = F.li_ ["role" $= "presentation", classNames [("active", active)]] .: F.a_ `adding` ["href" $= "#"]

formGroup_ :: ElementM -> ElementM
formGroup_ = div_ ["className" $= "form-group"]

button_ :: Container
button_ = F.button_ `adding` ["className" $= "btn btn-default"]

formRow_ :: Text -> String -> ElementM -> ElementM
formRow_ htmlId lbl = formGroup_ . (label <>) . div_ ["className" $= "col-md-10"]
  where
    label = label_ ["htmlFor" $= htmlId, "className" $= "col-md-2 control-label"] (elemText lbl)

formUnlabelledRow_ :: ElementM -> ElementM
formUnlabelledRow_ = div_ ["className" $= "col-md-offset-2 col-md-10"]

input_ :: Leaf
input_ hs = F.input_ $ "className" $= "form-control" : hs

textarea_ :: Leaf
textarea_ hs = F.textarea_ ("className" $= "form-control" : hs) (pure ())

-- TODO: append instead of overwriting className
adding :: Container -> [Handler] -> Container
adding e hs uhs = e (hs ++ uhs)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

infixr 8 .:
