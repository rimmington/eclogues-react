{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Components.Types where

import Data.Coerce (coerce)
import qualified React.Flux as F
import qualified React.Flux.Internal as F

type ElementM  a = F.ReactElementM F.ViewEventHandler a
type Element     = ElementM ()
type Container a = forall i r. (Construct a i r) => i -> r
type Leaf      a = forall r. (Leafy a r) => r
type Leaf'     a = [Prop a] -> Element
type RawElem   a = forall i r. (Construct a i r) => [Prop a] -> i -> r

newtype Prop a = Prop { _unProp :: F.PropertyOrHandler F.ViewEventHandler }

type JSString = F.JSString

class (i ~ PropOrElement a r, r ~ ElementOrFun a i) => Construct a i r where
    type PropOrElement a r :: *
    type ElementOrFun  a i :: *
    present :: JSString -> [Prop a] -> i -> r

instance (x ~ (), a ~ a') => Construct a [Prop a'] (ElementM x -> ElementM x) where
    type PropOrElement a (ElementM x -> ElementM x) = [Prop a]
    type ElementOrFun  a [Prop a']                  = (Element -> Element)
    present elemName ps = F.el elemName . coerce . (++ ps)

instance (x ~ ()) => Construct a (ElementM x) (ElementM x) where
    type PropOrElement a (ElementM x) = Element
    type ElementOrFun  a (ElementM x) = Element
    present elemName ps = F.el elemName (coerce ps)

class Leafy a r where
    leafy :: ([Prop a] -> Element) -> r

instance (x ~ (), a ~ a') => Leafy a ([Prop a'] -> ElementM x) where
    leafy = id
    {-# INLINE leafy #-}

instance (x ~ ()) => Leafy a (ElementM x) where
    leafy = ($ [])
    {-# INLINE leafy #-}
