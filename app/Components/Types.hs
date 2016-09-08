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
type Container a = forall i r. (Containery a i r) => i -> r
type Leaf      a = forall r. (Leafy a r) => r
type Leaf'     a = [Prop a] -> Element
type RawElem   a = [Prop a] -> Element -> Element

newtype Prop a = Prop { _unProp :: F.PropertyOrHandler F.ViewEventHandler }

type JSString = F.JSString

present :: JSString -> RawElem a
present n = F.el n . coerce

preset :: RawElem a -> Prop a -> RawElem a
preset f p = f . (p:)
{-# INLINE preset #-}

class (i ~ PropOrElement a r, r ~ ElementOrFun a i) => Containery a i r where
    type PropOrElement a r :: *
    type ElementOrFun  a i :: *
    containery :: ([Prop a] -> Element -> Element) -> i -> r

-- TODO: with GHC 8, a Type Error instance for [Style]
instance (x ~ (), a ~ a') => Containery a [Prop a'] (ElementM x -> ElementM x) where
    type PropOrElement a (ElementM x -> ElementM x) = [Prop a]
    type ElementOrFun  a [Prop a']                  = (Element -> Element)
    containery = id
    {-# INLINE containery #-}

instance (x ~ ()) => Containery a (ElementM x) (ElementM x) where
    type PropOrElement a (ElementM x) = Element
    type ElementOrFun  a (ElementM x) = Element
    containery = ($ [])
    {-# INLINE containery #-}

class Leafy a r where
    leafy :: ([Prop a] -> Element) -> r

instance (x ~ (), a ~ a') => Leafy a ([Prop a'] -> ElementM x) where
    leafy = id
    {-# INLINE leafy #-}

instance (x ~ ()) => Leafy a (ElementM x) where
    leafy = ($ [])
    {-# INLINE leafy #-}
