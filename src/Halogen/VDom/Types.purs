module Halogen.VDom.Types
  ( VDom(..)
  , renderWidget
  , Graft
  , GraftX(..)
  , graft
  , unGraft
  , runGraft
  , ElemName(..)
  , Namespace(..)
  ) where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Unsafe.Coerce (unsafeCoerce)

-- | The core virtual-dom tree type, where `a` is the type of attributes,
-- | and `w` is the type of "widgets". Widgets are machines that have complete
-- | control over the lifecycle of some `DOM.Node`.
-- |
-- | The `Grafted` constructor and associated machinery enables `bimap`
-- | fusion using a Coyoneda-like encoding. WHAT???
-- bimap - map a and w
-- coyoneda - obj ((i -> r), f i)

-- what is attributes - e.g. indexed attributes
-- what is widget - e.g. ComponentSlot

data VDom a w
  = Text String
  | Elem (Maybe Namespace) ElemName a (Array (VDom a w))
  | Keyed (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w)))
  | Widget w
  | Grafted (Graft a w)

instance functorVDom ∷ Functor (VDom a) where -- when vdom is mapped - it is not really mapped
  map g (Text a) = Text a
  map g (Grafted a) = Grafted (map g a) -- map graft
  map g a = Grafted (graft (Graft identity g a)) -- create graft, map later, dont map attributes, map widget

instance bifunctorVDom ∷ Bifunctor VDom where
  bimap f g (Text a) = Text a
  bimap f g (Grafted a) = Grafted (bimap f g a)
  bimap f g a = Grafted (graft (Graft f g a))

-- | Replaces "widgets" in the `VDom` with the ability to turn them into other
-- | `VDom` nodes.
-- |
-- | Using this function will fuse any `Graft`s present in the `VDom`.
renderWidget ∷ ∀ a b w x. (a → b) → (w → VDom b x) → VDom a w → VDom b x
renderWidget f g = case _ of
  Text a → Text a
  Elem ns n a ch → Elem ns n (f a) (map (renderWidget f g) ch)
  Keyed ns n a ch → Keyed ns n (f a) (map (map (renderWidget f g)) ch)
  Widget w → g w
  Grafted gaw → renderWidget f g (runGraft gaw)

foreign import data Graft ∷ Type → Type → Type

instance functorGraft ∷ Functor (Graft a) where
  map g = unGraft \(Graft f' g' a) → graft (Graft f' (g <<< g') a)

instance bifunctorGraft ∷ Bifunctor Graft where
  bimap f g = unGraft \(Graft f' g' a) → graft (Graft (f <<< f') (g <<< g') a)

data GraftX a a' w w' =
  Graft (a → a') (w → w') (VDom a w)

graft
  ∷ ∀ a a' w w'
  . GraftX a a' w w'
  → Graft a' w'
graft = unsafeCoerce

unGraft
  ∷ ∀ a' w' r
  . (∀ a w. GraftX a a' w w' → r)
  → Graft a' w'
  → r
unGraft f = f <<< unsafeCoerce

runGraft
  ∷ ∀ a' w'
  . Graft a' w'
  → VDom a' w'
runGraft =
  unGraft \(Graft fa fw v) →
    let
      go (Text s) = Text s
      go (Elem ns n a ch) = Elem ns n (fa a) (map go ch)
      go (Keyed ns n a ch) = Keyed ns n (fa a) (map (map go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (bimap fa fw g) -- what if move it to first?
    in
      go v

newtype ElemName = ElemName String

derive instance newtypeElemName ∷ Newtype ElemName _
derive newtype instance eqElemName ∷ Eq ElemName
derive newtype instance ordElemName ∷ Ord ElemName

newtype Namespace = Namespace String

derive instance newtypeNamespace ∷ Newtype Namespace _
derive newtype instance eqNamespace ∷ Eq Namespace
derive newtype instance ordNamespace ∷ Ord Namespace
