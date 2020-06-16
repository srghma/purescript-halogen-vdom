module Halogen.VDom.Set where

import Prelude (Unit)

import Effect (Effect)
import Effect.Uncurried (EffectFn2) as EFn
import Data.Function.Uncurried as Fn

data Set proxy

foreign import empty ∷ ∀ a . Effect (Set a)

foreign import delete ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import add ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import size ∷ ∀ a . Set a → Int

foreign import has ∷ ∀ a . Fn.Fn2 a (Set a) Boolean

foreign import toArray ∷ ∀ a . Set a → Array a
