module Test.Hydration.JSDOM where

import Effect (Effect)
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Web.HTML.Window as HTML

foreign import data JSDOM ∷ Type

foreign import makeImpl :: EffectFn1 String JSDOM
foreign import windowImpl :: EffectFn1 JSDOM HTML.Window
foreign import serializeImpl :: EffectFn1 JSDOM String

make :: String -> Effect JSDOM
make = runEffectFn1 makeImpl

window :: JSDOM -> Effect HTML.Window
window = runEffectFn1 windowImpl

serialize :: JSDOM -> Effect String
serialize = runEffectFn1 serializeImpl
