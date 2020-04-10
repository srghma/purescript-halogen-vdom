module Halogen.VDom.DOM.Prop
  ( Prop(..)
  , ElemRef(..)
  , PropValue
  , propFromString
  , propFromBoolean
  , propFromInt
  , propFromNumber
  , buildProp
  ) where

import Prelude

{-- import Foreign.Object.ST (STObject) --}
{-- import Foreign.Object.ST as STObject --}
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (null, toNullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Foreign.Object as Object
import Halogen.VDom as V
import Halogen.VDom.Machine (Step, Step'(..), mkStep)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.

-- | Attributes are defined by HTML. Properties (on DOM elements) are defined by DOM
-- |
-- | e.g. `class` attribute corresponds to `element.className` property
-- | almost always you should use properties on html elements, the svg elements don't have properties, only classes
-- | more https://github.com/purescript-halogen/purescript-halogen-vdom/issues/30#issuecomment-518015764
-- |
-- | attributes can be only strings, props - string, number, bool
data Prop a
  = Attribute
    (Maybe Namespace) -- XML namespace
    String -- attribute
    String -- value
  | Property
    String -- property, e.g. htmlFor (for attribute), className (class attribute) -}
    PropValue
  | Handler
    DOM.EventType -- listen only this event type, dont react on others
    (DOM.Event → Maybe a) -- build input for emitter (https://github.com/purescript-halogen/purescript-halogen/blob/bb715fe5c06ba3048f4d8b377ec842cd8cf37833/src/Halogen/Query/Input.purs#L15-L17)
    -- NOTE:
    -- in `H.div [HP.eventHandler (...), HP.eventHandler (...)]` - only last event handler is going to work
  | Ref (ElemRef DOM.Element → Maybe a)

instance functorProp ∷ Functor Prop where
  map f (Handler ty g) = Handler ty (map f <$> g)
  map f (Ref g) = Ref (map f <$> g)
  map f p = unsafeCoerce p

data ElemRef a
  = Created a
  | Removed a

instance functorElemRef ∷ Functor ElemRef where
  map f (Created a) = Created (f a)
  map f (Removed a) = Removed (f a)

foreign import data PropValue ∷ Type

propFromString ∷ String → PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean → PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int → PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number → PropValue
propFromNumber = unsafeCoerce

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.

-- | emitter vs effects handler:
-- |   if Prop is attribute or property - nothing is emitted
-- |   if Prop is event handler - emitter has type ``????
-- |   if Prop is ref - emitter has type ``????
buildProp
  ∷ ∀ a
  . (a → Effect Unit) -- emitter
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit -- TODO: why array of properties??????????????????????????????????????
buildProp emit el = renderProp
  where
  -- what it does - creates a machine, that contains state
  -- on next step - patch prop ?
  -- on halt - all ref watchers are notified that element is removed
  -- TODO: when events
  -- TODO: how to unwrap machine
  renderProp :: EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    -- events :: STObject region (Tuple DOM.EventListener (Ref.Ref (DOM.Event -> Maybe a)))

    -- e.g. event for property "class", listen only
    events ← Util.newMutMap

    -- for each prop in array:
    --   if prop is attr - set attr to element, store attr under "attr/XXX" key in a returned object
    --   if prop is property - set property to element, store property under "prop/XXX" key in a returned object
    --   if prop is handler for DOM.EventType - start listen and add listener to `events` mutable map, store handler under "handler/EVENTTYPE" in a returned object
    --   if prop is ref updater - store `emitterInputBuilder` in under a `ref` key in a returned object, call `emitter` on creation of all props (now) and on halt of all props (later)
    (props :: Object.Object (Prop a)) ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (applyProp events)
    let
      state =
        { events: Util.unsafeFreeze events -- Object (Tuple DOM.EventListener (Ref.Ref (DOM.Event -> Maybe a)))
        , props
        }
    pure $ mkStep $ Step unit state patchProp haltProp

  patchProp :: EFn.EffectFn2 { events :: Object.Object (Tuple DOM.EventListener (Ref.Ref (DOM.Event -> Maybe a))) , props :: Object.Object (Prop a) } (Array (Prop a)) (Step (Array (Prop a)) Unit)
  patchProp = EFn.mkEffectFn2 \state ps2 → do
    events ← Util.newMutMap
    let
      { events: prevEvents, props: ps1 } = state
      onThese = Fn.runFn2 diffProp prevEvents events
      onThis = removeProp prevEvents
      onThat = applyProp events
    props ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
    let
      nextState =
        { events: Util.unsafeFreeze events
        , props
        }
    pure $ mkStep $ Step unit nextState patchProp haltProp

  haltProp = EFn.mkEffectFn1 \state → do
    case Object.lookup "ref" state.props of
      Just (Ref emitterInputBuilder) →
        EFn.runEffectFn1 mbEmit (emitterInputBuilder (Removed el))
      _ → pure unit

  mbEmit = EFn.mkEffectFn1 case _ of
    Just a → emit a
    _ → pure unit

  {-- applyProp :: STObject t0 (Tuple EventListener (Ref (Event -> Maybe a))) -> EffectFn3 String Int (Prop a) (Prop a) --}
  applyProp events = EFn.mkEffectFn3 \_ _ v →
    case v of
      Attribute ns attr val → do
        EFn.runEffectFn4 Util.setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        EFn.runEffectFn3 setProperty prop val el
        pure v
      Handler (DOM.EventType eventType) emitterInputBuilder → do
        case Fn.runFn2 Util.unsafeGetAny eventType events of
          -- if eventType is already present/listened - in events storage
          handler | Fn.runFn2 Util.unsafeHasAny eventType events → do
            Ref.write emitterInputBuilder (snd handler) -- replace current event listener with new
            pure v
          _ → do
            ref ← Ref.new emitterInputBuilder
            listener ← DOM.eventListener \ev → do
              (emitterInputBuilder' :: DOM.Event -> Maybe a) ← Ref.read ref
              EFn.runEffectFn1 mbEmit (emitterInputBuilder' ev)

            -- set/add to events map, key is eventType, value contains element listener (so we can remove it on halt) AND current emitterInputBuilder
            EFn.runEffectFn3 Util.pokeMutMap eventType (Tuple listener ref) events

            -- listen events of that type on the element
            EFn.runEffectFn3 Util.addEventListener eventType listener el
            pure v
      Ref emitterInputBuilder → do
        EFn.runEffectFn1 mbEmit (emitterInputBuilder (Created el))
        pure v

  -- diffProp :: Fn2 (Object (Tuple EventListener (Ref (Event -> Maybe a)))) (STObject t1 (Tuple EventListener (Ref (Event -> Maybe a)))) (EffectFn4 String Int (Prop a) (Prop a) (Prop a))
  diffProp = Fn.mkFn2 \prevEvents events → EFn.mkEffectFn4 \_ _ v1 v2 →
    case v1, v2 of
      Attribute _ _ val1, Attribute ns2 attr2 val2 →
        if val1 == val2
          then pure v2
          else do
            EFn.runEffectFn4 Util.setAttribute (toNullable ns2) attr2 val2 el
            pure v2
      Property _ val1, Property prop2 val2 →
        case Fn.runFn2 Util.refEq val1 val2, prop2 of
          true, _ →
            pure v2
          _, "value" → do
            let elVal = Fn.runFn2 unsafeGetProperty "value" el
            if Fn.runFn2 Util.refEq elVal val2
              then pure v2
              else do
                EFn.runEffectFn3 setProperty prop2 val2 el
                pure v2
          _, _ → do
            EFn.runEffectFn3 setProperty prop2 val2 el
            pure v2
      Handler _ _, Handler (DOM.EventType ty) emitterInputBuilder → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Ref.write emitterInputBuilder (snd handler)
        EFn.runEffectFn3 Util.pokeMutMap ty handler events
        pure v2
      _, _ →
        pure v2

  removeProp prevEvents = EFn.mkEffectFn2 \_ v →
    case v of
      Attribute ns attr _ →
        EFn.runEffectFn3 Util.removeAttribute (toNullable ns) attr el
      Property prop _ →
        EFn.runEffectFn2 removeProperty prop el
      Handler (DOM.EventType ty) _ → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
      Ref _ →
        pure unit

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"

setProperty ∷ EFn.EffectFn3 String PropValue DOM.Element Unit
setProperty = Util.unsafeSetAny

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element PropValue
unsafeGetProperty = Util.unsafeGetAny

-- removes if attr exists using el.removeAttributeNS() or el.removeAttribute()
-- if property - sets to "", or undefined, or sets rowSpan or collSpan to 1
removeProperty ∷ EFn.EffectFn2 String DOM.Element Unit
removeProperty = EFn.mkEffectFn2 \key el →
  EFn.runEffectFn3 Util.hasAttribute null {- Nullable Namespace -} key el >>= if _
    then EFn.runEffectFn3 Util.removeAttribute null key el
    else case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
      "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el
      _        → case key of
        "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
