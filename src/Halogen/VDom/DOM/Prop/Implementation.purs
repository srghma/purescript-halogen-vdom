module Halogen.VDom.DOM.Prop.Implementation where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.DOM.Prop.Checkers (checkAttributeExistsAndIsEqual, checkPropExistsAndIsEqual)
import Halogen.VDom.DOM.Prop.Types (ElemRef(..), EmitterInputBuilder, EventListenerAndCurrentEmitterInputBuilder, Prop(..), PropValue)
import Halogen.VDom.DOM.Prop.Util (removeProperty, setProperty, unsafeGetProperty)
import Halogen.VDom.JsSet (JsSet)
import Halogen.VDom.JsSet as JsSet
import Halogen.VDom.Util (STObject', anyToString, fullAttributeName, quote)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM
import Foreign (unsafeToForeign, typeOf)
import Unsafe.Coerce (unsafeCoerce)

deleteRequiredElement :: EFn.EffectFn2 String (JsSet String) Unit
deleteRequiredElement = EFn.mkEffectFn2 \element extraAttributeNames -> do
  isPresent <- EFn.runEffectFn2 JsSet._has element extraAttributeNames
  if isPresent
    then EFn.runEffectFn2 JsSet._delete element extraAttributeNames
    else do
      EFn.runEffectFn2 Util.warnAny "Error info: " { element, extraAttributeNames }
      throwException $ error $ "Cannot delete element " <> quote element <> " that is not present in extraAttributeNames (check warning above for more information)"

checkPropExistsAndIsEqualAndDelete :: EFn.EffectFn5 (JsSet String) String PropValue DOM.Element String Unit
checkPropExistsAndIsEqualAndDelete = EFn.mkEffectFn5 \extraAttributeNames propName val el correspondingAttributeName -> do
  checkPropExistsAndIsEqual propName val el
  EFn.runEffectFn2 deleteRequiredElement correspondingAttributeName extraAttributeNames

hydrateApplyProp
  ∷ ∀ a
  . Fn.Fn4
  (JsSet String)
  DOM.Element
  (a → Effect Unit)
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn3 String Int (Prop a) (Prop a))
hydrateApplyProp = Fn.mkFn4 \extraAttributeNames el emit events → EFn.mkEffectFn3 \_ _ v →
  case v of
    Attribute maybeNamespace attributeName val → do
      checkAttributeExistsAndIsEqual maybeNamespace attributeName val el
      let fullAttributeName' = fullAttributeName maybeNamespace attributeName -- should be lowercased
      EFn.runEffectFn2 deleteRequiredElement fullAttributeName' extraAttributeNames
      pure v
    Property propName val → do
      case propName of
        -- | We use custom check for "href" (i.e. checking attribute instead of property) because:
        -- |   with <a href="/foo"></a>
        -- |     property $0.href is eq to "http://localhost:3000/foo"
        -- |   but attribute
        -- |     $0.attributes.href.value is eq to "/foo"
        -- |     $0.getAttribute("href") is eq to "/foo" too
        -- |
        -- | The same is true for <link> elements also
        "href" -> do
          checkAttributeExistsAndIsEqual Nothing "href" (anyToString val) el
          EFn.runEffectFn2 deleteRequiredElement "href" extraAttributeNames
        -- | these 4 property names are taken from https://github.com/elm/virtual-dom/blob/5a5bcf48720bc7d53461b3cd42a9f19f119c5503/src/Elm/Kernel/VirtualDom.server.js#L196-L201
        "className" -> EFn.runEffectFn5 checkPropExistsAndIsEqualAndDelete extraAttributeNames propName val el "class"
        "htmlFor" -> EFn.runEffectFn5 checkPropExistsAndIsEqualAndDelete extraAttributeNames propName val el "for"
        "httpEquiv" -> EFn.runEffectFn5 checkPropExistsAndIsEqualAndDelete extraAttributeNames propName val el "http-equiv"
        "acceptCharset" -> EFn.runEffectFn5 checkPropExistsAndIsEqualAndDelete extraAttributeNames propName val el "accept-charset"
        _ -> do
          checkPropExistsAndIsEqual propName val el
          case typeOf (unsafeToForeign val), (unsafeCoerce :: PropValue -> Boolean) val of
            -- | If this is a boolean and is false - then it should not have been prerendered
            -- |
            -- | For example:
            -- | `HH.button [HP.disabled false] []` should be rendered as `<button></button>`
            -- | `HH.button [HP.disabled true] []` should be rendered as `<button disabled></button>`
            -- |
            -- | Why it should NOT be rendered at all? Because
            -- |   `<button disabled></button>`         the `$0.disabled === true`
            -- |   `<button disabled="false"></button>` the `$0.disabled === true`
            -- |   `<button disabled="true"></button>`  the `$0.disabled === true`
            -- |   `<button disabled=""></button>`      the `$0.disabled === true`
            -- |   `<button></button>`                  the `$0.disabled === false`
            "boolean", false -> pure unit
            _, _ ->
              let fullAttributeName' = toLower propName -- transforms `colSpan` to `colspan`
               in EFn.runEffectFn2 deleteRequiredElement fullAttributeName' extraAttributeNames

      pure v
    Handler eventType emitterInputBuilder → do
      EFn.runEffectFn5 applyPropHandler el emit events eventType emitterInputBuilder
      pure v
    Ref emitterInputBuilder → do
      EFn.runEffectFn2 mbEmit emit (emitterInputBuilder (Created el))
      pure v

applyProp
  ∷ ∀ a
  . Fn.Fn3
  DOM.Element
  (a → Effect Unit)
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn3 String Int (Prop a) (Prop a))
applyProp = Fn.mkFn3 \el emit events → EFn.mkEffectFn3 \_ _ v →
  case v of
    Attribute maybeNamespace attributeName val → do
      EFn.runEffectFn4 Util.setAttribute (toNullable maybeNamespace) attributeName val el
      pure v
    Property propName val → do
      EFn.runEffectFn3 setProperty propName val el
      pure v
    Handler eventType emitterInputBuilder → do
      EFn.runEffectFn5 applyPropHandler el emit events eventType emitterInputBuilder
      pure v
    Ref emitterInputBuilder → do
      EFn.runEffectFn2 mbEmit emit (emitterInputBuilder (Created el))
      pure v

applyPropHandler
  ∷ ∀ a
  . EFn.EffectFn5
  DOM.Element
  (a -> Effect Unit)
  (STObject' (Tuple DOM.EventListener (Ref.Ref (DOM.Event -> Maybe a))))
  DOM.EventType
  (DOM.Event -> Maybe a)
  Unit
applyPropHandler = EFn.mkEffectFn5 \el emit events (DOM.EventType eventType) emitterInputBuilder →
  case Fn.runFn2 Util.unsafeGetAny eventType events of
    -- if eventType is already present in events storage / listened
    handler | Fn.runFn2 Util.unsafeHasAny eventType events → do
      -- replace current event listener with new
      Ref.write emitterInputBuilder (snd handler)
    _ → do
      ref ← Ref.new emitterInputBuilder
      listener ← DOM.eventListener \ev → do
        (emitterInputBuilder' ∷ EmitterInputBuilder a) ← Ref.read ref
        EFn.runEffectFn2 mbEmit emit (emitterInputBuilder' ev)

      -- set/add to events map, key is eventType, value contains element listener (so we can remove it on halt) AND current emitterInputBuilder
      EFn.runEffectFn3 Util.pokeMutMap eventType (Tuple listener ref) events

      -- listen events of that type on the element
      EFn.runEffectFn3 Util.addEventListener eventType listener el

mbEmit
  ∷ ∀ a
  . EFn.EffectFn2
  (a → Effect Unit)
  (Maybe a)
  Unit
mbEmit = EFn.mkEffectFn2 \emit ma → case ma of
  Just a → emit a
  _ → pure unit

diffProp
  ∷ ∀ a
  . Fn.Fn3
  DOM.Element
  (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn4 String Int (Prop a) (Prop a) (Prop a))
diffProp = Fn.mkFn3 \el prevEvents events → EFn.mkEffectFn4 \_ _ v1 v2 →
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
        -- | In many browsers, though it may not be the case anymore, setting the input value always resets the cursor position/selection.
        -- | This avoids setting it if it has not changed so as not to reset the cursor when you are typing.
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

removeProp
  ∷ ∀ a
  . Fn.Fn2
  DOM.Element
  (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn2 String (Prop a) Unit)
removeProp = Fn.mkFn2 \el prevEvents → EFn.mkEffectFn2 \_ v →
  case v of
    Attribute maybeNamespace attributeName _ →
      EFn.runEffectFn3 Util.removeAttribute (toNullable maybeNamespace) attributeName el
    Property propName _ →
      EFn.runEffectFn2 removeProperty propName el
    Handler (DOM.EventType ty) _ → do
      let
        handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
      EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
    Ref _ →
      pure unit
