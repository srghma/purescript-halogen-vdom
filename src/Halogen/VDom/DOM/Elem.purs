module Halogen.VDom.DOM.Elem where

import Prelude

import Data.Array (fromFoldable, length) as Array
import Data.Function.Uncurried as Fn
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Uncurried as EFn
import Halogen.VDom.DOM.Checkers (checkIsElementNode, checkTagNameIsEqualTo)
import Halogen.VDom.DOM.Types (VDomBuilder4, VDomHydrator4, VDomMachine, VDomSpec(..), VDomSpecWithHydration(..), VDomStep)
import Halogen.VDom.DOM.Util as DOMUtil
import Halogen.VDom.Machine (Step, Step'(..), extract, halt, mkStep, step)
import Halogen.VDom.Types (ElemName, Namespace, VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.NodeList as DOM.NodeList

type ElemState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Array (VDomStep a w)
  }

hydrateElem
  ∷ ∀ a w
  . VDomHydrator4
    (Maybe Namespace)
    ElemName
    a
    (Array (VDom a w))
    a
    w
hydrateElem = EFn.mkEffectFn8 \currentNode (VDomSpecWithHydration spec) hydrate build ns1 name1 as1 ch1 → do
  currentElement <- checkIsElementNode currentNode
  checkTagNameIsEqualTo ns1 name1 currentElement

  (currentElementChildren :: List DOM.Node) <- DOM.Node.childNodes currentNode >>= DOM.NodeList.toArray <#> List.fromFoldable

  let (currentElementChildren' :: List DOMUtil.ElementOrTextNode) = DOMUtil.listToElementOrTextNode currentElementChildren

  (zippedChildren :: List { node :: DOM.Node, vdom :: VDom a w }) <-
    EFn.runEffectFn6
    DOMUtil.zipChildrenAndSplitTextNodes
    (\(node :: DOMUtil.ElementOrTextNode) (vdom :: VDom a w) -> { node: DOMUtil.elementOrTextNodeToNode node, vdom })
    identity
    (case spec.vdomSpec of VDomSpec vdomSpec -> vdomSpec).document
    currentNode
    currentElementChildren'
    (List.fromFoldable ch1)

  let
    onChild :: { node :: DOM.Node, vdom :: VDom a w } -> Effect (Step (VDom a w) DOM.Node)
    onChild { node, vdom } = EFn.runEffectFn1 (hydrate node) vdom
  (children :: Array (Step (VDom a w) DOM.Node)) <- for zippedChildren onChild <#> Array.fromFoldable
  (attrs :: Step a Unit) ← EFn.runEffectFn1 (spec.hydrateAttributes currentElement) as1
  let
    state =
      { build
      , node: currentNode
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step currentNode state patchElem haltElem

buildElem
  ∷ ∀ a w
  . VDomBuilder4
    (Maybe Namespace)
    ElemName
    a
    (Array (VDom a w))
    a
    w
buildElem = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn3 Util.createElement (toNullable ns1) name1 spec.document
  let
    node :: DOM.Node
    node = DOM.Element.toNode el

    onChild :: EFn.EffectFn2 Int (VDom a w) (Step (VDom a w) DOM.Node)
    onChild = EFn.mkEffectFn2 \ix child → do
      (res :: Step (VDom a w) DOM.Node) ← EFn.runEffectFn1 build child
      EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
      pure res
  children ← EFn.runEffectFn2 Util.forE ch1 onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step node state patchElem haltElem

patchElem ∷ ∀ a w. EFn.EffectFn2 (ElemState a w) (VDom a w) (VDomStep a w)
patchElem = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchElem state (runGraft g)
    Elem ns2 name2 as2 ch2 | Fn.runFn4 Util.eqElemSpec ns1 name1 ns2 name2 → do -- if new vdom is elem AND new and old are equal
      case Array.length ch1, Array.length ch2 of
        0, 0 → do
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: ch1
              }
          pure $ mkStep $ Step node nextState patchElem haltElem
        _, _ → do
          let
            -- both elements are found
            onThese :: EFn.EffectFn3 Int (Step (VDom a w) DOM.Node) (VDom a w) (Step (VDom a w) DOM.Node)
            onThese = EFn.mkEffectFn3 \ix (ch1Elem :: VDomStep a w) (ch2Elem :: VDom a w) → do
              -- execute step function (compare previous dom and ch2Elem), the patchXXX function will be called for ch2Elem element
              -- if elements are different - old element is removed from DOM, replaced with new but not yet attached to DOM
              res ← EFn.runEffectFn2 step ch1Elem ch2Elem
              EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
              pure res

            -- there are no more new elements in the new list, but there is an element in old list
            onThis :: EFn.EffectFn2 Int (Step (VDom a w) DOM.Node) Unit
            onThis = EFn.mkEffectFn2 \ix ch1Elem → EFn.runEffectFn1 halt ch1Elem

            -- there are no more new elements in the old list, but there is an element in new list
            onThat :: EFn.EffectFn2 Int (VDom a w) (Step (VDom a w) DOM.Node)
            onThat = EFn.mkEffectFn2 \ix ch2Elem → do
              res ← EFn.runEffectFn1 build ch2Elem
              EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
              pure res
          (children2 :: Array (Step (VDom a w) DOM.Node)) ← EFn.runEffectFn5 Util.diffWithIxE ch1 ch2 onThese onThis onThat
          (attrs2 :: Step a Unit) ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: children2
              }
          pure $ mkStep $ Step node nextState patchElem haltElem
    _ → do
      EFn.runEffectFn1 haltElem state
      EFn.runEffectFn1 build vdom

haltElem ∷ ∀ a w. EFn.EffectFn1 (ElemState a w) Unit
haltElem = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn2 Util.removeChild node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs
