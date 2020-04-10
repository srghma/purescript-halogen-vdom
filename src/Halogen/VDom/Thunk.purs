module Halogen.VDom.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , hoist
  , mapThunk
  , thunked
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.Machine as M
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Node (Node)

foreign import data ThunkArg ∷ Type

foreign import data ThunkId ∷ Type

-- data Thunk f i = Thunk ThunkId (ThunkArg'a -> ThunkArg'b -> Boolean) (ThunkArg'a → f i) ThunkArg'a

--- widget type can be a thunk
data Thunk f i
  = Thunk
    ThunkId
    (Fn.Fn2 ThunkArg ThunkArg Boolean) -- (oldArg -> newArg -> isEqual)
    (ThunkArg → f i) -- (oldArg -> output)
    ThunkArg -- oldArg

unsafeThunkId ∷ ∀ a. a → ThunkId
unsafeThunkId = unsafeCoerce

instance functorThunk ∷ Functor f ⇒ Functor (Thunk f) where
  map f (Thunk a b c d) = Thunk a b (c >>> map f) d

hoist ∷ ∀ f g. (f ~> g) → Thunk f ~> Thunk g
hoist = mapThunk

-- bimap
mapThunk ∷ ∀ f g i j. (f i -> g j) → Thunk f i -> Thunk g j
mapThunk k (Thunk a b c d) = Thunk a b (c >>> k) d

thunk ∷ ∀ a f i. Fn.Fn4 ThunkId (Fn.Fn2 a a Boolean) (a → f i) a (Thunk f i)
thunk = Fn.mkFn4 \tid eqFn f a →
  Thunk tid
    (unsafeCoerce eqFn ∷ Fn.Fn2 ThunkArg ThunkArg Boolean)
    (unsafeCoerce f ∷ ThunkArg → f i)
    (unsafeCoerce a ∷ ThunkArg)

-- thunk with custom equality
thunked ∷ ∀ a f i. (a → a → Boolean) → (a → f i) → a → Thunk f i
thunked eqFn f =
  let
    tid = unsafeThunkId { f }
    eqFn' = Fn.mkFn2 eqFn
  in
    \a → Fn.runFn4 thunk tid eqFn' f a


{-

var thunked = function (eqFn) {
    return function (f) {
        var tid = unsafeThunkId({
            f: f
        });
        var eqFn$prime = Data_Function_Uncurried.mkFn2(eqFn);
        return function (a) {
            return thunk(tid, eqFn$prime, f, a);
        };
    };
};

var eqString = new Eq($foreign.eqStringImpl);

var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl(eq(dictEq)));
};


-}


{-

thunkCreator :: forall a . Eq a => a -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = fooFn "a"
bar = fooFn "a"

-- `is_eq` would return `true` if `thunked` was written without creating unique reference `tid = unsafeThunkId f`
-- BUT it returns `false` with current implementation `tid = unsafeThunkId { f }`
-- we are forced to fix `a` type beforehand to make it work

is_eq = Fn.runFn2 unsafeEqThunk foo bar

------

var thunkCreator = function (dictEq) {
    return thunked(Data_Eq.eq(dictEq))(function (a) {
        return [ 42 ];
    });
};
var foo = thunkCreator(Data_Eq.eqString)("a");
var bar = thunkCreator(Data_Eq.eqString)("a");
var is_eq = unsafeEqThunk(foo, bar);

------------------------------------------


thunkCreator :: String -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = thunkCreator "a"
bar = thunkCreator "a"

-- `is_eq` would return `true` if `thunked` was written without creating unique reference `tid = unsafeThunkId f`
-- AND it returns `true` with current implementation `tid = unsafeThunkId { f }`
-- everything works correctly

is_eq = Fn.runFn2 unsafeEqThunk foo bar

------

var thunkCreator = thunked(Data_Eq.eq(Data_Eq.eqString))(function (a) {
    return [ 42 ];
});
var foo = thunkCreator("a");
var bar = thunkCreator("a");
var is_eq = unsafeEqThunk(foo, bar);

------------------------------------------


thunkCreator :: forall a . Eq a => a -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = thunkCreator ["a"]
bar = thunkCreator ["a"]

-- `is_eq` would return `false` if `thunked` was written without creating unique reference `tid = unsafeThunkId f`
-- (because `Data_Eq.eqArray` always returns new dictionary)
-- AND it returns `false` with current implementation `tid = unsafeThunkId { f }`.
-- We are forced to fix `a` type beforehand to make it work

is_eq = Fn.runFn2 unsafeEqThunk foo bar

------


var thunkCreator = function (dictEq) {
    return thunked(Data_Eq.eq(dictEq))(function (a) {
        return [ 42 ];
    });
};
var foo = thunkCreator(Data_Eq.eqArray(Data_Eq.eqString))([ "a" ]);
var bar = thunkCreator(Data_Eq.eqArray(Data_Eq.eqString))([ "a" ]);
var is_eq = unsafeEqThunk(foo, bar);

------------------------------------------

thunkCreator :: [String] -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = thunkCreator ["a"]
bar = thunkCreator ["a"]

-- it returns `true` with current implementation

is_eq = Fn.runFn2 unsafeEqThunk foo bar

------

var thunkCreator = thunked(Data_Eq.eq(Data_Eq.eqArray(Data_Eq.eqString)))(function (a) {
    return [ 42 ];
});
var foo = thunkCreator("a");
var bar = thunkCreator("a");
var is_eq = unsafeEqThunk(foo, bar);

------------------------------------------

-}


-- thunk with refEq equality (similar to thunked function)
thunk1 ∷ ∀ a f i. Fn.Fn2 (a → f i) a (Thunk f i)
thunk1 = Fn.mkFn2 \f a → Fn.runFn4 thunk (unsafeThunkId f) Util.refEq f a

thunk2 ∷ ∀ a b f i. Fn.Fn3 (a → b → f i) a b (Thunk f i)
thunk2 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2
  in
    Fn.mkFn3 \f a b →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\{ _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c f i. Fn.Fn4 (a → b → c → f i) a b c (Thunk f i)
thunk3 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2 &&
      Fn.runFn2 Util.refEq a._3 b._3
  in
    Fn.mkFn4 \f a b c →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\{ _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ f i. Thunk f i → f i
runThunk (Thunk _ _ render arg) = render arg

unsafeEqThunk ∷ ∀ f i. Fn.Fn2 (Thunk f i) (Thunk f i) Boolean
unsafeEqThunk = Fn.mkFn2 \(Thunk id eqFn _ renderArg) (Thunk id' eqFn' _ renderArg') →
  Fn.runFn2 Util.refEq id id' &&
  Fn.runFn2 Util.refEq eqFn eqFn' &&
  Fn.runFn2 eqFn renderArg renderArg'

type ThunkState f i a w =
  { thunk ∷ Thunk f i -- old thunk
  , vdom ∷ M.Step (V.VDom a w) Node
  }

buildThunk
  ∷ ∀ f i a w
  . (f i → V.VDom a w)
  → V.VDomSpec a w
  → V.Machine (Thunk f i) Node
buildThunk toVDom = renderThunk
  where
  renderThunk ∷ V.VDomSpec a w → V.Machine (Thunk f i) Node
  renderThunk spec = EFn.mkEffectFn1 \t → do
    vdom ← EFn.runEffectFn1 (V.buildVDom spec) ((toVDom :: (f i → V.VDom a w)) (runThunk t))
    pure $ M.mkStep $ M.Step (M.extract vdom) { thunk: t, vdom } patchThunk haltThunk

  patchThunk ∷ EFn.EffectFn2 (ThunkState f i a w) (Thunk f i) (V.Step (Thunk f i) Node)
  patchThunk = EFn.mkEffectFn2 \state t2 → do
    let { vdom: prev, thunk: t1 } = state
    if Fn.runFn2 unsafeEqThunk t1 t2 -- if eq
      then pure $ M.mkStep $ M.Step (M.extract prev) state patchThunk haltThunk -- dont run effect
      else do
        vdom ← EFn.runEffectFn2 M.step prev (toVDom (runThunk t2)) -- else create new vdom, execute step (compare and patch if need)
        pure $ M.mkStep $ M.Step (M.extract vdom) { vdom, thunk: t2 } patchThunk haltThunk

  haltThunk ∷ EFn.EffectFn1 (ThunkState f i a w) Unit
  haltThunk = EFn.mkEffectFn1 \state → do
    EFn.runEffectFn1 M.halt state.vdom
