/* global exports, require */
"use strict";

// module Control.Monad.Fix

var message = "Control.Monad.Fix: Premature access to result of fixpoint computation."

function f(x) {
  console.log(x)
  return 1
}

var myobj = { a: f(myobj), b: 2 }

// fixEffect :: forall eff a. ((Unit -> a) -> Eff eff a) -> Eff eff a
exports.fixEffect = function(f) {
  return function() {
    var result = null;
    var ready = false;

    result = f(function(u) {
      if (!ready) throw new Error(message);
      return result;
    })();

    ready = true;
    return result;
  }
}

// fixPure :: forall a. ((Unit -> a) -> a) -> a
exports.fixPure = function(f) {
  return exports.fixEffect(function(a) { return function () { return f(a); }})();
}
