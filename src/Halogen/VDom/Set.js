exports.empty = function() {
  return new Set()
}

exports.delete = function(value, set) {
  set.delete(value)
}

exports.add = function(value, set) {
  set.add(value)
}

exports.size = function(set) {
  return set.size
}

exports.toArray = function(set) {
  return Array.from(set)
}
