(ns #^{:doc "Commonly functions for processing maps"
       :author "David Thomas Hume"}
  com.dthume.util.map-utils
  (:import [clojure.lang MapEntry]))

(defn filter-keys
  "Returns a lazy sequence of the entries in m for which (pred (key entry))
is true."
  [pred m]
  (filter (comp pred key) m))

(defn filter-vals
  "Returns a lazy sequence of the entries in m for which (pred (val entry))
is true."
  [pred m]
  (filter (comp pred val) m))

(defn bind-key
  "Takes a function, returns a function that takes a map entry, and returns
a new map entry of [(f (key e)) (val e)]"
  [f]
  (fn [e] (MapEntry. (f (key e)) (val e))))

(defn bind-val
  "Takes a function, returns a function that takes a map entry, and returns
a new map entry of [(key e) (f (val e))]"
  [f]
  (fn [e] (MapEntry. (key e) (f (val e)))))

(defn derive-keys
  "Takes a map of keys to functions, applies each function to m,
associating each result with the corresponding key."
  ([fmap m]
     (into m (map (fn [[k f]] (MapEntry. k (f m))) fmap))))

(defn bind*
  "Takes a function and keys, and returns a function which takes a
map and applies f, using the values corresponding to keys as arguments."
  [f & keys]
  (fn [m] (apply f (map m keys))))

(defmacro bind
  "Macro version of bind*"
  [f & keys]
  `(fn [~(symbol "m")] (~f ~@(for [k# keys] `(~(symbol "m") ~k#)))))

(defn map-by-key
  "Takes a map of keys -> fns km and optional fn n-f, and returns a function
which takes a map entry, and returns a new mapping from the key to the result
of applying the function mapped to the corresponding key in km. n-f, which
defaults to identity, is used when the value mapped to k in km is logical
false."
  ([km]
     (map-by-key km identity))
  ([km n-f]
     (fn [[k v]] (MapEntry. k ((or (get km k) n-f) v)))))
