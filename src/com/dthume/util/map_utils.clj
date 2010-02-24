(ns #^{:doc "Commonly functions for processing maps"
       :author "David Thomas Hume"}
  com.dthume.util.map-utils)

(defn filter-keys
  "Returns a map of the entries in m for which (pred (key entry)) is true.
Not lazy"
  [pred m]
  (into {} (filter #(pred (key %)) m)))

(defn filter-vals
  "Returns a map of the entries in m for which (pred (val entry)) is true.
Not lazy"
  [pred m]
  (into {} (filter #(pred (val %)) m)))

(defn map-keys
  "Returns a map of the entries in m, with keys mapped using f"
  [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-vals
  "Returns a map of the entries in m, with vals mapped using f"
  [f m]
  (into {} (map (fn [[k v]] [k (f v)] m))))

(defn derive-keys
  "Takes a map of keys to functions, applies each function to m,
associating each result with the corresponding key."
  ([fmap m]
     (into m (map (fn [[k f]] [k (f m)]) fmap))))

(defn bind*
  "Takes a function and keys, and returns a function which takes a
map and applies f, using the values corresponding to keys as arguments."
  [f & keys]
  (fn [m] (apply f (map m keys))))

(defmacro bind
  "Macro version of bind*"
  [f & keys]
  `(fn [~(symbol "m")] (~f ~@(for [k# keys] `(~(symbol "m") ~k#)))))

(defn apply-keymap
  "Takes a map of keys -> fns and optional fn n-f, and maps over map m,
transforming values with the fn mapped to the corresponding key in km.
n-f, which defaults to identity, is used when (get km k) is logical false."
  ([km m]
     (apply-keymap km identity m))
  ([km n-f m]
     (into {} (for [[k v] m] [k ((or (get km k) n-f) v)]))))
