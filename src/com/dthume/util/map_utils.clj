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