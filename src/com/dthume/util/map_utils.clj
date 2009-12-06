(ns com.dthume.util.map-utils)

(defn filter-keys
  [pred m]
  (into {} (filter #(pred (key %)) m)))

(defn filter-vals
  [pred m]
  (into {} (filter #(pred (val %)) m)))

(defn map-keys
  [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)] m))))