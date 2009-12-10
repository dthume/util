(ns #^{:doc
"Essentially a straight wrapper for clojure.set, but with arguments ordered to
consistently suit ->> where appropriate.  Also contains a few extra utilities
for working with sets and relations"
       :author "David Thomas Hume"}
  com.dthume.util.set
  (:require [clojure.set :as set]
            [com.dthume.util.map-utils :as map-u]))

(defn- as-set
  "Returns s if it is already a set, otherwise (set s)"
  [s]
  (if (set? s) s (set s)))

(defn bubble-max-key
  "Move a maximal element of coll according to fn k (which returns a number) 
to the front of coll. Copied from clojure.set, whose implementation is private"
  [k coll]
  (let [max (apply max-key k coll)]
    (cons max (remove #(identical? max %) coll))))

(def difference set/difference)
(def map-invert set/map-invert)
(def select set/select)

(defn union
  "Return a set that is the union of the input sets.
Like clojure.set/union, but slightly more efficient for larger numbers of sets"
  ([] #{})
  ([s1] s1)
  ([s1 s2]
     (if (< (count s1) (count s2))
       (into (as-set s2) s1)
       (into (as-set s1) s2)))
  ([s1 s2 & sets]
     (let [sets (bubble-max-key count (conj sets s2 s1))]
       (persistent!
        (reduce #(reduce conj! %1 %2)
                (transient (as-set (first sets)))
                (rest sets))))))

(defn intersection
  "Return a set that is the intersection of the input sets.  Like
clojure.set/intersection, but uses transients internally"
  ([s1] s1)
  ([s1 s2]
     (cond
       (empty? s1) s1
       (< (count s2) (count s1)) (recur s2 s1)
       :else
       (let [intersect-pair (fn [result item]
                              (if (contains? s2 item)
                                result
                                (disj! result item)))]
         (persistent! (reduce intersect-pair (transient s1) s1)))))
  ([s1 s2 & sets]
     (let [b-sets (bubble-max-key #(- (count %)) (conj sets s2 s1))]
       (reduce intersection (first b-sets) (rest b-sets)))))

(defn index
  "Like clojure.set/index, but with arguments ordered to suit ->> usage"
  [ks xrel]
  (set/index xrel ks))

(defn join
  "Like clojure.set/join, but with arguments ordered to suit ->> usage"
  ([xrel yrel]
     (set/join xrel yrel))
  ([ks xrel yrel]
     (set/join xrel yrel ks)))

(defn project
  "Like clojure.set/project, but with arguments ordered to suit ->> usage"
  [ks xrel]
  (set/project xrel ks))

(defn !project
  "Inverse of project; removes keys in ks from xrel. Arguments are ordered to
suit ->> usage"
  [ks xrel]
  (set (map #(apply dissoc % ks) xrel)))

(defn project-if
  "Like project, but project only keys matching pred"
  [pred xrel]
  (set (map #(map-u/filter-keys pred %) xrel)))

(defn rename
  "Like clojure.set/rename, but with arguments ordered to suit ->> usage"
  [kmap xrel]
  (set/rename xrel kmap))

(defn rename-keys
  "Like clojure.set/rename-keys, but with arguments ordered to suit ->> usage"
  [kmap map]
  (set/rename-keys map kmap))

(defn transform
  "Like clojure.core/map, but results in a fresh rel. Not lazy"
  [f xrel]
  (set (map f xrel)))

(defn select-key
  "Like select, but apply pred to the value mapped to k in each rel"
  [k pred xrel]
  (select (comp pred k) xrel))

(defn distinct-keys
  "Return a set of the distinct-keys contained in xrels. Not lazy"
  [xrels]
  (apply union (map #(set (keys %)) xrels)))
