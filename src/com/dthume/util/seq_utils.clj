(ns #^{:doc "Common functions for processing sequences"
       :author "David Thomas Hume"}
  com.dthume.util.seq-utils)

(defn unchunked-seq
  "Wrap sequence with a dechunking decorator, to ensure full laziness.
Taken from http://blog.fogus.me/2010/01/22/de-chunkifying-sequences-in-clojure/"
  [#^clojure.lang.ISeq s]
  (reify clojure.lang.ISeq
    (first [] (.first s))
    (more [] (unchunked-seq (.more s)))
    (next [] (when-let [sn (.next s)] (unchunked-seq sn)))
    (seq [] (when-let [ss (.seq s)] (unchunked-seq ss)))
    (count [] (.count s))
    (cons [o] (.cons s o))
    (empty [] (.empty s))
    (equiv [o] (.equiv s o))))

(defn map-aggregate
  "Map f across collection; f must be a function of two args: previous-value
and item, and must return the 'next' value. i, if specified, is the initial
first argument to f, otherwise the first item of c is used.  Note that in this
case the first item of c will never be passed to f as an item."
  ([f c]
     (map-aggregate f (first c) (rest c)))
  ([f i c]
     (if (seq c)
       (let [ni (f i (first c))]
         (lazy-seq (cons ni (map-aggregate f ni (rest c)))))
       nil)))

(defn partition-nths
  "Partition c into a sequence of sequences at the indexes contained in nths"
  [nths c]
  (if (and (seq c) (seq nths))
    (let [i (first nths)]
      (lazy-seq
        (cons (take i c) (partition-nths (rest nths) (drop i c)))))
    nil))