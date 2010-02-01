(ns #^{:doc
"Useful clojure abstractions which provide some of facilities found in
java.io in a more clojuresque way."
       :author "David Thomas Hume"}
  com.dthume.util.io
  (:import [java.io File]))

(defn file?
  "Returns logical true iff x is a java.io.File"
  [x] (instance? File x))

(defprotocol Fileable
  (#^File file [x] "Return an appropriate file for x"))

(extend-protocol Fileable
  java.io.File
    (file [f] f)
  java.net.URI
    (file [u] (File. u))
  java.net.URL
    (file [u] (File. (.toURI u)))
  java.lang.String
    (file [s] (File. s)))
