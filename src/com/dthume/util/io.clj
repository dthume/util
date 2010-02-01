(ns #^{:doc
"Useful clojure abstractions which provide some of facilities found in
java.net in a more clojuresque way."
       :author "David Thomas Hume"}
  com.dthume.util.io)

(def file? (partial instance? java.io.File))

(defprotocol Fileable
  (file [x] "Return an appropriate file for x"))

(extend-protocol Fileable
  java.io.File
    (file [f] f)
  java.net.URI
    (file [u] (java.io.File. u))
  java.net.URL
    (file [u] (java.io.File. (.toURI u)))
  java.lang.String
    (file [s] (java.io.File. s)))