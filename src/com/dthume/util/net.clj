(ns #^{:doc
"Useful clojure abstractions which provide some of facilities found in
java.net in a more clojuresque way."
       :author "David Thomas Hume"}
  com.dthume.util.net)

(defn uri?
  "Returns logical true iff x is a java.net.URI"
  [x] (instance? java.net.URI x))

(defprotocol URIable
  (uri [x] "Return an appropriate URI for x"))

(extend-protocol URIable
  java.net.URI
    (uri [u] u)
  java.io.File
    (uri [f] (.toURI f))
  java.net.URL
    (uri [u] (.toURI u))
  java.lang.String
    (uri [s] (java.net.URI. s)))
