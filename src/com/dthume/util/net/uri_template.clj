(ns #^{:doc "URI Templates"
       :author "David Thomas Hume"}
  com.dthume.util.net.uri-template
  (:import [java.net URI]
           [java.util.regex Pattern])
  (:refer-clojure :exclude [apply compile])
  (:require [clojure.contrib.seq :as seq]
            [clojure.contrib.string :as string]))

(defprotocol URITemplate
  "Protocol for URI Templates"
  (apply [t u] "Apply template t to uri u, returning a map")
  (#^URI bind [t params] "Apply template t to params, returning a uri")
  (matches? [t u] "True iff template t matches uri u"))

(defprotocol URITemplateSource
  "Protocol for objects which can be compiled into URITemplate instances"
  (compile [s] "Compile a URITemplate from source s"))

(defprotocol URITemplateComponent
  (#^String re-component [c] "Returns the string regex for this component")
  (#^int re-group-count [c] "Returns the number of capturing groups used by this components re")
  (#^String bind-component [c context])
  (apply-component [c s]))

(defn- compile-var
  [vn]
  (let [vnk (keyword vn)]
    (reify :as this
      URITemplateComponent
        (re-component [] "([^/]+)")
        (re-group-count [] (int 1))
        (bind-component [context] (get context vnk))
        (apply-component [s]))))

(defmulti compile-op
  ""
  (fn [& args] (.toLowerCase (first args))))

(defmethod compile-op :default
  [& args]
  (throw (IllegalArgumentException. "Unrecognised op format: " args)))

(defmethod compile-op "opt"
  [op arg vars]
  (reify URITemplateComponent
    (re-component [] (str "(" (Pattern/quote arg) ")?"))
    (re-group-count [] (int 1))
    (bind-component [context] (if (some identity context) arg ""))
    (apply-component [s]
      ; todo
      )))

(defmethod compile-op "neg"
  [op arg vars]
  (format "[NEG: %s %s" arg vars))

(defmethod compile-op "prefix"
  [op arg vars]
  (format "[PREFIX: %s %s" arg vars))

(defmethod compile-op "suffix"
  [op arg vars]
  (format "[SUFFIX: %s %s" arg vars))

(defmethod compile-op "join"
  [op arg vars]
  (format "[JOIN: %s %s" arg vars))

(defmethod compile-op "list"
  [op arg vars]
  (format "[LIST: %s %s" arg vars))

(let [op-re #"^\-(.*?)\|(.*?)\|(.*?)$"]
  (defn- parse-op
    [s]
    (let [[m op arg varlist] (re-matches op-re s)]
      [op arg (seq (.split varlist ","))])))

(defn- compile-expr
  [expr]
  (if (.startsWith expr "-")
    (let [[op arg vars] (parse-op expr)]
      (compile-op op arg vars))
    (compile-var expr)))

(defn- compile-string-literal
  [#^String s]
  (let [re-c (Pattern/quote s)]
    (reify URITemplateComponent
      (re-component [] re-c)
      (re-group-count [] (int 0))
      (bind-component [context] re-c)
      (apply-component [m] {}))))

(defn- compile-group-index
  [components]
  (let []))

(defn- compile-component-matcher
  [components]
  (let [compile-component (fn [c] (hash-map :ro))
        gc (->> components (map re-group-count) seq/indexed)]))

(defn- compile-uri-template
  [#^String string-rep components]
  (let [template-re
        (delay
          (->> components
               (map #(str "(" (re-component %) ")"))
               (clojure.core/apply str)
               re-pattern))
        component-group-index
        (delay
          (->> components (map re-group-count) seq/indexed))]
    (reify :as this
      URITemplateSource
        (compile [] this)
      URITemplate
        (apply [u]
          )
        (bind [params]
          (->> components
               (map #(bind-component % params))
               (clojure.core/apply str)
               URI.))
        (matches? [u]
          (re-matches (force template-re) u))
      Object
        (#^String toString [] string-rep))))

(defn- compile-string
  [s]
  (let [template-re #"\{(.*?)\}"
        expr-group (int 1)
        compile-result (fn [m]
                         (if (string? m)
                           (compile-string-literal m)
                           (compile-expr (nth m expr-group))))
        components (map compile-result (string/partition template-re s))]
    (compile-uri-template s components)))

(extend-class String
  URITemplateSource
    (compile [s] (compile-string s))
  URITemplate
    (matches? [s u]
      (matches? (compile s) u))
    (apply [s u]
      (apply (compile s) u))
    (bind [s params]
      (bind (compile s) params)))

(comment

  (add-classpath "file:/d:/dthws/util/src/")
  (require '[com.dthume.util.net.uri-template :as uri-t]
           '[clojure.contrib.seq :as seq])
  (seq/indexed (range 5))
  (uri-t/compile "http://example.org/?q={bar}")
  (uri-t/compile "http://example.org/?{-join|&|foo,bar,xyzzy,baz}")
)