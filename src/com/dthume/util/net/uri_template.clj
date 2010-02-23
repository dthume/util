(ns #^{:doc "URI Templates"
       :author "David Thomas Hume"}
  com.dthume.util.net.uri-template
  (:import [java.net URI]
           [java.util.regex Pattern])
  (:refer-clojure :exclude [apply compile])
  (:require [clojure.contrib.seq :as seq]
            [clojure.contrib.string :as string]
            [com.dthume.util.seq-utils :as seq-u]))

(defprotocol URITemplate
  "Protocol for URI Templates"
  (apply [t u] "Apply template t to uri u, returning a map of variables bound from u")
  (#^URI bind [t params] "Apply template t to params, returning a uri")
  (#^boolean matches? [t u] "Boolean true iff template t matches uri u, else false"))

(defprotocol URITemplateSource
  "Protocol for objects which can be compiled into URITemplate instances"
  (compile [s] "Compile a URITemplate from source s"))

(defprotocol URITemplateComponent
  "Protocol for a component of a URITemplate e.g. a string literal, or var expression"
  (#^String re-component [c] "Returns the string regex for this component")
  (#^int re-group-count [c] "Returns the number of capturing groups used by this components re")
  (#^String bind-component [c context])
  (apply-component [c s]))

(defn- compile-var
  "Compile variable name vn into a URITemplateComponent"
  [vn]
  (reify :as this
    URITemplateComponent
      (re-component [] "([^/]+)")
      (re-group-count [] (int 1))
      (bind-component [context] (str (get context vn)))
      (apply-component [m] (hash-map vn (second m)))))

(defmulti compile-op
  "Compile a URITemplateComponent for an operator by op-name"
  (fn [op-name & op-spec] op-name))

(defmethod compile-op :default
  [& args]
  (throw (IllegalArgumentException. (str "Unrecognised op format: " args))))

(defmethod compile-op "opt"
  [op arg vars]
  (let [vars (into #{} vars)]
    (reify URITemplateComponent
      (re-component [] (str "(" (Pattern/quote arg) ")?"))
      (re-group-count [] (int 1))
      (bind-component [context] (if (some #(contains? context %) vars) arg ""))
      (apply-component [s] {}))))

(defmethod compile-op "neg"
  [op arg vars]
  (let [vars (into #{} vars)]
    (reify URITemplateComponent
      (re-component [] (str "(" (Pattern/quote arg) ")?"))
      (re-group-count [] (int 1))
      (bind-component [context] (if (some #(contains? context %) vars) "" arg))
      (apply-component [s] {}))))

(defmethod compile-op "prefix"
  [op arg vars]
  (if (= (count vars) 1)
    (let [quoted-arg (Pattern/quote arg)
          v-name (first vars)]
      (reify URITemplateComponent
        (re-component [] (str "((" quoted-arg "[^" quoted-arg "]*?)*?)??"))
        (re-group-count [] (int 2))
        (bind-component [context]
          (if-let [v-val (get context v-name)]
            (if (instance? clojure.lang.Seqable v-val)
              (reduce #(str %1 arg %2) "" v-val)
              (str arg v-val))
            ""))
        (apply-component [match]
          {v-name (rest (.split (nth match (int 1)) quoted-arg))})))
    (throw (IllegalArgumentException.
             (str "Expected a single variable for prefix operator, found: "
                  vars)))))

(defmethod compile-op "suffix"
  [op arg vars]
  (if (= (count vars) 1)
    (let [quoted-arg (Pattern/quote arg)
          v-name (first vars)]
      (reify URITemplateComponent
        (re-component [] (str "(([^" quoted-arg "]*?(?:" quoted-arg "))*?)??"))
        (re-group-count [] (int 2))
        (bind-component [context]
          (if-let [v-val (get context v-name)]
            (if (instance? clojure.lang.Seqable v-val)
              (reduce #(str %1 %2 arg) "" v-val)
              (str arg v-val))
            ""))
        (apply-component [match]
          {v-name (seq (.split (nth match (int 1)) quoted-arg))})))
    (throw (IllegalArgumentException.
             (str "Expected a single variable for suffix operator, found: "
                  vars)))))

(defmethod compile-op "join"
  [op arg vars]
  (format "[JOIN: %s %s" arg vars))

(defmethod compile-op "list"
  [op arg vars]
  (if (= (count vars) 1)
    (let [quoted-arg (Pattern/quote arg)
          v-name (first vars)]
      (reify URITemplateComponent
        (re-component [] (str "(([^" quoted-arg "]*?"
                                "(?:(?:" quoted-arg ")"
                                "[^" quoted-arg "]*?" ")*?)??)"))
        (re-group-count [] (int 2))
        (bind-component [context]
          (if-let [v-val (get context v-name)]
            (if (instance? clojure.lang.Seqable v-val)
              (reduce #(str %1 %2 arg) "" v-val)
              (throw (IllegalArgumentException.
                      (str "Expected a list value for variable: "
                           v-name ", found: " v-val))))
            ""))
        (apply-component [match]
          {v-name (seq (.split (nth match (int 1)) quoted-arg))})))
    (throw (IllegalArgumentException.
             (str "Expected a single variable for list operator, found: "
                  vars)))))

(let [op-re #"^\-(.*?)\|(.*?)\|(.*?)"]
  (defn- parse-op
    [s]
    (let [[m op arg varlist] (re-matches op-re s)]
      [op arg (map keyword (.split varlist ","))])))

(defn- compile-expr
  "Compile an expression (contained in {} within uri templates)"
  [expr]
  (if (.startsWith expr "-")
    (let [[op arg vars] (parse-op expr)]
      (compile-op op arg vars))
    (compile-var (keyword expr))))

(defn- compile-string-literal
  "Compile literal string s into a URITemplateComponent"
  [#^String s]
  (let [re-c (str "(" (Pattern/quote s) ")")]
    (reify URITemplateComponent
      (re-component [] re-c)
      (re-group-count [] (int 1))
      (bind-component [context] s)
      (apply-component [m] {}))))

(defn- compile-uri-template
  "Compile the given string and sequence of components into a URITemplate"
  [#^String string-rep components]
  (let [template-re (->> components
                         (map #(str "(" (re-component %) ")"))
                         (clojure.core/apply str)
                         re-pattern)
        c-indexes (map (comp inc re-group-count) components)]
    (reify :as this
      URITemplate
        (apply [u]
          (when-let [match (re-matches template-re (str u))]
            (->> (rest match)
                 (seq-u/partition-nths c-indexes)
                 (map apply-component components)
                 (clojure.core/apply merge))))
        (bind [params]
          (->> components
               (map #(bind-component % params))
               (clojure.core/apply str)
               URI.))
        (matches? [u]
          (if (re-matches template-re (str u)) true false))
      URITemplateSource
        (compile [] this)
      Object
        (toString [] string-rep))))

(defn- compile-string
  "Compile s into a URITemplate"
  [#^String s]
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
    (matches? [s u] (matches? (compile-string s) u))
    (apply [s u] (apply (compile-string s) u))
    (bind [s params] (bind (compile-string s) params)))

(comment

  (add-classpath "file:/d:/dthws/util/src/")
  (require '[com.dthume.util.net.uri-template :as uri-t]
           '[clojure.contrib.seq :as seq])
  (def *uri-t* "http://example.org/{foo}?bar={bar}")
  (def *uri-t2* "http://example.org/{-opt|somestring|foo}?bar={bar}")
  (def *uri-t3* "http://example.org/{-neg|somestring|me}?bar={bar}")
  (def *uri-t4* "http://example.org{-prefix|/|slist}?bar={bar}")
  (def *uri-t5* "http://example.org/{-suffix|/|slist}?bar={bar}")
  (def *uri-t6* "http://example.org/{-list|/|slist}?bar={bar}")
  (uri-t/compile *uri-t*)
  (uri-t/matches? *uri-t* "http://example.org/foo-val?bar=bar-val")
  (uri-t/bind *uri-t* {:foo "foo-val" :bar "bar-val"})
  (uri-t/bind *uri-t2* {:foo "foo-val" :bar "bar-val"})
  (uri-t/bind *uri-t2* {:me "foo-val" :bar "bar-val"})
  (uri-t/bind *uri-t3* {:foo "foo-val" :bar "bar-val"})
  (uri-t/bind *uri-t3* {:bar "foo-val" :me "bar-val"})
  (uri-t/bind *uri-t6* {:bar "foo-val" :me "bar-val" :slist ["li1" "li2" "li3"]})
  (uri-t/bind *uri-t4* {:bar "foo-val" :me "bar-val" :slist "li1"})
  (uri-t/bind *uri-t4* {:bar "foo-val" :me "bar-val" :slist nil})
  (uri-t/bind *uri-t4* {:bar "foo-val" :me "bar-val"})
  (uri-t/apply *uri-t2* "http://example.org/foo?bar=bar-val")
  (uri-t/apply *uri-t4* "http://example.org/foo/bar/me?bar=bar-val")
  (uri-t/apply *uri-t6* "http://example.org/foo/bar/me?bar=bar-val")
)