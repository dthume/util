(ns com.dthume.util.test-set
  (:require [com.dthume.util.set :as s])
  (:use [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn s-range
  [start end]
  (set (range start end)))

(def *1-3* (s-range 1 4))
(def *2-4* (s-range 2 5))
(def *3-5* (s-range 3 6))
(def *4-6* (s-range 4 7))
(def *5-7* (s-range 5 8))
(def *6-8* (s-range 6 9))
(def *7-9* (s-range 7 10))
(def *8-10* (s-range 8 11))

(def *1-5* (s-range 1 6))
(def *6-10* (s-range 6 11))

(def *1-10* (s-range 1 11))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-union
  (is (= *1-5* (s/union *1-3* *3-5*)))
  (is (= *1-10* (s/union *1-5* *6-10*)))
  (is (= *1-5* (s/union *1-5*)))
  (is (= *1-5* (s/union *1-5* *1-5*)))
  (is (empty? (s/union #{})))
  (is (empty? (s/union nil))))
