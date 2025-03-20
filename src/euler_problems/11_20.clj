(ns euler-problems.11-20
  (:require [clojure.math :as math]))

;; ========================|

;; ========================|
(defn triangle-number [n]
  (quot (* n (inc n)) 2))

(defn triangle-seq []
  ())

(defn divisible? [div n]
  (if (= (mod n div) 0)
    (if (= (quot n div) div) 1 2)
    0))

(defn count-divisors [n]
  (reduce + (map #(divisible? % n) (range 1 (math/sqrt n)))))

(defn triangle-sequence []
  ())

(defn problem-twelve
  ""
  [n]
  (->> (triangle-sequence)
       (drop-while #(<= (count-divisors %) n))
       first))

(problem-twelve 500)

;; ========================|
