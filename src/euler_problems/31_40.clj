(ns euler-problems.31-40
  (:require [clojure.math.combinatorics :as cmb]
            [euler-problems.core :as my]))

;; Problem 31

(defn problem-31 [])

;; Problem 32

(defn problem-32 [])

;; Problem 36

;; Problem 37
(defn rl-trunc [n]
  (take-while #(< 0 %) (iterate #(quot % 10) n)))

(defn lr-trunc [n]
  (take-while #(<= % n) (iterate #(* % 10) 10)))

(defn truncatable? [n]
  (let [lr (map #(my/prime? (mod n %)) (lr-trunc n))
        rl (map my/prime? (rl-trunc n))]
    (every? true? (into rl lr))))

(def prime-trunc
  (comp
   (filter #(> % 10))
   (filter truncatable?)
   (take 11)))

(defn problem-37 []
  (transduce prime-trunc + (my/lazy-generators :primes)))

;; Problem 38
