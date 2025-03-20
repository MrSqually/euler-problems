(ns euler-problems.31-40
  (:require [clojure.math :as math]
            [euler-problems.core :as my]))

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
