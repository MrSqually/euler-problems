(ns euler-problems-clj.1-50
  (:require [clojure.math :as math]))

;;===========================================================================|
;; Problem One

(defn problem-one
  "find the sum of all multiples of 3 or 5 below 1000"
  [upper-bound]
  (let [pred? (fn [x] (or (= (mod x 3) 0)
                          (= (mod x 5) 0)))]
    (reduce + (filter pred? (range upper-bound)))))

(problem-one 1000)

;;===========================================================================|
;; Problem Two
(defn bounded-fibonnaci-seq
  "produce the fibonnaci sequence, up
  to `n` upper bound"
  [n]
  (take-while #(< % n)
              ((fn r [a b]
                 (lazy-seq
                  (cons a (r b (+ a b)))))
               1 1)))

(defn problem-two
  "Find the sum of even fibonnaci numbers
  whose values do not exceed 4,000,000"
  [upper-bound]
  (reduce +
          (filter #(= (mod % 2) 0) (bounded-fibonnaci-seq upper-bound))))

(problem-two 4000000)

;;===========================================================================|
;; Problem Three

(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
  []
  (letfn [(reinsert [table x prime]
            (update-in table [(+ prime x)] conj prime))
          (primes-step [table d]
            (if-let [factors (get table d)]
              (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                     (inc d))
              (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                             (inc d))))))]
    (primes-step {} 2)))

(defn problem-three
  "find the largest prime factor of
  the number 600851475143"
  [n]
  (let [primes (gen-primes)]
    (last (filter #(= (mod n %) 0) (take-while #(< % (math/sqrt n)) primes)))))

(problem-three 13195)

(problem-three 600851475143)

;;===========================================================================|
;; Problem Four -- Palindrome Product

(defn problem-four
  "Find the largest palindrome made from
  the product of two 3-digit numbers"
  [])

;;===========================================================================|
;; Problem Five -- Smallest Multiple
(defn factorize [n]
  (let [primes (gen-primes)]
    (loop [primes primes n n]
      (let [p (first primes)]
        (if (= (mod n p) 0)
          (recur primes (/ n p))
          (recur (rest primes) n))))))

(factorize 10) ;; EXPECTS -> 2,5
(factorize 9) ;; expects -> 3,3
(factorize 2520) ;;expects -> 2,2,2,3,3,5,7

(defn problem-five
  "finds the smallest number evenly divisible
  by all integers from 1 to `upper-bound`"
  [upper-bound])

(problem-five 10)  ;;EXPECTS -> 2520
