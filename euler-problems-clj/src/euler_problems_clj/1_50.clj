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
(defn gen-primes
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

(problem-three 600851475143)

;;===========================================================================|
;; TODO Problem Four -- Palindrome Product
(defn naive-palindrome-search [lower upper]
  (loop [u upper acc []]
    (if (= lower u)
      acc
      (let [prod (* lower u)
            prod-rep (seq (str prod))]
        (if (= (reverse prod-rep) prod-rep)
          (recur (dec u) (conj acc prod))
          (recur (dec u) acc))))))

(defn palindrome-search [lower upper]
  ())

(palindrome-search 10 99)
(palindrome-search 900 999)   ;; ?? pattern does not include ...99
(palindrome-search 1000 9999)

(defn problem-four
  "Find the largest palindrome made from
  the product of two 3-digit numbers"
  [])

;;===========================================================================|
;; TODO Problem Five -- Smallest Multiple
(defn factorize [n]
  (loop [n n acc [1] primes (gen-primes)]
    (if (= n 1)
      acc
      (let [p-factor (first primes)]
        (if (= 0 (mod n p-factor))
          (recur (quot n p-factor) (conj acc p-factor) primes)
          (recur n acc (rest primes)))))))

(defn prime-factorization [n]
  (let [factors (factorize n) d {}]
    (reduce (fn [dict val]
              (if (dict val)
                (assoc dict val (+ 1 (dict val)))
                (assoc dict val 1)))
            d factors)))

;; (f [{a 1 b 2 c 3} {a 2 b 1 c 3} {a 1 b 1 c 1}]) := {a 2 b 2 c 3}
;; given a vector of dicts, return a dict with the maximum values for each key

(defn problem-five
  "finds the smallest number evenly divisible
  by all integers from 1 to `upper-bound`"
  [upper-bound])

(problem-five 10)  ;;EXPECTS -> 2520
(problem-five 20)  ;;expects -> ??

;;===========================================================================|
;; Problem Six -- Sum Square Difference
(defn sum-sq-diff [lim]
  (let [vals (range 1 (+ 1 lim))
        sqsum (math/pow (reduce + vals) 2)
        sumsq (reduce + (map #(math/pow % 2) vals))]
    (- sqsum sumsq)))

;;===========================================================================|
;; Problem Seven -- 10,001st Prime
(nth (gen-primes) 10000) ;;0-indexing

;;===========================================================================|
;; TODO Problem Eight -- Largest Product in a Series

;;===========================================================================|
;; TODO Problem Nine -- Special Pythagorean Triple

;;===========================================================================|
;; TODO Problem Ten --

;;===========================================================================|
;; TODO Problem Eleven --

;;===========================================================================|
;; TODO Problem Twelve -- Highly Divisible Triangular Number
(defn triangle-number
  [n]
  (reduce + (take n (iterate inc 1))))

;; > 500 divisors

;;===========================================================================|
;; TODO Problem Thirteen -- Large Sum
