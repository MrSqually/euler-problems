;; Project Euler --- Problems 1 - 10
;; solutions by: Dean C.

;;===========================================================================|
;; Namespace Declaration
(ns euler-problems.1-10
  (:require [euler-problems.core :as my]
            [clojure.math :as math]))

;;===========================================================================|
;; Solutions

;;==================|
;; Problem One

(defn problem-one
  "find the sum of all multiples of 3 or 5 below UPPER-BOUND"
  [upper-bound]
  (let [fizzbuzz? (fn [x] (or (= (mod x 3) 0)
                              (= (mod x 5) 0)))]
    (reduce + (filter fizzbuzz? (range upper-bound)))))

;;==================|
;; Problem Two

(defn problem-two
  "find the sum of even fibonnaci numbers < UPPER-BOUND"
  [upper-bound]
  (->> (my/lazy-generators :fibonnaci)
       (take-while #(<= % upper-bound))
       (filter even?)
       (reduce +)))

;;==================|
;;Problem Three

(defn problem-three
  "find the largest prime factor of UPPER-BOUND"
  [upper-bound]
  (->> (my/lazy-generators :primes)
       (take-while #(< % (math/sqrt upper-bound)))
       (filter #(= (mod upper-bound %) 0))
       (last)))

;;==================|
;; TODO
(defn problem-four
  ""
  [])

;;==================|
(defn prime-factorization [n]
  (let [factorize (fn [n]
                    (loop [n n acc [1] primes (my/lazy-generators :primes)]
                      (if (= n 1)
                        acc
                        (let [p (first primes)]
                          (if (= 0 (mod n p))
                            (recur (quot n p) (conj acc p) primes)
                            (recur n acc (rest primes)))))))
        factors (factorize n)
        d {}]
    (reduce (fn [res inp]
              (if (res inp)
                (assoc res inp (inc (res inp)))
                (assoc res inp 1)))
            d factors)))

(defn problem-five
  "find the smallest number evenly divisible
  by all integers from `upper-bound`"
  [upper-bound]
  (let [prime-dist (map prime-factorization (range 1 upper-bound))
        max-primes (reduce #(merge-with max %1 %2) prime-dist)]
    (reduce * (map #(math/pow % (max-primes %)) (keys max-primes)))))

;;==================|
(defn problem-six
  "find the sum-square difference of the values from 1 to LIM"
  [lim]
  (let [vals (range (inc lim))
        sqsum (math/pow (reduce + vals) 2)
        sumsq (reduce + (map #(math/pow % 2) vals))]
    (- sqsum sumsq)))

;;==================|
(defn problem-seven
  "Find the 10001st prime number"
  []
  (nth (my/lazy-generators :primes) 10000))

;;==================|
(defn problem-eight
  "find the value of the largest 13
  adjacent element product in the list"
  [adj num]
  (let [[h t] (split-at adj num)
        m (reduce * h)
        stepfn (fn [[tot xs] el]
                 (let [ys (conj (vec (rest xs)) el)
                       out (reduce * ys)]
                   (if (>= out tot) [out ys] [tot xs])))]
    (first (reduce stepfn [m h] t))))

(problem-eight
 13
 (->> "resource/p8/number.txt" slurp vec (map #(Character/digit % 10))))

;;==================|
;; TODO
(defn problem-nine [])
;;==================|
(defn problem-ten
  "find the sums of all prime integers below `upper-bound`"
  [upper-bound]
  (reduce + (take-while #(< % upper-bound) (my/lazy-generators :primes))))
;;===========================================================================|
