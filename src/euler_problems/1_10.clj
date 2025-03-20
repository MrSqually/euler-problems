(ns euler-problems.1-10
  (:require [euler-problems.core :as my]
            [clojure.math :as math]))

;;===========================================================================|

;;==================|
(defn problem-one
  "find the sum of all multiples of 3 or 5 below UPPER-BOUND"
  [upper-bound]
  (let [fizzbuzz? (fn [x] (or (= (mod x 3) 0)
                              (= (mod x 5) 0)))]
    (reduce + (filter fizzbuzz? (range upper-bound)))))

;;==================|
(defn problem-two
  "find the sum of even fibonnaci numbers < UPPER-BOUND"
  [upper-bound]
  (->> (my/lazy-generators :fibonnaci)
       (take-while #(<= % upper-bound))
       (filter even?)
       (reduce +)))

;;==================|
(defn problem-three
  "find the largest prime factor of UPPER-BOUND"
  [upper-bound]
  (->> (my/lazy-generators :primes)
       (take-while #(< % (math/sqrt n)))
       (filter #(= (mod n %) 0))
       (last)))

;;==================|
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
  ""
  []
  (nth (my/lazy-generators :primes) 10000))

;;==================|
(defn problem-eight
  ""
  [])

;;==================|
(defn problem-nine [])

;;==================|
(defn problem-ten
  ""
  [upper-bound]
  (reduce + (take-while #(< % upper-bound) (my/lazy-generators :primes))))
;;===========================================================================|
