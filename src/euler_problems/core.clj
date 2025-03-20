(ns euler-problems.core
  (:require [clojure.math :as math]))

;; Generators
(defn fib-generator "infinite lazy fibonnaci sequence"
  []
  ((fn r [a b]
     (lazy-seq
      (cons a (r b (+ a b)))))
   1 1))

(defn prime-generator "(efficient) infinite lazy prime sequence"
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

(def lazy-generators {:fibonnaci (fib-generator)
                      :primes (prime-generator)
                      :integers (iterate inc 1)})

;; Predicates
(defn prime? [n]
  (every? #(not (zero? (mod n %))) (take-while #(< % (math/sqrt n)) (prime-generator))))
