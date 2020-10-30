(ns proj.p22
  (:require [proj.utils :refer [cfind]]
            [clojure.string :as str]))

(def N 10007)
(def orig (fn [i] i))

(defn dealNewRev [_]
  (fn [prev]
    (fn [i]
      (- N (prev i) 1))))

(defn cutRev [n]
  (fn [prev]
    (fn [i]
      (mod (- (prev i) n) N))))

(defn dealIncRev [n]
  (fn [prev]
    (fn [i]
      (mod (* (prev i) n) N))))

(defn compile [prog]
  (reduce
    (fn [res [insn arg]]
      ((insn arg) res))
    orig
    prog))

(defn run [p]
  (loop [res (vec (repeat 10 -1))
         left (range 10)]
    (if (empty? left)
      res
      (recur (assoc res (p (first left)) (first left))
             (rest left)))))

(defn transformerRev [l]
  (cond
    (str/starts-with? l "deal into new stack") [dealNewRev nil]
    (str/starts-with? l "cut ") [cutRev (read-string (subs l 4))]
    (str/starts-with? l "deal with increment ") [dealIncRev (read-string (subs l 20))]
    :default (throw (new IllegalStateException))))



(def parsedRev (->> "p22.txt"
                    slurp
                    str/split-lines
                    (mapv transformerRev)))

(defn p1 []
  ((compile parsedRev) 2019))

(defn dealNew [_]
  (fn [prev]
    (fn [i]
      (prev (- N 1 i)))))

(defn cut [n]
  (fn [prev]
    (fn [i]
      (prev (- i n)))))

(defn dealInc [n]
  (fn [prev]
    (fn [i]
      )))
