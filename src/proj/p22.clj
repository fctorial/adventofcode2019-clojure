(ns proj.p22
  (:require [proj.utils :refer [cfind]]
            [clojure.string :as str]))

(def N1 10007)
(def orig (fn [i] (bigint i)))

(defn dealNewRev [_]
  (fn [prev]
    (fn [i]
      (- N1 (prev i) 1))))

(defn cutRev [n]
  (fn [prev]
    (fn [i]
      (mod (- (prev i) n) N1))))

(defn dealIncRev [n]
  (fn [prev]
    (fn [i]
      (mod (* (prev i) n) N1))))

(defn compile [prog]
  (reduce
    (fn [res [insn arg]]
      ((insn arg) res))
    orig
    prog))

(defn runRev [p]
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

(def N2 119315717514047)

(defn dealNew [_]
  (fn [prev]
    (fn [i]
      (prev (- N2 1 i)))))

(defn cut [n]
  (fn [prev]
    (fn [i]
      (prev (mod (+ i n) N2)))))

(defn dealInc [n]
  (fn [prev]
    (fn [i]
      (prev (mod (* i (- N2 (mod n N2))) N2)))))

(defn run [p]
  (mapv #(p %) (range 10)))

(defn transformer [l]
  (cond
    (str/starts-with? l "deal into new stack") [dealNew nil]
    (str/starts-with? l "cut ") [cut (bigint (read-string (subs l 4)))]
    (str/starts-with? l "deal with increment ") [dealInc (bigint (read-string (subs l 20)))]
    :default (throw (new IllegalStateException))))

(def parsed (->> "p22.txt"
                 slurp
                 str/split-lines
                 (mapv transformer)))

(def prog2 (compile parsed))

(defn p2 []
  (loop [seen 0
         curr 2020]
    (let [nxt (prog2 curr)]
      (if (= nxt 2020)
        seen
        (recur (inc seen)
               nxt)))))
