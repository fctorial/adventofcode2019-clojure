(ns proj.p16
  (:require [clojure.string :refer [split split-lines trim join]]
            [proj.utils :refer :all]))

(defn parse-ip [s]
  (->> (split s #"")
       (map read-string)))

(def ip (parse-ip (slurp "p16.txt")))

(def base-pattern [0 1 0 -1])

(def get-pattern (memoize (fn [idx]
                            (drop 1 (flatten (repeat
                                               (flatten (apply zip-colls (repeat idx base-pattern)))))))))

(defn apply-phase [ip]
  (map
    (fn [ei idx]
      (let [pattern (get-pattern (inc idx))
            oi (reduce + (map *
                              ip pattern))]
        (mod (Math/abs oi)
             10)))
    ip (range (count ip))))

(defn p1 []
  (->> (nth (iterate apply-phase ip) 100)
       (take 8)
       (join "")))

(def m-lcm (memoize lcm))

(defn p2 [ip]
  (nth (iterate apply-phase (flatten (repeat 10000 ip))) 1))

(defn phase-2-back
  [xs]
  (reductions
    (fn [a b] (mod (+ a b) 10))
    xs))

(defn part2
  [xs]
  (let [offset (Integer. (apply str (take 7 xs)))
        whole (apply concat (repeat 10000 xs))
        n (apply str (take 8 (reverse (nth (iterate phase-2-back (reverse (drop offset whole))) 100))))]
    n))
