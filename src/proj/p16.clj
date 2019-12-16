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
       (join #"")))

(def m-lcm (memoize lcm))

(def mult-table (mapv
                  (fn [i]
                    [(* -1 i) 0 i])
                  (range 10)))
(def base-pattern-shifted (mapv inc base-pattern))

(defn apply-phase-full [ip]
  (let [n (count ip)
        rep (mapv
              (range n))]
    ))