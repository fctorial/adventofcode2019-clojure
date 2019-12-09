(ns proj.p08
  (:require [clojure.string :refer [split trim-newline join]]))

(def nm {\0 0
         \1 1
         \2 2
         \3 3
         \4 4
         \5 5
         \6 6
         \7 7
         \8 8
         \9 9})

(def layers (->> (slurp "p08.txt")
                 (map nm)
                 (filter identity)
                 (partition (* 25 6))))

(defn count-of [pred coll]
  (count (filter pred coll)))

(defn p1 []
  (let [l (->> layers
               (map (fn [l]
                      [l (count-of zero? l)]))
               (reduce #(min-key second %1 %2))
               first)]
    (* (count-of #(= % 1) l)
       (count-of #(= % 2) l))))

(defn p2 []
  (let [final (partition (count layers)
                         (apply interleave layers))
        final-layered (->> final
                           (map (fn [p]
                                  (or (first (drop-while #(= % 2) p))
                                      2)))
                           (map {0 "  " 1 "##"}))
        printed (->> final-layered
                     (partition 25)
                     (map #(apply str %))
                     (join "\n"))]
    printed))