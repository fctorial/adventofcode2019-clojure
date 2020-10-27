(ns proj.p24
  (:require [clojure.string :as str :refer [split-lines]]
            [proj.utils :refer [trace dthrow]]))

(def init (as-> "p24.txt" $
                (slurp $)
                (split-lines $)
                (map #(str/split % #"") $)
                (map #(map (fn [c] (= c "#")) %) $)
                (map #(concat [false] % [false]) $)
                (concat [(repeat 7 false)] $ [(repeat 7 false)])
                (mapv vec $)))

(defn nc [g loc]
  (count (filter identity
                 (map #(get-in g %)
                      (map #(mapv + loc %)
                           [[1 0] [-1 0]
                            [0 1] [0 -1]])))))

(defn iter [b]
  (reduce
    (fn [a r]
      (reduce
        (fn [a c]
          (let [loc [r c]
                count (nc b loc)]
            (if (get-in b loc)
              (assoc-in a loc (= count 1))
              (assoc-in a loc (or (= count 1)
                                  (= count 2))))))
        a
        (range 1 6)))
    b
    (range 1 6)))

(defn rating [g]
  (int (apply +
              (for [r (range 1 6)
                    c (range 1 6)
                    :when (get-in g [r c])]
                (Math/pow 2 (+ (* 5 (dec r))
                               (dec c)))))))

(defn p1 []
  (loop [seen #{(hash init)}
         curr init]
    (let [nxt (iter curr)
          h (hash nxt)]
      (if (contains? seen h)
        (rating nxt)
        (recur (conj seen h)
               nxt)))))
