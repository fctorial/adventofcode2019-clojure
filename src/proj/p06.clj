(ns proj.p06
  (:require [clojure.string :refer [split split-lines]])
  (:require [clojure.zip :as zip]))

(def ip (->> (slurp "p06.txt")
             split-lines
             (filter not-empty)
             (map #(split % #"\)"))))

(def all (set (flatten ip)))

(def parent (->> ip
                 (map (fn [[p c]] [c p]))
                 (into {})))

(defn seq-branch [e]
  (loop [c e
         res (list c)]
    (let [p (parent c)]
      (if p
        (recur p (cons p res))
        res))))

(def p1 (->> all
             (map seq-branch)
             (map count)
             (map dec)
             (reduce +)))

(def p2 (let [b_m (seq-branch (parent "YOU"))
              b_s (seq-branch (parent "SAN"))
              common_len (loop [res 0
                                m b_m
                                s b_s]
                           (if (= (first m)
                                  (first s))
                             (recur (inc res)
                                    (rest m)
                                    (rest s))
                             res))]
          (+ (- (count b_m)
                common_len)
             (- (count b_s)
                common_len))))
