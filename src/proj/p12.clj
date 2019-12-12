(ns proj.p12
  (:require [clojure.string :refer [split split-lines trim-newline]]
            [clojure.math.combinatorics :as combs :refer [permutations combinations]]))

(def ip (->> "p12.txt"
             slurp
             split-lines
             (map #(re-matcher #"-?\d+" %))
             (mapv (fn [mat]
                     (vec (for [_ (range 3)]
                            (read-string (re-find mat))))))))

(defn abs [a]
  (Math/abs a))

(defn potential [p]
  (->> p
       (map abs)
       (reduce +)))

(defn kinetic [v]
  (->> v
       (map abs)
       (reduce +)))

(defn calc-energy [ps vs]
  (reduce +
          (map
            #(* (potential %1)
                (kinetic %2))
            ps vs)))

(def ovels (mapv
             (fn [_] (mapv (fn [_] 0) (range 3)))
             (range 4)))

(defn p1 []
  (loop [t 0
         vels ovels
         poss ip]
    (if (= t 1000)
      (calc-energy poss vels)
      (let [new-vels (mapv
                       (fn [i]
                         (let [curr (poss i)]
                           (reduce
                             (fn [v po]
                               (mapv
                                 (fn [vi po pi]
                                   (cond
                                     (< pi po) (inc vi)
                                     (> pi po) (dec vi)
                                     :else vi))
                                 v po curr))
                             (vels i) poss)))
                       (range 4))
            new-poss (mapv
                       (fn [p v]
                         (mapv + p v))
                       poss new-vels)]
        (recur (inc t)
               new-vels
               new-poss)))))

(defn p2 []
  (loop [t 0
         vels ovels
         poss ip
         states (transient #{[vels poss]})]
    (if (zero? (mod t 10000))
      (println t))
    (let [new-vels (mapv
                     (fn [i]
                       (let [curr (poss i)]
                         (reduce
                           (fn [v po]
                             (mapv
                               (fn [vi po pi]
                                 (cond
                                   (< pi po) (inc vi)
                                   (> pi po) (dec vi)
                                   :else vi))
                               v po curr))
                           (vels i) poss)))
                     (range 4))
          new-poss (mapv
                     (fn [p v]
                       (mapv + p v))
                     poss new-vels)
          new-state [new-vels new-poss]]
      (if (contains? states new-state)
        (inc t)
        (recur (inc t)
              new-vels
              new-poss
              (conj! states new-state))))))