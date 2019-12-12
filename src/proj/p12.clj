(ns proj.p12
  (:require [clojure.string :refer [split split-lines trim-newline]]
            [clojure.math.combinatorics :as combs :refer [permutations combinations]]
            [proj.utils :refer [lcm]]))

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

(defn linear-period [vels poss]
  (loop [t 0
         vels vels
         poss poss
         states (transient #{[vels poss]})]
    (let [new-vels (map
                     (fn [vc pc]
                       (reduce
                         (fn [v po]
                           (cond
                             (< pc po) (inc v)
                             (> pc po) (dec v)
                             :else v))
                         vc poss))
                     vels poss)
          new-poss (map
                     +
                     poss new-vels)
          new-state [new-vels new-poss]]
      (if (contains? states new-state)
        (inc t)
        (recur (inc t)
               new-vels
               new-poss
               (conj! states new-state))))))

(defn p2 []
  (apply lcm (for [i (range 3)]
     (let [vis [0 0 0 0]
           pis (map
                 (fn [p] (p i))
                 ip)]
       (linear-period vis pis)))))
