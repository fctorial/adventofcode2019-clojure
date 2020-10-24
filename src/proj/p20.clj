(ns proj.p20
  (:require [clojure.string :as str :refer [split-lines]]
            [proj.utils :refer [zip-colls iter-n dijkstra filter-out]]))

(def g1 (->> (slurp "t.txt")
             split-lines
             (mapv vec)))
(def X (count g1))
(def Y (count (first g1)))

(def horiz
  (for [x (range X)
        [b c1 c2 a] (iter-n (range -1 (inc Y)) 4)
        :let [ch1 (get-in g1 [x c1])
              ch2 (get-in g1 [x c2])]
        :when (and (Character/isAlphabetic (int ch1))
                   (Character/isAlphabetic (int ch2)))]
    [(keyword (str ch1 ch2))
     {:loc  [x (if (= \. (get-in g1 [x a]))
                 a b)]
      :loff (if (= b -1)
              -1 1)}]))
(def vert
  (for [y (range Y)
        [b r1 r2 a] (iter-n (range -1 (inc X)) 4)
        :let [ch1 (get-in g1 [r1 y])
              ch2 (get-in g1 [r2 y])]
        :when (and (Character/isAlphabetic (int ch1))
                   (Character/isAlphabetic (int ch2)))]
    [(keyword (str ch1 ch2))
     {:loc  [(if (= \. (get-in g1 [a y]))
               a b)
             y]
      :loff (if (= b -1)
              -1 1)}]))

(def all (reduce
           (fn [res [name loc]]
             (if (res name)
               (update res name conj loc)
               (assoc res name [loc])))
           {}
           (concat horiz vert)))

(def start (first (all :AA)))
(def end (first (all :ZZ)))

(def gates (dissoc all :AA :ZZ))

(defn cfind [pred coll]
  (first (filter pred coll)))

(def nodes (set (apply concat (vals all))))

(def rall (->> nodes
               (map
                 (fn [n]
                   [n (first (cfind (fn [[_ locs]]
                                      (= n (cfind #(= % n) locs)))
                                    all))]))
               (into {})))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))


(def g2 (loop [left nodes
               ds {}]
          (if (empty? left)
            ds
            (let [nxt (first left)
                  toph (dijkstra (fn [c]
                                   (filter
                                     #(= (get-in g1 %) \.)
                                     (map #(mapv (partial apply +) (zip-colls c %))
                                          [[1 0] [-1 0] [0 1] [0 -1]])))
                                 (fn [& _] 1)
                                 (:loc nxt))
                  found (filter #(toph (:loc %)) (filter-out nxt nodes))]
              (recur
                (rest left)
                (deep-merge ds
                            {(rall nxt)
                             (into {} (map (fn [n] [(rall n) (inc (first (toph (:loc n))))])
                                           found))}))))))

(defn p1 []
  (dec (first (:ZZ (dijkstra (fn [n]
                               (keys (g2 n)))
                             #(get-in g2 [%1 %2])
                             :AA)))))
