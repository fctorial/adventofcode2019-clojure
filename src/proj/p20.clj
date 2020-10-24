(ns proj.p20
  (:require [clojure.string :as str :refer [split-lines]]
            [clojure.data.priority-map :refer [priority-map-by priority-map-keyfn]]
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
      :loff (if (or (= b -1)
                    (= a Y))
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
      :loff (if (or (= b -1)
                    (= a X))
              -1 1)}]))

(def all (reduce
           (fn [res [name loc]]
             (if (res name)
               (update res name conj loc)
               (assoc res name [loc])))
           {}
           (concat horiz vert)))

(def nodes (map :loc (set (apply concat (vals all)))))
(def loffs (->> (set (apply concat (vals all)))
                (map (fn [{loc :loc :as n}]
                       [loc (n :loff)]))
                (into {})))

(def start (:loc (first (all :AA))))
(def end (:loc (first (all :ZZ))))

(def portals (->> (dissoc all :AA :ZZ)
                  (map (fn [[_ [{loc1 :loc} {loc2 :loc}]]]
                         [[loc1 loc2]
                          [loc2 loc1]]))
                  (apply concat)
                  (into {})))

(defn cfind [pred coll]
  (first (filter pred coll)))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))


(def dists (loop [left nodes
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
                                    nxt)
                     found (filter #(toph %) (filter-out nxt nodes))]
                 (recur
                   (rest left)
                   (deep-merge ds
                               {nxt
                                (dissoc (into {(portals nxt) 1} (map (fn [n] [n (first (toph n))])
                                                                     found))
                                        nil)}))))))

(defn p1 []
  (first ((dijkstra (fn [n]
                      (keys (dists n)))
                    #(get-in dists [%1 %2])
                    start)
          end)))

(defn A*
  [neighbour-fn dist-fn h start goal]
  (loop [visited {}
         queue (priority-map-keyfn #(* 1 (first %)) start [0 0 nil])]
    (when (not (empty? queue))
      (let [[current [_ current-score previous]] (peek queue)
            _ (println current)
            visited (assoc visited current [current-score previous])]
        (if (= current goal)
          visited
          (recur visited (reduce (fn [queue node]
                                   (let [score (+ current-score (dist-fn current node))]
                                     (if (and (not (contains? visited node))
                                              (or (not (contains? queue node))
                                                  (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node)) score current])
                                       queue)))
                                 (pop queue)
                                 (neighbour-fn current))))))))

(defn p2 []
  (A* (fn [{loc :loc layer :layer}]
        (->> (keys (dists loc))
             (map (fn [l] {:loc   l
                           :layer (+ layer (loffs l))}))
             (filter (fn [{layer :layer}] (> layer 0)))))
      (fn [{loc1 :loc} {loc2 :loc}]
        (get-in dists [loc1 loc2]))
      (fn [{layer :layer}] layer)
      {:loc   start
       :layer 1}
      {:loc   end
       :layer 1}))

