(ns proj.p15
  (:require [clojure.string :refer [split split-lines trim]]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.data.priority-map :refer [priority-map-by priority-map-keyfn]]
            [clojure.set :refer :all]
            [proj.utils :refer [minimax]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]))

(def prog (extend-prog (read-prog "p15.txt") 1000))

(def move->dir {1 [0 -1]
                2 [0 1]
                3 [-1 0]
                4 [1 0]})
(def dir->move (into {}
                     (map
                       (fn [[k v]] [v k])
                       move->dir)))
(def dirs (vals move->dir))
(def cmdmap {"w" 1
             "s" 2
             "a" 3
             "d" 4})

(def pixmap {:wall  "##"
             :empty "  "
             :water "()"
             :bot   "<>"})

(defn print-grid [g]
  (let [[maxX minX] (minimax (map first (keys g)))
        [maxY minY] (minimax (map second (keys g)))]
    (println "============")
    (doseq [y (range minY (inc maxY))]
      (doseq [x (range minX (inc maxX))]
        (print (pixmap (or (g [x y])
                           :empty))))
      (println))
    (println "============")))

(defn get-ip []
  (loop []
    (let [v (cmdmap (read-line))]
      (println v)
      (if v
        v
        (recur)))))

(defn dijkstra [graph dist src]
    (loop [border (priority-map-keyfn first src [0 nil])
           visited {}]
      (if (empty? border)
        visited
        (let [[curr [curr-dist parent]] (peek border)
              border (pop border)
              neighbours (graph curr)]
          (recur
            (reduce (fn [border neighbour]
                      (let [new-dist (+ curr-dist (dist curr neighbour))]
                        (if (or (not (contains? visited neighbour))
                                (< new-dist (first (visited neighbour))))
                          (assoc border neighbour [new-dist curr])
                          border))) border neighbours)
            (assoc visited curr [curr-dist parent]))))))

(defn A*
  [graph dist h start goal]
  (loop [visited {}
         queue (priority-map-keyfn #(* 1 (first %)) start [0 0 nil])]
    (when (not (empty? queue))
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (= current goal)
          visited
          (recur visited (reduce (fn [queue node]
                                   (let [score (+ current-score (dist current node))]
                                     (if (and (not (contains? visited node))
                                              (or (not (contains? queue node))
                                                  (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node goal)) score current])
                                       queue)))
                                 (pop queue)
                                 (graph current))))))))
(defn grid-graph [g]
  (fn [p]
    (->> dirs
         (map #(mapv + p %))
         (filter #(= :empty (g %))))))

(defn parse-path [pth trg]
  (loop [curr trg
         res []]
    (let [p (pth curr)]
      (if (nil? p)
        (reverse res)
        (let [del (mapv - curr p)]
          (recur p
                 (conj res (dir->move del))))))))

(def manhatten proj.utils/manhatten)

(defn min-key-coll [f coll]
  (reduce
    (fn [a b]
      (min-key f a b))
    coll))

(defn p1 []
  (let [ip (chan 10)
        op (chan 10)
        droid (run prog ip op "droid")]
    (loop [pos [0 0]
           grid {pos :empty}
           border (reduce conj
                          clojure.lang.PersistentQueue/EMPTY dirs)]
      (if (empty? border)
        grid
        (let [nxt (first border)
              prevs (A* (grid-graph (assoc grid nxt :empty))
                        manhatten manhatten
                        pos nxt)
              _ (when (nil? prevs)
                  (print-grid (assoc grid nxt :empty))
                  (println grid)
                  (println pos nxt)
                  (read-line)
                  (read-line))
              pth (parse-path prevs nxt)
              final (last pth)]
          (doseq [m (drop-last pth)]
            (>!! ip m)
            (<!! op))
          (>!! ip final)
          (let [pos (prevs nxt)
                nxt_border (reduce conj
                                   (rest border)
                                   (->> dirs
                                        (map #(mapv + nxt %))
                                        (filter #(not (contains? grid %)))
                                        set))
                o (<!! op)]
            (case o
              0 (recur pos (assoc grid nxt :wall)
                       (rest border))
              1 (recur nxt (assoc grid nxt :empty)
                       nxt_border)
              2 (recur nxt (assoc grid pos :empty nxt :water)
                       nxt_border))))))))

(defn p2 []
  (let [grid (p1)
        res (dijkstra (grid-graph grid)
                      manhatten [16 16])]
    (->> (vals res)
         (map first)
         (reduce max))))