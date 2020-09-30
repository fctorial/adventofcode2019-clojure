(ns proj.p18
  (:require [clojure.string :refer [join split split-lines trim upper-case] :as str]
            [proj.utils :refer :all]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close! timeout]]
            [proj.vis.main :refer [create-window synced-window]])
  (:import (java.awt Color)))

(def grid__ (->> (slurp "p18.txt")
                 split-lines
                 (mapv #(mapv (fn [k] (get {\# :Wall \. :Empty} k k)) %))))
(def X (count grid__))
(def Y (count (first grid__)))

(defn coordinates-of [grid c]
  (first (for [x (range (count grid))
               y (range (count (grid x)))
               :when (= (get-in grid [x y])
                        c)]
           [x y])))

(def key-index (let [keymap (reduce
                              (fn [res idx]
                                (if (lower-char? (get-in grid__ idx))
                                  (assoc res idx (get-in grid__ idx))
                                  res))
                              {}
                              (for [x (range X)
                                    y (range Y)]
                                [x y]))
                     valmap (reduce
                              (fn [res idx]
                                (if (upper-char? (get-in grid__ idx))
                                  (assoc res (get-in grid__ idx) idx)
                                  res))
                              {}
                              (for [x (range X)
                                    y (range Y)]
                                [x y]))]
                 (map-values keymap (comp valmap first upper-case))))
(def all-keys (set (keys key-index)))
(def all-gates (set (vals key-index)))

(def bot-loc (coordinates-of grid__ \@))

(def grid_ (mapv
             #(mapv
                (fn [e]
                  (cond
                    (keyword? e) e
                    (= e \@) :Empty
                    (lower-char? e) :Wall
                    (upper-char? e) :Wall
                    true (println (type e) e)))
                %)
             grid__))

(defn neighbours_ [grid loc]
  (filter
    #(= (get-in grid %)
        :Empty)
    (map
      #(mapv + loc %)
      [[1 0] [-1 0]
       [0 1] [0 -1]])))

(defn dijkstra [neighbour-fn dist-fn src]
  (loop [border (priority-map-keyfn first src [0 nil])
         visited {}]
    (if (empty? border)
      visited
      (let [[curr [curr-dist parent]] (peek border)
            border (pop border)
            neighbours (neighbour-fn curr)]
        (recur
          (reduce (fn [border neighbour]
                    (let [new-dist (+ curr-dist (dist-fn curr neighbour))]
                      (if (or (not (contains? visited neighbour))
                              (< new-dist (first (visited neighbour))))
                        (assoc border neighbour [new-dist curr])
                        border))) border neighbours)
          (assoc visited curr [curr-dist parent]))))))

(def nodes (set (filter identity
                        (concat (for [x (range X)
                                      y (range Y)
                                      :let [coord [x y]]
                                      :when (and (= (get-in grid_ coord) :Empty)
                                                 (> (count (neighbours_ grid_ coord))
                                                    2))]
                                  coord)
                                all-keys
                                all-gates
                                [bot-loc]))))

(defn neighbours_2 [grid nodes loc]
  (if (nodes loc)
    []
    (filter
      #(or (= (get-in grid %)
              :Empty)
           (nodes %))
      (map
        #(mapv + loc %)
        [[1 0] [-1 0]
         [0 1] [0 -1]]))))

(def grid (->> nodes
               (map (fn [node]
                      (let [toph (dijkstra #(neighbours_2 grid_ (disj nodes node) %) ; <<<<<<<<<<<<<
                                           (fn [& _] 1)
                                           node)]
                        [node (->> nodes
                                   (filter-out node)
                                   (filter toph)
                                   (map (fn [n] [n (first (toph n))]))
                                   (into {}))])))
               (into {})))

(defn minimum-chan [c]
  (go
    (loop [curr Long/MAX_VALUE]
      (let [[nxt] (<! c)]
        (if (nil? nxt)
          curr
          (let [new_min (min nxt curr)]
            (if (= new_min nxt)
              (println "Min: " new_min))
            (recur new_min)))))))

(defn f [grid key-index closed-gates loc travelled
         logger sub-counter fin-counter]
  (swap! sub-counter inc)
  (go
    (if (empty? key-index)
      (>! logger [travelled])
      (let [dists (dijkstra (fn [loc]
                              (filter #(not (closed-gates %))
                                      (keys (grid loc))))
                            (fn [curr neighbour] (get-in grid [curr neighbour]))
                            loc)
            reachable (for [k (keys key-index)
                            :when (dists k)]
                        k)]
        (doseq [target reachable]
          (let [gate (key-index target)
                [dist _] (dists target)
                travelled_new (+ travelled dist)]
            (f grid
               (dissoc key-index target)
               (disj closed-gates gate)
               target
               travelled_new

               logger
               sub-counter
               fin-counter)))))
    (swap! fin-counter inc)
    (if (= @sub-counter
           @fin-counter)
      (close! logger))))

(defn p1 []
  (let [logger (chan 1024)
        sub-count (atom 0)
        finish-count (atom 0)]
    (go
      (loop []
        (println @finish-count @sub-count)
        (Thread/sleep 1000)
        (recur)))
    (f grid key-index (set (vals key-index)) bot-loc 0
       logger sub-count finish-count)
    (<!! (minimum-chan logger))))