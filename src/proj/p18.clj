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
(def all-gates (set (filter identity (vals key-index))))

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

(def nodes (set (filter identity
                        (concat #_(for [x (range X)
                                        y (range Y)
                                        :let [coord [x y]]
                                        :when (and (= (get-in grid_ coord) :Empty)
                                                   (> (count (neighbours_ grid_ coord))
                                                      2))]
                                    coord)
                          all-keys
                          all-gates
                          [bot-loc]))))

(defn neighbours_2 [nodes loc]
  (if (nodes loc)
    []
    (filter
      #(or (= (get-in grid_ %)
              :Empty)
           (nodes %))
      (map
        #(mapv + loc %)
        [[1 0] [-1 0]
         [0 1] [0 -1]]))))

(def grid (->> nodes
               (map (fn [node]
                      (let [toph (dijkstra #(neighbours_2 (disj nodes node) %) ; <<<<<<<<<<<<<
                                           (fn [& _] 1)
                                           node)]
                        [node (->> nodes
                                   (filter-out node)
                                   (filter toph)
                                   (map (fn [n] [n (first (toph n))]))
                                   (into {}))])))
               (into {})))

(def meta-start {:bot          bot-loc
                 :closed-gates all-gates
                 :keys-left    all-keys})

(defn meta-neighbours [{bot :bot gs :closed-gates ks :keys-left}]
  (for [new-loc (filter
                  #(not (get gs % false))
                  (keys (grid bot)))]
    {:bot          new-loc
     :closed-gates (disj gs (key-index new-loc))
     :keys-left    (disj ks new-loc)}))

(defn meta-dist [curr neighbour]
  (get-in grid [(curr :bot) (neighbour :bot)]))

(defn p1 [meta-start]
  (let [toph (dijkstra meta-neighbours
                       meta-dist
                       meta-start)
        [last [dist _]] (->> toph
                             (filter #(empty? (:keys-left (first %))))
                             (apply min-key #(first (second %))))
        path (loop [coll `(~last)]
               (let [nxt (second (toph (first coll)))]
                 (if nxt
                   (recur (cons nxt coll))
                   coll)))
        key-order (loop [order []
                         keys-left all-keys
                         locs (map :bot path)]
                    (if (empty? locs)
                      order
                      (let [nxt-loc (first locs)]
                        (recur (if (keys-left nxt-loc)
                                 (conj order nxt-loc)
                                 order)
                               (disj keys-left nxt-loc)
                               (rest locs)))))]
    {:dist dist
     :path path
     :keys key-order}))