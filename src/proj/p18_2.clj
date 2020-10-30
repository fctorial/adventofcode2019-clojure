(ns proj.p18_2
  (:require [clojure.string :refer [join split split-lines trim upper-case] :as str]
            [proj.utils :refer :all]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close! timeout]]
            [proj.vis.main :refer [create-window synced-window]]))

(def grid__ (->> (slurp "t.txt")
                 split-lines
                 (mapv #(mapv (fn [k] (get {\# :Wall \. :Empty} k k)) %))))
(def X (count grid__))
(def Y (count (first grid__)))
(def MIDX (/ (dec X) 2))
(def MIDY (/ (dec Y) 2))
(def grid__ (-> grid__
                (assoc-in [(dec MIDX) MIDY] :Wall)
                (assoc-in [(inc MIDX) MIDY] :Wall)
                (assoc-in [MIDX (dec MIDY)] :Wall)
                (assoc-in [MIDX (inc MIDY)] :Wall)))

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

(def bot-locs [[(dec MIDX) (dec MIDY)]
               [(dec MIDX) (inc MIDY)]
               [(inc MIDX) (dec MIDY)]
               [(inc MIDX) (inc MIDY)]])

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
                        (concat (for [x (range X)
                                        y (range Y)
                                        :let [coord [x y]]
                                        :when (and (= (get-in grid_ coord) :Empty)
                                                   (> (count (neighbours_ grid_ coord))
                                                      2))]
                                    coord)
                          all-keys
                          all-gates
                          bot-locs))))

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

(def meta-start {:bots         bot-locs
                 :closed-gates all-gates
                 :keys-left    all-keys
                 :grid         grid})

(defn dissolve [grid node]
  (let [ns (grid node)]
    (reduce
      (fn [res [n _]]
        (reduce
          (fn [res n2]
            (update res n assoc n2 (min (+ (get-in grid [n node])
                                           (get-in grid [node n2]))
                                        (get-in grid [n n2] Integer/MAX_VALUE))))
          (update res n dissoc node)
          (keys (dissoc ns n))))
      grid
      ns)))

(defn fuck [grid loc ks]
  (if (ks loc)
    (as-> grid $
          (dissolve $ loc)
          (dissolve $ (key-index loc)))
    grid))

(def c (atom 0))
(defn meta-neighbours-1 [{bot :bot gs :closed-gates ks :keys-left grid :grid}]
  (for [new-loc (filter
                  #(not (get gs % false))
                  (keys (grid bot)))]
    {:bot          new-loc
     :closed-gates (disj gs (key-index new-loc))
     :keys-left    (disj ks new-loc)
     :grid         (fuck grid new-loc ks)}))

(defn meta-neighbours [{bots :bots gs :closed-gates ks :keys-left grid :grid}]
  (swap! c inc)
  (apply concat (for [[bot idx] (zip-colls bots (range))
                      :let [n-1 (meta-neighbours-1 {:bot          bot
                                                    :closed-gates gs
                                                    :keys-left    ks
                                                    :grid         grid})]]
                  (for [n n-1]
                    {:bots         (assoc bots idx (n :bot))
                     :closed-gates (n :closed-gates)
                     :keys-left    (n :keys-left)
                     :grid         (n :grid)}))))

(defn meta-dist [curr neighbour]
  (apply max (map (fn [[a b]] (get-in (curr :grid) [a b] 0))
                  (zip-colls (curr :bots)
                             (neighbour :bots)))))

(defn p2 []
  (reset! c 0)
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
     :path (map #(select-keys % [:bots ]) path)
     :keys key-order}))
