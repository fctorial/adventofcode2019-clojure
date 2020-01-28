(ns proj.p17
  (:require [clojure.string :refer [join split split-lines trim]]
            [proj.utils :refer :all]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [proj.vis.main :refer [create-window synced-window]])
  (:import (java.awt Color)))

(def prog (extend-prog (read-prog "p17.txt") 10000))

(defn split-coll [val coll]
  (let [[fst rst] (split-with #(not= val %) coll)]
    (lazy-seq (cons fst (let [rst (drop 1 rst)]
                          (if (empty? rst)
                            nil
                            (split-coll val rst)))))))

(defn get-feed []
  (let [ip (closed-chan)
        op (chan 5)
        cam (run prog ip op "cam")]
    (go
      (<! cam)
      (close! op))
    (let [feed (chan->seq op)
          grid (->> feed
                    (map char)
                    (split-coll \newline)
                    (filter not-empty)
                    (mapv vec)
                    vec)]
      grid)))

(def dirs {\^ [-1 0]
           \v [1 0]
           \< [0 -1]
           \> [0 1]})

(defn is-intersection [grid pos]
  (every? #(= \# %) (map
                      #(let [pos (mapv + pos %)]
                         (get-in grid pos))
                      (conj (vals dirs) [0 0]))))

(defn alignment-param [pos]
  (apply * pos))

(defn p1 []
  (let [grid (get-feed)
        intersections (filter #(is-intersection grid %)
                              (for [x (range (count (first grid)))
                                    y (range (count grid))]
                                [x y]))]
    (reduce + (map alignment-param intersections))))

(def color-map {\# (new Color 0 0 100)
                \. (new Color 100 100 100)
                ;\> (new Color 100 0 0)
                ;\< (new Color 100 0 0)
                \^ (new Color 100 0 0)
                ;\v (new Color 100 0 0)
                })

(defn display-grid [grid]
  (let [X (count (first grid))
        Y (count grid)
        vis (create-window X Y 12)]
    (go
      (doseq [[line y] (zip-colls grid (range))]
        (doseq [[elem x] (zip-colls line (range))]
          (>! vis [x y (color-map elem)]))))
    nil))

(defn pad-feed [feed]
  (let [X (count (first feed))
        Y (count feed)]
    (vec (concat [(vec (repeat (+ X 2) \.))]
                 (map
                   #(vec (concat [\.] % [\.]))
                   feed)
                 [(vec (repeat (+ X 2) \.))]))))

(defn get-bot [grid]
  (let [X (count (first grid))
        Y (count grid)]
    (first
      (for [x (range X)
            y (range Y)
            :let [dir (dirs (get-in grid [x y]))]
            :when dir]
        {:dir dir
         :pos [x y]}))))

(defn extract-segs [grid]
  (loop [bot (get-bot grid)
         moves []]
    (let [dir (bot :dir)
          pos (bot :pos)
          frnt (mapv + pos dir)]
      (if (= \. (get-in grid frnt))
        (let [[dx dy] dir
              left [(* dy -1) dx]
              right [dy (* dx -1)]]
          (cond
            (= \# (get-in grid (mapv + pos left))) (recur (assoc bot :dir left)
                                                          (conj moves \L))
            (= \# (get-in grid (mapv + pos right))) (recur (assoc bot :dir right)
                                                           (conj moves \R))
            :else moves))
        (let [[steps nxt_pos] (loop [curr_pos pos
                                     steps 0]
                                (let [frnt (mapv + curr_pos dir)
                                      nxt (get-in grid frnt)]
                                  (if (= \. nxt)
                                    [steps curr_pos]
                                    (recur frnt (inc steps)))))]
          (recur {:dir dir
                  :pos nxt_pos}
                 (conj moves steps)))))))

(def grid (pad-feed (get-feed)))

(defn count-of [seq sub]
  (count (filter
           #(= sub (subvec seq % (+ % (count sub))))
           (range (count seq)))))

(defn routine-size [r]
  (count (join "," r)))

(defn index-of [pred coll] (first (keep-indexed #(if (pred %2) %1) coll)))

(defn index-of-seq [a b]
  (index-of identity (map #{a} (partition (count a) 1 b))))

(defn split-coll-seq [seq coll]
  (let [idx (index-of-seq seq coll)]
    (if idx
      (concat [(subvec coll 0 idx)]
              (split-coll-seq seq (subvec coll (+ idx (count seq)))))
      [coll])))

(defn expand-routine [r]
  (flatten (map
             #(if (int? %)
                (repeat % 1)
                %)
             r)))

(defn rev-map [m]
  (into {}
        (map (fn [[k v]]
               [v k])
             m)))

(defn f [[main subs]]
  (let [subseqs (for [s (range (dec (count main)))
                      e (range (inc s) (count main))
                      :let [res (subvec main s e)]
                      :when (< (routine-size (apply concat (map subs res)))
                               20)]
                  res)
        new-mains (map
                    (fn [ss] (reduce (fn [c1 c2] (concat c1 [:N] c2))
                                     (split-coll-seq ss main)))
                    subseqs)
        [ss mn]  (reduce (fn [[ss1 mn1]
                               [ss2 mn2]]
                            (if (< (count mn1)
                                   (count mn2))
                              [ss1 mn1]
                              [ss2 mn2]))
                          (zip-colls subseqs new-mains))]
    (if (empty? new-mains)
      (throw (new Error "lol, what"))
      (let [changed-routine (first (filter
                                     #(= % (first ss))
                                     (keys subs)))]
        [(mapv #(if (= % :N) changed-routine
                             %)
               mn)
         (assoc subs changed-routine (vec (apply concat (map subs ss))))]))))

(def init-map {:A [\L]
               :B [\R]
               :C [1]})

(def init-main (mapv {\L :A \R :B 1 :C} (expand-routine (extract-segs grid))))

(defn p2 []
  ())

