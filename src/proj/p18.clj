(ns proj.p18
  (:require [clojure.string :refer [join split split-lines trim]]
            [proj.utils :refer :all]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [proj.vis.main :refer [create-window synced-window]])
  (:import (java.awt Color)))

(def cmap-field {:Wall  (new Color 100 100 100)
                 :Empty (new Color 250 250 250)
                 :Bot   (new Color 250 0 0)})

(def grid (->> (slurp "p18.txt")
               split-lines
               (map #(map (fn [k] (get {\# :Wall \. :Empty \@ :Bot} k (str k))) %))
               (mapv vec)))

(let [groups (group-by uppercase? (filter string? (flatten grid)))]
  (def KEYS (groups false))
  (def GATES (groups true)))

(def cmap )

(defn map-grid [g]
  (into {}
        (for [x (range (count (first g)))
              y (range (count g))]
          [[x y] (get-in g [x y])])))

(defn display-grid [g]
  (>!! (synced-window (count (first g)) (count g) 8 cmap-field 0 0)
       (map-grid g)))

(defn p1 []
  (let [bot (first (for [y (range (count grid))
                         x (range (count (first grid)))
                         :when (= (get-in grid [x y])
                                  :BOT)]
                     [x y]))
        gates (for [y (range (count grid))
                    x (range (count (first grid)))
                    :when (uppercase? (get-in grid [x y]))]
                [x y])]))
