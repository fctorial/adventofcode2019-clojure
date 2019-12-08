(ns proj.p07
  (:require [clojure.string :refer [split trim-newline]]
            [proj.comp :refer [run read-prog]]
            [clojure.math.combinatorics :refer [cartesian-product permutations]]
            [clojure.core.async :as async :refer [>!! <!! >! <! poll! go go-loop chan close! sliding-buffer]]))

(def prog (read-prog "p07.txt"))

(defn link-amps [phases]
  (let [channels (mapv (fn [_] (chan 5)) (range 6))]
    (mapv (fn [i p]
            (>!! i p)) channels phases)
    (go
      (>! (first channels) 0))
    (mapv
      (fn [[i o] n] (run prog i o n))
      (map vector channels (drop 1 channels))
      (range 5))
    (<!! (last channels))))

(defn p1 [] ()
  (->> (permutations (range 5))
       (map link-amps)
       (reduce max)))

(defn backloop-phases [phases]
  (let [thrust (chan (sliding-buffer 1))
        channels (mapv (fn [_] (chan 5)) (range 6))]
    (go
      (while true
        (let [last (<! (last channels))]
          (>! (first channels) last)
          (>! thrust last))))
    (mapv (fn [i p]
            (>!! i p)) channels phases)
    (go
      (>! (first channels) 0))
    (let [results (mapv
                    (fn [[i o] n] (run prog i o n))
                    (map vector channels (drop 1 channels))
                    (range 5))]
      (<!! (last results))
      (<!! thrust))))

(defn p2 []
  (->> (permutations (range 5 10))
       (map #(backloop-phases %))
       (reduce max)))
