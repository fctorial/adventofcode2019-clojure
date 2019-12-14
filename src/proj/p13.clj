(ns proj.p13
  (:require [proj.comp :refer [run read-prog extend-prog pln]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]))

(def prog (extend-prog (read-prog "p13.txt") 1000))

(defn gpu [input id]
  (go-loop [state {}]
    (let [[x y type] (map (fn [_] (<!! input)) (range 3))]
      (if (and x y type)
        (if (= x -1)
          (do
            (println type)
            (recur (assoc state :score type)))
          (recur (assoc state [x y] type)))
        state))))

(def reprs {0 "  "
            1 "--"
            2 "##"
            3 "=="
            4 "<>"})

(defn print-scr [scr w h]
  (mapv
    (fn [y]
      (mapv
        (fn [x]
          (let [v (reprs (or (scr [x y])
                       0))]
            (if v
              (print v)
              (throw (new Exception)))))
        (range w))
      (println))
    (range h))
  nil)

(defn p1 []
  (let [ip (chan 5)
        op (chan 5)
        cpu (run prog ip op "cpu")
        gpu (gpu op "gpu")]
    (<!! cpu)
    (close! op)
    (<!! gpu)))

; replace the line with paddle (1,0,0...,3,...,0,1) with all 3s before running it
(defn p2 []
  (let [ip (chan 5)
        op (chan 5)
        cpu (run (assoc prog 0 2) ip op "cpu")
        gpu (gpu op "gpu")]
    (go-loop []
      (>! ip 0)
      (recur))
    (<!! cpu)
    (close! op)
    (<!! gpu)
    nil))

