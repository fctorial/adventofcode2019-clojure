(ns proj.p15
  (:require [clojure.string :refer [split split-lines trim]]
            [proj.comp :refer [run read-prog extend-prog pln]]
            [proj.utils :refer [minimax]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]))

(def prog (extend-prog (read-prog "p15.txt") 1000))

(def dirmap {1 [0 -1]
             2 [0 1]
             3 [-1 0]
             4 [1 0]})
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

(defn p1 []
  (let [ip (chan 10)
        op (chan 10)
        droid (run prog ip op "droid")]
    (loop [pos [0 0]
           grid {[0 0] :bot}]
      (print-grid grid)
      (println pos)
      (let [cmd (get-ip)]
        (>!! ip cmd)
        (let [nxt_pos (mapv + pos (dirmap cmd))
              o (<!! op)]
          (case o
            0 (recur pos (assoc grid nxt_pos :wall))
            1 (recur nxt_pos (assoc grid pos :empty nxt_pos :bot))
            2 (recur nxt_pos (assoc grid pos :empty nxt_pos :water))))))))