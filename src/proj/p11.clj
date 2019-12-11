(ns proj.p11
  (:require [proj.comp :refer [read-prog extend-prog run pln]]
            [clojure.core.async :refer [<! >! <!! >!! go-loop go close! chan merge]]))

(def prog (extend-prog (read-prog "p11.txt") 1000))

(def fixdir {0 -1
             1 1})

(def dirmap {0 [0 -1]
             1 [1 0]
             2 [0 1]
             3 [-1 0]})

(def charmap {0 "  "
              1 "##"})

(defn robot [panel]
  (let [ip (chan 5)
        op (chan 5)
        runner (run prog ip op 0)]
    (go
      (<! runner)
      (close! op))
    (loop [painted panel
           loc [0 0]
           dir 0]
      (let [curr_c (or (painted loc)
                       0)
            c (and (>!! ip curr_c)
                   (<!! op))
            d (fixdir (<!! op))]
        (if (and c d)
          (let [newdir (mod (+ dir d)
                            4)]
            (recur (assoc painted loc c)
                   (mapv + loc (dirmap newdir))
                   newdir))
          painted)))))

(defn p1 []
  (robot {}))

(defn rand-range [n]
  (map
    (fn [_] (rand-int 100))
    (range n)))

(defn minimax [nums]
  (reduce
    (fn [[mx mn] num]
      [(max mx num) (min mn num)])
    [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
    nums))

(defn p2 []
  (let [panelMap (robot {[0 0] 1})
        [maxX minX] (->> panelMap
                         keys
                         (map first)
                         minimax)
        [maxY minY] (->> panelMap
                         keys
                         (map second)
                         minimax)]
    (mapv
      (fn [y]
        (mapv
          (fn [x]
            (print (charmap (or (panelMap [x y])
                                0))))
          (range minX (inc maxX)))
        (println))
      (range minY (inc maxY)))
    nil))