(ns proj.p11
  (:require [proj.comp :refer [read-prog extend-prog run pln]]
            [proj.utils :refer [minimax]]
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
  (count (robot {})))

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
    (doseq [y (range minY (inc maxY))]
      (doseq [x (range minX (inc maxX))]
        (print (charmap (or (panelMap [x y])
                            0))))
      (println))))