(ns proj.p09
  (:require [proj.comp :refer [run read-prog]]
            [clojure.core.async :refer [>! >!! <! <!! chan go go-loop]]))

(def big0 (bigint 0))

(defn extend-prog [prog by]
  (reduce
    (fn [p _] (conj p big0))
    prog
    (range by)))

(def prog (-> "p09.txt"
              read-prog
              (extend-prog 1000)))

(defn printing-chan [buf]
  (let [ch (chan buf)]
    (go-loop []
      (println (<! ch))
      (recur))
    ch))

(defn p1 []
  (let [ip (chan 1)
        op (chan 10)]
    (>!! ip 1)
    (run prog ip op 1)
    (<!! op)))

(defn p2 []
  (let [ip (chan 1)
        op (chan 10)]
    (>!! ip 2)
    (run prog ip op 1)
    (<!! op)))
