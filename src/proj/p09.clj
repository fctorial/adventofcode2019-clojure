(ns proj.p09
  (:require [proj.comp :refer [run read-prog extend-prog]]
            [clojure.core.async :refer [>! >!! <! <!! chan go go-loop]]))

(def prog (-> "p09.txt"
              read-prog
              (extend-prog 1000)))

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
