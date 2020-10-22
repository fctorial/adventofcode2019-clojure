(ns proj.p19
  (:require [clojure.string :refer [join split split-lines trim]]
            [proj.utils :refer :all]
            [proj.comp :refer [run read-prog extend-prog pln lint]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [proj.vis.main :refer [create-window synced-window]]))

(def prog (extend-prog (read-prog "p19.txt") 10000))

(defn runsim [x y]
  (go
    (let [ip (chan 5)
          op (chan 5)
          d (run prog ip op (str [x y]))]
      (go
        (<! d)
        (close! op))
      (>! ip x)
      (>! ip y)
      (= (<! op) 1))))

(defn pulledDrones []
  (filter identity
          (map (fn [[sp c]]
                 (if (<!! sp)
                   c))
               (for [x (range 50)
                     y (range 50)
                     :let [statusp (runsim x y)]]
                 [statusp [x y]]))))

(defn p1 []
  (count (pulledDrones)))