(ns proj.p05
  (:require [clojure.string :refer [split split-lines]]
            [proj.comp :refer [run read-prog]]
            [clojure.core.async :refer [<! >! chan go-loop go]]))

(def prog (read-prog "p05.txt"))

(defn diagnose [sys]
  (let [ip (chan 1)]
    (go (>! ip sys))
    (let [op (run prog ip "")]
     (go-loop []
       (let [o (<! op)]
         (if o
           (do
             (println o)
             (recur))))))))

(defn p1 []
  (diagnose 1))

(defn p2 []
  (diagnose 5))