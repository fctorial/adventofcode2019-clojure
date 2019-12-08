(ns proj.p05
  (:require [clojure.string :refer [split split-lines]]
            [proj.comp :refer [run read-prog]]
            [clojure.core.async :refer [<! >! chan go-loop go]]))

(def prog (read-prog "p05.txt"))

(defn diagnose [sys]
  (let [ip (chan 1)
        op (chan 1)]
    (go (>! ip sys))
    (run prog ip op "")
    (go-loop []
      (println (<! op))
      (recur))))

(defn p1 []
  (diagnose 1))

(defn p2 []
  (diagnose 5))