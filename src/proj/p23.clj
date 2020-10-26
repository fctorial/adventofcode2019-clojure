(ns proj.p23
  (:require [proj.comp :refer [run read-prog extend-prog]]
            [proj.utils :refer [zip-colls printing-chan trace]]
            [clojure.core.async :refer [>! <! go chan close! poll! <!! >!! alts! timeout]]
            [clojure.string :as str]))

(def prog (-> "p23.txt"
              read-prog
              (extend-prog 10000)))

(defn p1 []
  (let [ips (mapv (fn [_] (chan 10240)) (range 50))
        _ (doseq [[c addr] (zip-colls ips (range))]
            (>!! c addr))
        ops (mapv (fn [_] (chan 10240)) (range 50))
        nics (mapv #(run prog (ips %) (ops %) (if (zero? %)
                                                0)) (range 50))
        router (go
                 (loop []
                   (let [[trg-addr src-port] (alts! ops)
                         X (<! src-port)
                         Y (<! src-port)]
                     (if (= trg-addr 255)
                       [X Y]
                       (let [trg-port (ips trg-addr)]
                         (locking trg-port
                           (>! trg-port X)
                           (>! trg-port Y))
                         (recur))))))]
    (doseq [ip ips]
      (go
        (loop []
          (>! ip -1)
          (<! (timeout 1000)))))
    (<!! router)))
