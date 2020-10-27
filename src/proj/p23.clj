(ns proj.p23
  (:require [proj.comp :refer [run read-prog extend-prog]]
            [proj.utils :refer [zip-colls printing-chan trace]]
            [clojure.core.async :refer [>! <! go chan close! poll! <!! >!! alts! timeout sliding-buffer]]
            [clojure.string :as str]))

(def prog (-> "p23.txt"
              read-prog
              (extend-prog 10000)))

(defn p1 []
  (let [ips (mapv (fn [_] (chan 10240)) (range 50))
        _ (doseq [[c addr] (zip-colls ips (range))]
            (>!! c addr))
        ops (mapv (fn [_] (chan 10240)) (range 50))
        nics (mapv #(run prog (ips %) (ops %) nil) (range 50))
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
          (locking ip
            (>! ip -1))
          (<! (timeout 1000)))))
    (second (<!! router))))

(defmacro mlocking [o & ss]
  `(do
     ~@ss))

(defn p2 []
  (let [idle (mapv (fn [_] (atom 0)) (range 50))
        rips (mapv (fn [_] (chan 10240)) (range 50))
        _ (doseq [[c addr] (zip-colls rips (range))]
            (>!! c addr))
        ips (mapv (fn [[c idleC]]
                    (let [fe (chan 10240)]
                      (go
                        (loop []
                          (let [X (<! fe)]
                            (>! c X)
                            (if (= X -1)
                              (swap! idleC inc)
                              (do
                                (>! c (<! fe))
                                (reset! idleC 0)))
                            (recur))))
                      fe))
                  (zip-colls rips idle))
        first (rips 0)
        natVal (atom [nil nil])
        nat (go
              (loop [lastY nil]
                (<! (timeout 1000))
                (let [activeC (->> idle
                                   (map deref)
                                   (filter #(< % 2))
                                   count)]
                  (if (zero? activeC)
                    (let [[X Y] @natVal]
                      (if (= Y lastY)
                        [X Y]
                        (do
                          (mlocking first
                                    (>! first X)
                                    (>! first Y))
                          (recur Y))))
                    (recur lastY)))))
        ops (mapv (fn [_] (chan 10240)) (range 50))
        nics (mapv (fn [[ip op id]] (run prog ip op (if (zero? id) id))) (zip-colls rips ops))
        router (go
                 (loop []
                   (let [[trg-addr src-port] (alts! ops)
                         X (<! src-port)
                         Y (<! src-port)]
                     (if (= trg-addr 255)
                       (reset! natVal [X Y])
                       (let [trg-port (ips trg-addr)]
                         (mlocking trg-port
                                   (>! trg-port X)
                                   (>! trg-port Y))))
                     (recur))))]
    (doseq [ip ips]
      (go
        (loop []
          (mlocking ip
                    (>! ip -1))
          (<! (timeout 1000)))))
    (<!! nat)))

