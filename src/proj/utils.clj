(ns proj.utils
  (:require [clojure.core.async :refer [<! >! go-loop chan merge]]))

(defn zip-colls [& cs]
  (partition (count cs)
             (apply interleave cs)))

(defn iter-n [coll n]
  (apply zip-colls
         (map
           #(drop % coll)
           (range n))))

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

(defn -< [ip ops]
  (go-loop []
    (let [val (<! ip)]
      (when val
        (doseq [op ops]
          (>! op val))
        (recur)))))

(defn >- [ips op]
  (let [ip (merge ips)]
    (go-loop []
      (let [val (<! ip)]
        (when val
          (>! op val)
          (recur))))))
