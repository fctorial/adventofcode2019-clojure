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
