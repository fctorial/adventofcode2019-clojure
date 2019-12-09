(ns proj.core)

(defn zip-colls [& cs]
  (partition (count cs)
             (apply interleave cs)))

(defn iter-n [coll n]
  (apply zip-colls
         (map
           #(drop % coll)
           (range n))))