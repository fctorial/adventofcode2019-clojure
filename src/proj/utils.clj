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

(deftype vecview [v s e]
  clojure.lang.IPersistentVector
  (length [_]
    (- e s))
  (assocN [_ i val]
    (throw (new UnsupportedOperationException)))
  (cons [_ val]
    (throw (new UnsupportedOperationException)))
  (rseq [_]
    (throw (new UnsupportedOperationException)))
  (peek [_]
    (throw (new UnsupportedOperationException)))
  (pop [_]
    (throw (new UnsupportedOperationException)))
  (assoc [_ k v]
    (throw (new UnsupportedOperationException)))
  (nth [_ i]
    (if (and (>= i s)
             (< i e))
      (v (+ s i))
      (throw (new IndexOutOfBoundsException))))
  (nth [_ i nf]
    (if (and (>= i s)
             (< i e))
      (v (+ s i))
      nf))
  (containsKey [_ k]
    (and (< k e)
         (>= k s)))
  (entryAt [this k]
    [k (nth this k)])
  clojure.lang.IFn
  (invoke [this a]
    (nth this a)))
