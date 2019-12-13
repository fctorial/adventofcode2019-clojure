(ns proj.utils
  (:require [clojure.core.async :as async :refer [<! >! go-loop chan]]
            [clojure.set :refer :all]))

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
  (let [ip (async/merge ips)]
    (go-loop []
      (let [val (<! ip)]
        (when val
          (>! op val)
          (recur))))))

(defn primes-below [n]
  (let [lim (Math/sqrt n)]
    (loop [curr 3
           seive (transient (vec (concat [false false true]
                                         (take (- n 2) (cycle [true false])))))]
      (let [nxt (+ 2 curr)]
        (if (> curr lim)
          (filter
            #(seive %)
            (range (inc n)))
          (if (seive curr)
            (recur nxt
                   (reduce
                     #(assoc! %1 (* %2 curr) false)
                     seive
                     (range 2 (/ (inc n) curr))))
            (recur nxt
                   seive)))))))

(def factorize
  (memoize
    (fn [n primes]
      (if (< n 2)
        {}
        (let [pr (first
                   (drop-while #(not= 0 (mod n %)) primes))
              [pr_pow left] (loop [pr_pow 0
                                   curr n]
                              (if (not= 0 (mod curr pr))
                                [pr_pow curr]
                                (recur (inc pr_pow) (/ curr pr))))]
          (assoc (factorize left primes) pr pr_pow))))))

(defn lcm [& nums]
  (let [prs (primes-below (apply max nums))
        fs (map #(factorize % prs) nums)
        lcm (reduce #(merge-with max %1 %2) fs)]
    (apply *
           (map
             (fn [[p c]]
               (apply * (repeat c p)))
             lcm))))

(defn hcf [& nums]
  (let [prs (primes-below (apply max nums))
        fs (map #(factorize % prs) nums)
        common (apply intersection (->> fs
                                        (map keys)
                                        (map set)))
        hcf (select-keys
              (reduce #(merge-with min %1 %2) fs)
              common)]
    (apply *
           (map
             (fn [[p c]]
               (apply * (repeat c p)))
             hcf))))

(defn lmap [f & colls]
  (fn [i]
    (apply f (map #(% i) colls))))