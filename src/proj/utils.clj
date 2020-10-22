(ns proj.utils
  (:require [clojure.core.async :as async :refer [go <! >! <!! >!! go-loop chan close!]]
            [clojure.set :refer :all]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

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

(defn manhatten [a b]
  (->> (map - a b)
       (map #(Math/abs %))
       (apply +)))

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

(defn closed-chan []
  (let [ch (chan)]
    (close! ch)
    ch))

(defn chan->seq [ch]
  (when-let [val (<!! ch)]
    (lazy-seq (cons val (chan->seq ch)))))

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

(def uppercase-alpha "QWERTYUIOPASDFGHJKLZXCVBNM")

(defn uppercase? [s]
  (clojure.string/includes? uppercase-alpha s))

(defmacro mdef [names value]
  (if (seqable? names)
    (let [res-var (gensym)
          varnames (map (fn [_] (gensym)) names)
          defexprs (map (fn [[vn n]]
                          `(def ~n ~vn)) (zip-colls varnames names))]
      `(let [~res-var ~value
             [~@varnames] ~res-var]
         ~@defexprs
         ~res-var))
    `(def ~names ~value)))

(defn lower-char? [c]
  (and (char? c) (<= (int \a) (int c) (int \z))))
(defn upper-char? [c]
  (and (char? c) (<= (int \A) (int c) (int \Z))))
(defn map-values [m f]
  (into {}
        (map (fn [[k v]] [k (f v)]) m)))
(defn printing-chan [n]
  (let [c (chan n)]
    (go
      (loop []
        (let [nxt (<! c)]
          (when nxt
            (println nxt)
            (recur)))))
    c))
(defn sink-chan [n]
  (let [c (chan n)]
    (go
      (loop []
        (let [_ (<! c)])))
    c))

(defn filter-out [val coll]
  (filter #(not= val %) coll))

(defn split-coll [val coll]
  (let [[fst rst] (split-with #(not= val %) coll)]
    (lazy-seq (cons fst (let [rst (drop 1 rst)]
                          (if (empty? rst)
                            nil
                            (split-coll val rst)))))))

(defn dijkstra [neighbour-fn dist-fn src]
  (loop [border (priority-map-keyfn first src [0 nil])
         visited {}]
    (if (empty? border)
      visited
      (let [[curr [curr-dist parent]] (peek border)
            border (pop border)
            neighbours (neighbour-fn curr)]
        (recur
          (reduce (fn [border neighbour]
                    (let [new-dist (+ curr-dist (dist-fn curr neighbour))]
                      (if (and (or (not (contains? visited neighbour))
                                   (< new-dist (first (visited neighbour))))
                               (< new-dist (get-in border [neighbour 0] Long/MAX_VALUE)))
                        (assoc border neighbour [new-dist curr])
                        border))) border neighbours)
          (assoc visited curr [curr-dist parent]))))))