(ns proj.p14
  (:require [clojure.string :refer [split split-lines trim-newline trim]]
            [amalloy.ring-buffer :refer :all]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]))

(def ip (->> (slurp "p14.txt")
             split-lines
             (map #(split % #"=>|,"))
             (map #(map trim %))))

(def rs (into {}
              (map
                (fn [es]
                  (let [es (mapv
                             (fn [e]
                               (let [[q u] (split e #" ")]
                                 [u (read-string q)]))
                             es)
                        [unit amount] (last es)]
                    [unit {:ingredients (into {} (pop es)) :amount amount}]))
                ip)))

(def children (assoc
                (->> rs
                     (map (fn [[p c]]
                            (let [cs (keys (c :ingredients))]
                              [p cs])))
                     (into {}))
                "ORE" '()))

(def parents (assoc (reduce
                      (fn [res [p cs]]
                        (reduce
                          (fn [res c]
                            (if (contains? res c)
                              (update res c #(conj %1 p))
                              (assoc res c #{p})))
                          res cs))
                      {} children)
               "FUEL" '()))

(def depth (loop [res {}
                  left (conj clojure.lang.PersistentQueue/EMPTY ["FUEL" 0])]
             (if (empty? left)
               res
               (let [[p d] (first left)
                     cs (children p)]
                 (recur
                   (assoc res p d)
                   (reduce
                     (fn [left c]
                       (conj left [c (inc d)]))
                     (pop left) cs))))))

(def height (loop [res {}
                   left (conj clojure.lang.PersistentQueue/EMPTY ["ORE" 0])]
              (if (empty? left)
                res
                (let [[p d] (first left)
                      cs (parents p)]
                  (recur
                    (if (contains? res p)
                      res
                      (assoc res p d))
                    (reduce
                      (fn [left c]
                        (conj left [c (inc d)]))
                      (pop left) cs))))))

(defn _p1 [f]
  (loop [req (sorted-map-by (fn [a b]
                              (let [dc (compare (depth a)
                                                (depth b))]
                                (if (not= dc 0)
                                  dc
                                  (let [hc (compare (height b)
                                                    (height a))]
                                    (if (not= hc 0)
                                      hc
                                      (compare a b)))))) "FUEL" f)]
    (if (= "ORE"
           (first (first req)))
      req
      (let [[unit amount] (first req)
            rxn (rs unit)
            rxn_reps (quot amount (rxn :amount))
            rxn_reps (if (zero? (mod amount
                                     (rxn :amount)))
                       rxn_reps
                       (inc rxn_reps))
            ingredients (into {}
                              (map
                                (fn [[u am]]
                                  [u (* am rxn_reps)])
                                (rxn :ingredients)))
            nxt_reqs (reduce (fn [reqs [u am]]
                               (if (contains? reqs u)
                                 (update reqs u #(+ % am))
                                 (assoc reqs u am)))
                             (dissoc req unit)
                             ingredients)]
        (recur nxt_reqs)))))

(defn p1 []
  (_p1 1))

(defn binary-search [s e pred]
  (cond
    (pred e) e
    (and (= (inc s) e)
         (pred s)) s
    :else (let [mid (unchecked-divide-int (+ s e) 2)]
            (if (pred mid)
              (recur (inc mid) e pred)
              (recur s (dec mid) pred)))))

(defn p2 []
  (binary-search 1 10000000 (fn [v] (< ((_p1 v) "ORE")
                                    1000000000000))))
