(ns proj.p04
  (:require [clojure.string :refer [split]]))

(def ip (map
          read-string
          (split (slurp "p04.txt") #"-")))

(defn get-digits [n]
  (loop [res ()
         curr n]
    (if (= curr 0)
      res
      (recur (conj res (mod curr 10))
             (unchecked-divide-int curr 10)))))

(defn join-digits [digs]
  (loop [res 0
         digs digs]
    (if (empty? digs)
      res
      (recur (+ (first digs)
                (* 10 res))
             (rest digs)))))

(defn make-fours [_digs]
  (let [nil-seq [nil]
        digs (concat nil-seq _digs nil-seq)]
    (let [cnt (count digs)]
      (map vector
           (take (- cnt 2) digs)
           (drop 1 digs)
           (drop 2 digs)
           (drop 3 digs)))))

(defn make-pairs [coll]
  (map vector
       coll
       (drop 1 coll)))

(defn inc-nums [st end]
  (if (> st end)
    nil
    (let [digs (get-digits st)
          non-dec? (apply <= digs)]
      (if non-dec?
        (lazy-seq (cons digs (inc-nums (inc st) end)))
        (let [pairs (make-pairs digs)
              inc-ind (inc (count (take-while (fn [[n1 n2]]
                                                (<= n1 n2)) pairs)))
              first-part (take inc-ind digs)
              nxt-st-digs (concat first-part (repeat (- (count digs) inc-ind) (last first-part)))]
          (recur (join-digits nxt-st-digs) end))))))

(defn p1 []
  (count (filter
           (fn [n]
             (some #(apply = %)
                   (make-pairs n)))
           (apply inc-nums ip))))

(defn p2 []
  (count (filter
           (fn [n]
             (some
               (fn [[a b c d]]
                 (and (= b c)
                      (not= a b)
                      (not= c d)))
               (make-fours n)))
           (apply inc-nums ip))))