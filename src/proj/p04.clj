(ns proj.p04
  (:require [clojure.string :refer [split]]))

(def ip (map
          read-string
          (split (slurp "p04.txt") #"-")))

(def p1 (for [n (apply range ip)
              :let [digs (loop [res ()
                                curr n]
                           (if (= curr 0)
                             res
                             (recur (conj res (mod curr 10))
                                    (unchecked-divide-int curr 10))))
                    cnt (count digs)
                    neighs (map vector
                                (take (dec cnt) digs)
                                (drop 1 digs))
                    same-pair? (some
                                 #(apply = %) neighs)
                    non-dec? (every?
                               #(apply <= %) neighs)]
              :when (and same-pair? non-dec?)]
          n))

(defn make-fours [_digs]
  (let [nil-seq [nil]
        digs (concat nil-seq _digs nil-seq)]
    (let [cnt (count digs)]
      (map vector
           (take (- cnt 2) digs)
           (drop 1 digs)
           (drop 2 digs)
           (drop 3 digs)))))

(def p2 (for [n (apply range ip)
              :let [digs (loop [res ()
                                curr n]
                           (if (= curr 0)
                             res
                             (recur (conj res (mod curr 10))
                                    (unchecked-divide-int curr 10))))
                    fours (make-fours digs)
                    only-pair? (some
                                 (fn [[a b c d]]
                                   (and (not= a b)
                                        (not= c d)
                                        (= b c))) fours)
                    non-dec? (apply <= digs)]
              :when (and non-dec?
                         only-pair?)]
          n))