(ns proj.p03
  (:require [clojure.string :as string])
  (:require [clojure.set :refer :all]))

(def dirmap {"R" [1 0]
             "D" [0 1]
             "L" [-1 0]
             "U" [0 -1]})

(defn manhatten
  ([p] (apply + (map #(Math/abs %) p)))
  ([p1 p2] (manhatten (map - p1 p2))))

(defn linear [curr p nxt]
  (= (manhatten curr nxt)
     (+ (manhatten curr p)
        (manhatten p nxt))))

(defn s []
  (let [[w1_raw w2_raw] (->> (slurp "p03.txt")
                             string/split-lines
                             (filter not-empty))
        parse-path (fn [ln]
                     (let [steps (filter not-empty (string/split ln #","))]
                       (map
                         (fn [e]
                           {:dir (dirmap (subs e 0 1))
                            :len (read-string (subs e 1))})
                         steps)))
        w1 (parse-path w1_raw)
        w2 (parse-path w2_raw)
        covered-blocks (fn [w]
                         (loop [curr [0 0]
                                path w
                                res #{}]
                           (if (empty? path)
                             res
                             (let [step (first path)
                                   dir (step :dir)
                                   len (step :len)
                                   del (map *
                                            dir [len len])
                                   between (set (map
                                                  #(mapv +
                                                         curr (map *
                                                                   dir [% %]))
                                                  (range 1 (inc len))))]
                               (recur (mapv + curr del)
                                      (rest path)
                                      (union res between))))))
        w1_blocks (covered-blocks w1)
        w2_blocks (covered-blocks w2)
        intersections (intersection w1_blocks w2_blocks)
        ;p1 (reduce #(min-key manhatten %1 %2) intersections)
        dist-along-wire (fn [p w]
                          (loop [curr [0 0]
                                 path w
                                 dist 0]
                            (let [step (first path)
                                  dir (step :dir)
                                  len (step :len)
                                  del (map *
                                           dir [len len])
                                  nxt (mapv +
                                            curr del)
                                  in-between (linear curr p nxt)]
                              (if in-between
                                (+ dist (manhatten p curr))
                                (recur nxt
                                       (rest path)
                                       (+ dist (manhatten curr nxt)))))))
        p2 (reduce #(min-key first %1 %2)
                   (map (fn [p]
                          [(+ (dist-along-wire p w1)
                              (dist-along-wire p w2))
                           p]) intersections))]
    p2))
