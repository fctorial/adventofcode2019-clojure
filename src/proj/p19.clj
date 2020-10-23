(ns proj.p19
  (:require [clojure.string :refer [join split split-lines trim]]
            [proj.utils :refer :all]
            [proj.comp :refer [run read-prog extend-prog pln lint]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]))

(def prog (extend-prog (read-prog "p19.txt") 10000))

(defn sim [x y]
  (go
    (let [ip (chan 5)
          op (chan 5)
          d (run prog ip op (str [x y]))]
      (go
        (<! d)
        (close! op))
      (>! ip x)
      (>! ip y)
      (= (<! op) 1))))

(defn is-pulled [x y]
  (<!! (sim x y)))

(defn pulledDrones [X Y]
  (filter identity
          (map (fn [[sp c]]
                 (if (<!! sp)
                   c))
               (for [x (range X)
                     y (range Y)
                     :let [statusp (sim x y)]]
                 [statusp [x y]]))))

(defn gridDrones [X Y]
  (reduce
    (fn [g c]
      (assoc-in g c 1))
    (mapv (fn [_] (vec (repeat Y 0))) (range X))
    (pulledDrones X Y)))

(defn p1 []
  (count (pulledDrones 50 50)))

(defn valid-corner [x y]
  (and (is-pulled x y)
       (is-pulled (+ x 99) y)
       (is-pulled x (+ y 99))))

(defn jiggle-up [[x y]]
  (let [ny (bsearch-lower 0 y #(valid-corner x %))
        nx (bsearch-lower 0 x #(valid-corner % ny))]
    [nx ny]))

(defn goku [[x y]]
  (apply min-key (partial apply +)
         (filter
           (fn [[x y]]
             (valid-corner x y))
           (map
             (fn [fr]
               (let [x (int (/ (* x fr) 2000))
                     y (int (/ (* y fr) 2000))]
                 [x y]))
             (range 1000 2000)))))


(defn p2 []
  (let [jiggled (loop [c [2500 1800]]
                  (let [nc (jiggle-up c)]
                    (println nc)
                    (if (= nc c)
                      c
                      (recur nc))))]
    (goku jiggled)))