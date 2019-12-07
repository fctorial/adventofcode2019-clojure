(ns proj.p07
  (:require [clojure.string :refer [split trim-newline]]
            [proj.comp :refer [run read-prog]]
            [clojure.math.combinatorics :refer [cartesian-product permutations]]
            [clojure.core.async :refer [>!! <!! >! <! poll! go go-loop chan close! sliding-buffer]]))

(def prog (read-prog "p07.txt"))

(defn pln [& args]
  (locking System/out
    (apply println args)))

(defn link-amps [phases]
  (let [thrust (chan 1)
        inputs (mapv (fn [_] (chan 5)) (range 5))
        outputs (mapv
                  #(run prog %1 %2)
                  inputs (range 5))]
    (doseq [i [0 1 2 3]]
      (go
        (let [val (<! (outputs i))]
          (>! (inputs (inc i)) val))))
    (go
      (>! thrust (<! (outputs 4))))
    (doall
      (map (fn [i p]
             (>!! i p)) inputs phases))
    (go
      (>!! (first inputs) 0))
    thrust))

(defn p1 [] ()
  (->> (permutations (range 5))
       (map link-amps)
       (map <!!)
       (reduce max)))

(defn backloop-phases [phases]
  (let [thrust (chan 20)
        inputs (mapv (fn [_] (chan 5)) (range 5))
        outputs (mapv
                  #(run prog %1 %2)
                  inputs (range 5))]
    (doseq [i [0 1 2 3]]
      (go-loop []
        (let [val (<! (outputs i))]
          (if val
            (do
              (>! (inputs (inc i)) val)
              (recur))))))
    (go-loop []
      (let [last (<! (outputs 4))]
        (if last
          (do
            (>! (first inputs) last)
            (>! thrust last)
            (recur))
          (close! thrust))))
    (doall
      (map (fn [i p]
             (>!! i p)) inputs phases))
    (go
      (>!! (first inputs) 0))
    thrust))

(defn p2 []
  (->> (permutations (range 5 10))
       (map #(backloop-phases %))
       (map #(loop [last nil
                   curr (<!! %)]
               (if (nil? curr)
                 last
                 (recur curr (<!! %)))))
       (reduce max)))
