(ns proj.p24
  (:require [clojure.string :as str :refer [split-lines]]
            [proj.utils :refer [trace dthrow]]))

(def init (as-> "p24.txt" $
                (slurp $)
                (split-lines $)
                (map #(str/split % #"") $)
                (map #(map (fn [c] (= c "#")) %) $)
                (map #(concat [false] % [false]) $)
                (concat [(repeat 7 false)] $ [(repeat 7 false)])
                (mapv vec $)))

(def idxs (range 1 6))

(defn nc [g loc]
  (count (filter identity
                 (map #(get-in g %)
                      (map #(mapv + loc %)
                           [[1 0] [-1 0]
                            [0 1] [0 -1]])))))

(defn iter [b]
  (reduce
    (fn [a r]
      (reduce
        (fn [a c]
          (let [loc [r c]
                count (nc b loc)]
            (if (get-in b loc)
              (assoc-in a loc (= count 1))
              (assoc-in a loc (or (= count 1)
                                  (= count 2))))))
        a
        idxs))
    b
    idxs))

(defn rating [g]
  (int (apply +
              (for [r idxs
                    c idxs
                    :when (get-in g [r c])]
                (Math/pow 2 (+ (* 5 (dec r))
                               (dec c)))))))

(defn p1 []
  (loop [seen #{(hash init)}
         curr init]
    (let [nxt (iter curr)
          h (hash nxt)]
      (if (contains? seen h)
        (rating nxt)
        (recur (conj seen h)
               nxt)))))


(def init2 (into #{}
                 (for [r idxs
                       c idxs
                       :when (get-in init [r c])]
                   [r c 0])))

(defn neighbour-coords [loc]
  (let [[ri ci] loc
        n1 (map #(mapv + loc %)
                [[1 0 0] [-1 0 0]
                 [0 1 0] [0 -1 0]])]
    (apply concat
           (map
             (fn [[r c l :as nloc]]
               (cond
                 (= r 0) [[2 3 (dec l)]]
                 (= r 6) [[4 3 (dec l)]]
                 (= c 0) [[3 2 (dec l)]]
                 (= c 6) [[3 4 (dec l)]]
                 (and (= r 3) (= c 3)) (cond
                                         (= ri 2) (mapv (fn [i] [1 i (inc l)]) idxs)
                                         (= ri 4) (mapv (fn [i] [5 i (inc l)]) idxs)
                                         (= ci 2) (mapv (fn [i] [i 1 (inc l)]) idxs)
                                         (= ci 4) (mapv (fn [i] [i 5 (inc l)]) idxs)
                                         :default :fuck)
                 :default [nloc]))
             n1))))

(defn nc2 [g loc]
  (count (filter g (neighbour-coords loc))))

(defn iter2 [b]
  (let [scope (reduce
                (fn [res s] (into res s))
                b
                (map neighbour-coords b))]
    (reduce
      (fn [n loc]
        (let [count (nc2 b loc)]
          (if (b loc)
            (if (= count 1)
              n
              (disj n loc))
            (if (or (= count 1)
                    (= count 2))
              (conj n loc)
              n))))
      b scope)))

(defn vis
  ([g] (doseq [l (sort (set (map #(nth % 2) g)))]
         (println "Depth " l)
         (vis g l)))
  ([g layer] (println (str/join "\n"
                                (map
                                  #(str/join "" %)
                                  (reduce
                                    (fn [res [r c]]
                                      (assoc-in res [(dec r) (dec c)] \#))
                                    (mapv (fn [_] (vec (repeat 5 \.))) (range 5))
                                    (filter (fn [[_ _ l]]
                                              (= layer l))
                                            g)))))))
