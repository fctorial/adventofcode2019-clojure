(ns proj.p17
  (:require [clojure.string :refer [join split split-lines trim]]
            [proj.utils :refer :all]
            [proj.comp :refer [run read-prog extend-prog pln lint]]
            [clojure.core.async :as async :refer [>! >!! <! <!! chan go go-loop close!]]
            [proj.vis.main :refer [create-window synced-window]])
  (:import (java.awt Color)
           (proj.java Return)))

(def prog (extend-prog (read-prog "p17.txt") 10000))

(defn get-feed []
  (let [ip (closed-chan)
        op (chan 5)
        cam (run prog ip op "cam")]
    (go
      (<! cam)
      (close! op))
    (let [feed (chan->seq op)
          grid (->> feed
                    (map char)
                    (split-coll \newline)
                    (filter not-empty)
                    (mapv vec)
                    vec)]
      grid)))

(def dirs {\^ [-1 0]
           \v [1 0]
           \< [0 -1]
           \> [0 1]})

(defn is-intersection [grid pos]
  (every? #(= \# %) (map
                      #(let [pos (mapv + pos %)]
                         (get-in grid pos))
                      (conj (vals dirs) [0 0]))))

(defn alignment-param [pos]
  (apply * pos))

(defn p1 []
  (let [grid (get-feed)
        intersections (filter #(is-intersection grid %)
                              (for [x (range (count (first grid)))
                                    y (range (count grid))]
                                [x y]))]
    (reduce + (map alignment-param intersections))))

(def color-map {\# (new Color 0 0 100)
                \. (new Color 100 100 100)
                \> (new Color 100 0 0)
                \< (new Color 100 0 0)
                \^ (new Color 100 0 0)
                \v (new Color 100 0 0)})

(defn display-grid [grid]
  (let [X (count (first grid))
        Y (count grid)
        vis (create-window X Y 12)]
    (go
      (doseq [[line y] (zip-colls grid (range))]
        (doseq [[elem x] (zip-colls line (range))]
          (>! vis [x y (color-map elem)]))))
    nil))

(defn pad-feed [feed]
  (let [X (count (first feed))
        Y (count feed)]
    (vec (concat [(vec (repeat (+ X 2) \.))]
                 (map
                   #(vec (concat [\.] % [\.]))
                   feed)
                 [(vec (repeat (+ X 2) \.))]))))

(defn get-bot [grid]
  (let [X (count (first grid))
        Y (count grid)]
    (first
      (for [x (range X)
            y (range Y)
            :let [dir (dirs (get-in grid [x y]))]
            :when dir]
        {:dir dir
         :pos [x y]}))))

(defn extract-segs [grid]
  (loop [bot (get-bot grid)
         moves []]
    (let [dir (bot :dir)
          pos (bot :pos)
          frnt (mapv + pos dir)]
      (if (= \. (get-in grid frnt))
        (let [[dx dy] dir
              left [(* dy -1) dx]
              right [dy (* dx -1)]]
          (cond
            (= \# (get-in grid (mapv + pos left))) (recur (assoc bot :dir left)
                                                          (conj moves \L))
            (= \# (get-in grid (mapv + pos right))) (recur (assoc bot :dir right)
                                                           (conj moves \R))
            :else moves))
        (let [[steps nxt_pos] (loop [curr_pos pos
                                     steps 0]
                                (let [frnt (mapv + curr_pos dir)
                                      nxt (get-in grid frnt)]
                                  (if (= \. nxt)
                                    [steps curr_pos]
                                    (recur frnt (inc steps)))))]
          (recur {:dir dir
                  :pos nxt_pos}
                 (conj moves steps)))))))

(defn minimize-routine [r]
  (apply concat (map #(if (char? (first %))
                        %
                        [(apply + %)]) (partition-by char? r))))

(defn routine-size [r]
  (count (join "," (minimize-routine r))))

(defn index-of [pred coll] (first (keep-indexed #(if (pred %2) %1) coll)))

(defn index-of-seq [a b]
  (index-of identity (map #{a} (partition (count a) 1 b))))

(defn split-coll-seq [seq coll]
  (let [idx (index-of-seq seq coll)]
    (if idx
      (concat [(subvec coll 0 idx)]
              (split-coll-seq seq (subvec coll (+ idx (count seq)))))
      [coll])))

(defn expand-routine [r]
  (vec (flatten (map
                  #(if (int? %)
                     (if (not= (mod % 2) 0)
                       (throw (new Exception))
                       (repeat (/ % 2) 2))
                     %)
                  r))))

(defn compress-routine [r]
  (mapv
    #(if (number? (first %))
       (apply + %)
       (first %))
    (partition-by type r)))

(defn rev-map [m]
  (into {}
        (map (fn [[k v]]
               [v k])
             m)))

(defmacro df [ks vex]
  (let [vss (mapv gensym ks)
        ex (map
             (fn [[k vs]] (list 'def k vs))
             (zip-colls ks vss))]
    `(let [val# ~vex
           ~vss val#]
       ~@ex
       nil)))

;(declare grid)
;(declare segs)
;(declare full)
;(declare sub1-lim)
;(declare sub2-lim)
;(declare test-grid)
;(defn init []
;  ;(def test-grid (pad-feed (mapv
;  ;                           #(mapv first (split % #""))
;  ;                           (split "#######...#####\n#.....#...#...#\n#.....#...#...#\n......#...#...#\n......#...###.#\n......#.....#.#\n^########...#.#\n......#.#...#.#\n......#########\n........#...#..\n....#########..\n....#...#......\n....#...#......\n....#...#......\n....#####......"
;  ;                                  #"\n"))))
;  ;(def grid test-grid)
;  (def grid (pad-feed (get-feed)))
;  (def segs (extract-segs grid))
;  (def full (expand-routine segs))
;  (def sub1-lim (last (filter
;                        #(<= (routine-size (compress-routine (subvec full 0 %)))
;                             20)
;                        (range (count full)))))
;  (def sub2-lim (last (filter
;                        #(<= (routine-size (compress-routine (subvec full (- (count full) %))))
;                             20)
;                        (range (count full))))))

(defn is-prefix [pref coll]
  (and (>= (count coll)
           (count pref))
       (every? #(apply = %)
               (zip-colls pref coll))))

(defn refactor
  ([submap full] (refactor submap full []))
  ([submap remaining result] (if (empty? remaining)
                               result
                               (let [[name routine :as prefix] (first (filter (fn [[_ routine]] (is-prefix routine remaining))
                                                                              submap))]
                                 (if prefix
                                   (recur submap
                                          (drop (count routine) remaining)
                                          (conj result name)))))))

(defn find-result []
  (try
    (let [grid (pad-feed (get-feed))
          segs (extract-segs grid)
          full (expand-routine segs)
          sub1-lim (last (filter
                           #(<= (routine-size (compress-routine (subvec full 0 %)))
                                20)
                           (range (count full))))
          sub2-lim (last (filter
                           #(<= (routine-size (compress-routine (subvec full (- (count full) %))))
                                20)
                           (range (count full))))]
      (doseq [g1 (range 1 sub1-lim)
              g2 (range 1 sub2-lim)]
        (let [lower (- (count full) g2)
              sub1 (subvec full 0 g1)
              sub2 (subvec full (- (count full) g2))]
          (doseq [sub3-start (range g1 lower)
                  sub3-end (range (inc sub3-start) (dec (first (filter
                                                                 #(or
                                                                    (>= % lower)
                                                                    (> (routine-size (compress-routine (subvec full sub3-start %)))
                                                                       20))
                                                                 (range sub3-start (inc lower))))))
                  :let [sub3 (subvec full sub3-start sub3-end)]
                  :when (<= (routine-size (compress-routine sub3))
                            20)]
            (let [routine-map {\A sub1
                               \B sub2
                               \C sub3}
                  main (refactor routine-map
                                 full)]
              (when (and (not (empty? main))
                         (<= (routine-size main) 20))
                (throw (new Return {:main main
                                    :subs (map-values routine-map compress-routine)}))))))))
    (catch Return ret
      (.-value ret))))

(defn compile-routine [r]
  (join "," r))

(def result {:main [\A \B \A \C \B \A \C \A \C \B],
             :subs {\A [\L 12 \L 8 \L 8], \B [\L 12 \R 4 \L 12 \R 6], \C [\R 4 \L 12 \L 12 \R 6]}})

(defn p2 []
  (let [result (find-result)
        main (compile-routine (result :main))
        subs (map compile-routine (vals (result :subs)))
        ip (chan 40960)
        op (chan 40960)
        prog (assoc prog 0 2)
        task (run prog ip op "processor")]
    (doseq [c main]
      (>!! ip (int c)))
    (>!! ip (int \newline))
    (doseq [sub subs]
      (doseq [c sub]
        (>!! ip (int c)))
      (>!! ip (int \newline)))
    (>!! ip (int \n))
    (>!! ip (int \newline))
    (close! ip)
    (<!! task)
    (close! op)
    (last (chan->seq op))))