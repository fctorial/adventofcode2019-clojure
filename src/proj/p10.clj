(ns proj.p10
  (:require [clojure.string :refer [split split-lines trim-newline]]))

(def ip (->> (slurp "p10.txt")
             split-lines
             (mapv vec)))
(defn loc [x y]
  (get-in ip [x y]))
(def X (count ip))
(def Y (count (first ip)))

(def asts (vec (for [x (range X)
                     y (range Y)
                     :when (= (loc x y)
                              \#)]
                 [x y])))

(def N (count asts))
(defn calc-angs []
  (mapv (fn [ai]
          (mapv (fn [aj]
                  (if (not= ai aj)
                    (let [del (map - aj ai)]
                      (* -1 (Math/atan2 (second del) (first del))))))
                asts))
        asts))

(defn _p1 [angs]
  (->> angs
       (map frequencies)
       (map count)
       (map dec)
       (map (fn [i c] [i c]) (range N))
       (reduce #(max-key second %1 %2))))
(defn p1 []
  (_p1 (calc-angs)))

(defn collect [coll cmp]
  (reduce (fn [res [k v]]
            (assoc res v (if (contains? res v)
                           (conj (res v) k)
                           (sorted-set-by cmp k))))
          (sorted-map) coll))

(defn dist2 [i j]
  (let [li (asts i)
        lj (asts j)]
    (->> (map - lj li)
         (map #(* % %))
         (reduce +))))

(defn _p2 [angs]
  (let [laser (first (_p1 angs))
        view (collect
               (map vector (range N) (angs laser))
               (fn [i j]
                 (< (dist2 i laser)
                    (dist2 j laser))))]
    (loop [left 200
           last nil
           curr 0
           view (vec (map second (dissoc view nil)))]
      (if (zero? left)
        (asts last)
        (let [targeted (view curr)
              next (inc curr)
              next (if (= (count view) next)
                     0 next)]
          (if (empty? targeted)
            (recur left last next view)
            (recur (dec left)
                   (first targeted)
                   next
                   (update view curr rest))))))))
(defn p2 []
  (_p2 (calc-angs)))
