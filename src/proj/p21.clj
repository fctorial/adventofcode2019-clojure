(ns proj.p21
  (:require [proj.comp :refer [run read-prog extend-prog]]
            [proj.utils :refer [zip-colls]]
            [clojure.core.async :refer [>! <! go chan close! poll! <!! >!!]]
            [clojure.string :as str]))

(def prog (-> "p21.txt"
              read-prog
              (extend-prog 10000)))

(defn printing-chan []
  (let [res (chan 1024)]
    (go
      (loop []
        (let [nxt (<! res)]
          (when nxt
            (if (> nxt 256)
              (println nxt)
              (print (char nxt)))
            (recur)))))
    res))

(defn try-prog [& p]
  (let [ip (chan 1024)
        op (printing-chan)
        comp (run prog ip op nil)
        script (str/join "\n" (concat p [""]))]
    (doseq [c script]
      (>!! ip (int c)))
    (<!! comp)
    nil))

(defn gen-matcher [[reg status :as a]]
  (if status
    [(str "NOT " reg " T")
     (str "NOT T T")
     (str "AND T J")]
    [(str "NOT " reg " T")
     (str "AND T J")]))

(defn trace [a]
  (println (type a) " => " a)
  a)

(defn compile-script [ss n]
  (->> ss
       (map #(take n (concat % (repeat nil))))
       (map #(zip-colls [\A \B \C \D \E \F \G \H \I] %))
       (map #(filter (fn [[_ status]] (not (nil? status))) %))
       (map #(map gen-matcher %))
       (map #(apply concat %))
       (apply concat)
       ))

(defn try-prog-1 [& p]
  (apply try-prog (concat p ["WALK"])))

(defn try-prog-2 [& p]
  (apply try-prog (concat p ["RUN"])))

(defn p1 []
  (try-prog-1 "NOT C J" "NOT D T" "NOT T T" "AND T J" "NOT A T" "OR T J"))

