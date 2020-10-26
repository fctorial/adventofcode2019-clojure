(ns proj.p21
  (:require [proj.comp :refer [run read-prog extend-prog]]
            [proj.utils :refer [zip-colls printing-chan trace]]
            [clojure.core.async :refer [>! <! go chan close! poll! <!! >!!]]
            [clojure.string :as str]))

(def prog (-> "p21.txt"
              read-prog
              (extend-prog 10000)))

(defn try-prog [& p]
  (let [ip (chan 1024)
        op (printing-chan)
        comp (run prog ip op nil)
        script (str/join "\n" (concat p [""]))]
    (doseq [c script]
      (>!! ip (int c)))
    (<!! comp)
    nil))

(defn try-prog-1 [& p]
  (apply try-prog (concat p ["WALK"])))

(defn try-prog-2 [& p]
  (time (apply try-prog (concat p ["RUN"])))
  (println (count p)))

(defn p1 []
  (try-prog-1 "NOT C J" "NOT D T" "NOT T T" "AND T J" "NOT A T" "OR T J"))
(defn p2 []
  (try-prog-2 "NOT B J" "NOT C T" "OR T J" "NOT G T" "OR T J" "AND D J" "AND H J" "NOT B T" "NOT T T" "AND C T" "NOT T T" "AND T J" "NOT A T" "OR T J"))

