(ns proj.p25
  (:require [proj.comp :refer [extend-prog read-prog run]]
            [proj.utils :refer [zip-colls]]
            [clojure.core.async :refer [go chan <!! <! >!!]])
  (:import (java.io BufferedReader)))

(def prog (extend-prog (read-prog "p25.txt") 10000))

(defn printing-chan []
  (let [c (chan 1024)]
    (go
      (loop []
        (let [nxt (<! c)]
          (when nxt
            (if (< nxt 255)
              (print (char nxt))
              (println nxt))
            (flush)
            (recur)))))
    c))

(def aliases {"n" "north"
              "s" "south"
              "e" "east"
              "w" "west"})

(defn record []
  (let [ip (chan 10240)
        op (printing-chan)
        _ (run prog ip op nil)]
    (loop [lines (line-seq (BufferedReader. *in*))
           entered []]
      (let [line (aliases (first lines) (first lines))]
        (if (= "finish" line)
          entered
          (do
            (doseq [c line]
              (>!! ip (int c)))
            (>!! ip 10)
            (recur (rest lines)
                   (conj entered line))))))))

(defn replay [ip r]
  (doseq [line r]
    (doseq [c line]
      (>!! ip (int c)))
    (>!! ip 10)))

(def collectR ["south"
               "take cake"
               "south"
               "west"
               "take mutex"
               "east"
               "north"
               "east"
               "east"
               "north"
               "south"
               "west"
               "west"
               "north"
               "west"
               "take klein bottle"
               "south"
               "west"
               "east"
               "east"
               "take monolith"
               "south"
               "take fuel cell"
               "west"
               "west"
               "take astrolabe"
               "east"
               "east"
               "north"
               "west"
               "north"
               "west"
               "north"
               "take tambourine"
               "south"
               "west"
               "take dark matter"
               "west"])

(def ITEMS ["mutex"
            "dark matter"
            "klein bottle"
            "tambourine"
            "fuel cell"
            "astrolabe"
            "monolith"
            "cake"])

(defn cmd [ip s]
  (doseq [c s]
    (>!! ip (int c)))
  (>!! ip 10))

(defn pow [b e]
  (reduce (fn [res _] (* res b)) 1 (range e)))

(defn chooseItems [i]
  (for [[item idx] (zip-colls ITEMS (range))
        :when (not (zero? (bit-and i (pow 2 idx))))]
    item))

(defn _p1 []
  (let [ip (chan 10240)
        op (printing-chan)]
    (run prog ip op nil)
    (replay ip collectR)
    (doseq [i (range 256)]
      (read-line)
      (doseq [item ITEMS]
        (cmd ip (str "drop " item)))
      (doseq [item (chooseItems i)]
        (cmd ip (str "take " item)))
      (cmd ip "north")
      (cmd ip "inv"))))

(def CORRECT_ITEMS ["dark matter"
                    "tambourine"
                    "astrolabe"
                    "monolith"])

(defn p1 []
  (let [ip (chan 10240)
        op (printing-chan)]
    (run prog ip op nil)
    (replay ip collectR)
    (doseq [item CORRECT_ITEMS]
      (cmd ip (str "take " item)))))

(defn p2 [])
