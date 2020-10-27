(ns proj.p25
  (:require [proj.comp :refer [extend-prog read-prog run]]
            [proj.utils :refer []]
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

(defn p1 []
  (let [ip (chan 10240)
        op (printing-chan)]
    (run prog ip op nil)
    (doseq [line (line-seq (BufferedReader. *in*))]
      (doseq [c line]
        (>!! ip (int c)))
      (>!! ip 10))))

(defn p2 [])

(def ITEMS ["mutex"
            "dark matter"
            "klein bottle"
            "tambourine"
            "fuel cell"
            "astrolabe"
            "monolith"
            "cake"])

;(def commander (p1))
;
;(defn cmd [s]
;  (doseq [c s]
;    (>!! commander (int c)))
;  (>!! commander 10))
