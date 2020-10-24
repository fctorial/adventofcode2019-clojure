(ns proj.p21
  (:require [proj.comp :refer [run read-prog extend-prog]]
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
        script (str/join "\n" (concat p ["WALK" ""]))]
    (doseq [c script]
      (>!! ip (int c)))
    (<!! comp)
    nil))

(defn p1 []
  )