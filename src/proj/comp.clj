(ns proj.comp
  (:require [clojure.string :refer [split trim-newline]]
            [clojure.core.async :refer [>! <! go chan close!]]))


(def ac {1 3
         2 3
         3 1
         4 1
         5 2
         6 2
         7 3
         8 3
         99 0})

(defn read-prog [fl]
  (->> (split (slurp fl) #",")
       (map trim-newline)
       (filter not-empty)
       (mapv read-string)))

(defn run [prog input output id]
  (go
    (loop [mem prog
           ip 0]
      (let [op_full (mem ip)
            op (mod op_full 100)
            ac (ac op)
            modes (take ac (concat
                             (reverse (str (unchecked-divide-int op_full 100)))
                             (repeat \0)))
            nxt_ip (+ ip ac 1)
            args_raw (subvec mem (inc ip) nxt_ip)
            args (mapv
                   (fn [a m]
                     (if (= m \0)
                       (mem a)
                       a))
                   args_raw modes)]
        #_(println id op_full args_raw)
        (case op
          1 (recur
              (assoc mem (args_raw 2) (+ (args 0) (args 1)))
              nxt_ip)
          2 (recur
              (assoc mem (args_raw 2) (* (args 0) (args 1)))
              nxt_ip)
          3 (recur
              (assoc mem (args_raw 0) (<! input))
              nxt_ip)
          4 (do
              (>! output (args 0))
              (recur mem
                     nxt_ip))
          5 (recur mem
                   (if (zero? (args 0))
                     nxt_ip
                     (args 1)))
          6 (recur mem
                   (if (zero? (args 0))
                     (args 1)
                     nxt_ip))
          7 (recur
              (assoc mem (args_raw 2)
                         (if (< (args 0) (args 1))
                           1 0))
              nxt_ip)
          8 (recur
              (assoc mem (args_raw 2)
                         (if (= (args 0) (args 1))
                           1 0))
              nxt_ip)
          99 mem)))))