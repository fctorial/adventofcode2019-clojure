(ns proj.comp
  (:require [clojure.string :refer [split trim-newline]]
            [clojure.core.async :refer [>! <! go chan close!]]))


(def ac {1  3
         2  3
         3  1
         4  1
         5  2
         6  2
         7  3
         8  3
         9  1
         99 0})

(defn read-prog [fl]
  (->> (split (slurp fl) #",")
       (map trim-newline)
       (filter not-empty)
       (mapv bigint)))

(def big0 (bigint 0))
(defn extend-prog [prog by]
  (reduce
    (fn [p _] (conj p big0))
    prog
    (range by)))

(defn pln [& args]
  (locking System/out
    (apply println args)))

(defn run [prog input output id]
  (go
    (loop [mem prog
           meta {:ip   0
                 :base 0}]
      (let [ip (meta :ip)
            op_full (mem ip)
            op (mod op_full 100)
            ac (ac op)
            modes (take ac (concat
                             (reverse (str (unchecked-divide-int op_full 100)))
                             (repeat \0)))
            nxt_ip (+ ip ac 1)
            nxt_meta (assoc meta :ip nxt_ip)
            args_raw (subvec mem (inc ip) nxt_ip)
            args_based (mapv
                         (fn [a m]
                           (if (= m \2)
                             (+ a (meta :base))
                             a))
                         args_raw modes)
            args_ref (mapv
                       (fn [a m]
                         (if (= m \1)
                           a (mem a)))
                       args_based modes)]
        #_(pln id op_full args_raw meta)
        (case op
          1 (recur
              (assoc mem (args_based 2) (+ (args_ref 0) (args_ref 1)))
              nxt_meta)
          2 (recur
              (assoc mem (args_based 2) (* (args_ref 0) (args_ref 1)))
              nxt_meta)
          3 (recur
              (assoc mem (args_based 0) (<! input))
              nxt_meta)
          4 (do
              (>! output (args_ref 0))
              (recur mem
                     nxt_meta))
          5 (recur mem
                   (if (zero? (args_ref 0))
                     nxt_meta
                     (assoc nxt_meta :ip (args_ref 1))))
          6 (recur mem
                   (if (zero? (args_ref 0))
                     (assoc nxt_meta :ip (args_ref 1))
                     nxt_meta))
          7 (recur
              (assoc mem (args_based 2)
                         (if (< (args_ref 0) (args_ref 1))
                           1 0))
              nxt_meta)
          8 (recur
              (assoc mem (args_based 2)
                         (if (= (args_ref 0) (args_ref 1))
                           1 0))
              nxt_meta)
          9 (recur mem
                   (update nxt_meta :base #(+ % (args_ref 0))))
          99 mem)))))
