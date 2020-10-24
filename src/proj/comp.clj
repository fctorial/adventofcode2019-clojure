(ns proj.comp
  (:require [clojure.string :refer [split trim-newline] :as str]
            [clojure.core.async :refer [>! <! go chan close! poll! <!! >!!]]
            [proj.utils :refer [zip-colls]]))

(defn _chan->seq [ch]
  (let [val (poll! ch)]
    (if val
      (lazy-seq (cons val (_chan->seq ch))))))

(defn chan->seq [ch]
  (let [val (<!! ch)]
    (if val
      (lazy-seq (cons val (chan->seq ch))))))

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
  (->> (split (str/replace (slurp fl) #"\s+" "") #",")
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

(defn format-data [prog res off]
  (>!! res "DATA:=>")
  (doseq [[chunk o] (zip-colls (partition 4 (subvec prog off))
                                   (range off (+ off 1000000) 4))]
    (>!! res (str o "\t\t: " (str/join ", " chunk)))))

(defn lint [prog code_size]
  (let [res (chan 10000)]
    (go
      (loop [off 0]
        (if (>= off code_size)
          (do
            (format-data prog res off)
            (close! res))
          (let [op_full (prog off)
                op (mod op_full 100)
                ac (ac op)]
            (cond
              (= op 0) (throw (new Exception "JABBASHIT"))
              (= op 99) (do
                          (>! res (str off "\t\t: 99"))
                          (recur (inc off)))
              :default (do
                         (>! res (str off "\t\t: " (str/join ", " (subvec prog off (+ off ac 1)))))
                         (recur (+ off ac 1))))))))
    (str/join "\n" (chan->seq res))))

(defn run [prog input output id]
  (go
    (loop [mem prog
           meta {:ip   0
                 :base 0}]
      (let [log (if id
                  (fn [msg]
                    (println msg))
                  (fn [& _]))
            ip (meta :ip)
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
        (case op
          1 (do
              (log (str "ADD *" (args_based 0) " *" (args_based 1) " => " (args_based 2)))
              (recur
               (assoc mem (args_based 2) (+ (args_ref 0) (args_ref 1)))
               nxt_meta))
          2 (do
              (log (str "MUL *" (args_based 0) " *" (args_based 1) " => " (args_based 2)))
              (recur
               (assoc mem (args_based 2) (* (args_ref 0) (args_ref 1)))
               nxt_meta))
          3 (do
              (log (str "INP => " (args_based 0)))
              (recur
               (assoc mem (args_based 0) (<! input))
               nxt_meta))
          4 (do
              (log (str "OUT <= *" (args_based 0)))
              (do
               (>! output (args_ref 0))
               (recur mem
                      nxt_meta)))
          5 (do
              (log (str "IF *" (args_based 0) " != 0; JMP *" (args_based 1)))
              (recur mem
                    (if (zero? (args_ref 0))
                      nxt_meta
                      (assoc nxt_meta :ip (args_ref 1)))))
          6 (do
              (log (str "IF *" (args_based 0) " == 0; JMP *" (args_based 1)))
              (recur mem
                    (if (zero? (args_ref 0))
                      (assoc nxt_meta :ip (args_ref 1))
                      nxt_meta)))
          7 (do
              (log (str "*" (args_based 2) " = *" (args_based 0) " < *" (args_based 1)))
              (recur
               (assoc mem (args_based 2)
                          (if (< (args_ref 0) (args_ref 1))
                            1 0))
               nxt_meta))
          8 (do
              (log (str "*" (args_based 2) " = *" (args_based 0) " == *" (args_based 1)))
              (recur
               (assoc mem (args_based 2)
                          (if (= (args_ref 0) (args_ref 1))
                            1 0))
               nxt_meta))
          9 (do
              (log (str "__BASE__ = *" (args_based 0)))
              (recur mem
                    (update nxt_meta :base #(+ % (args_ref 0)))))
          99 (do
               (log "")
               mem))))))
