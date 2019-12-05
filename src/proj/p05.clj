(ns proj.p05
  (:require [clojure.string :refer [split split-lines]]))

()

(def insns
  {1  {:ac   3
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)
                     a3 (nth args 2)]
                 [(assoc mem a3 (+ a1 a2)) nil]))}
   2  {:ac   3
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)
                     a3 (nth args 2)]
                 [(assoc mem a3 (* a1 a2)) nil]))}
   3  {:ac   1
       :code (fn [mem [a] _]
               (println "inputting 5")
               [(assoc mem a 5) nil])}

   4  {:ac   1
       :code (fn [mem [a] [m]]
               (let [a (if (= m \0)
                         (mem a) a)]
                 (println a)
                 [mem nil]))}

   5  {:ac   2
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)]
                 (if (zero? a1)
                   [mem nil]
                   [mem a2])))}

   6  {:ac   2
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)]
                 (if (zero? a1)
                   [mem a2]
                   [mem nil])))}

   7  {:ac   3
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)
                     a3 (nth args 2)]
                 [(assoc mem a3 (if (< a1 a2) 1 0))
                  nil]))}

   8  {:ac   3
       :code (fn [mem args modes]
               (let [[a1 a2]
                     (map
                       #(if (= %2 \0)
                          (mem %1) %1)
                       args modes)
                     a3 (nth args 2)]
                 [(assoc mem a3 (if (= a1 a2) 1 0))
                  nil]))}})

(def fsck {0 \0
           1 \1})

(defn run [prog]
  (loop [mem prog
         ip 0]
    (let [op_full (mem ip)
          op (mod op_full 100)
          modes (concat
            (reverse (str (unchecked-divide-int op_full 100)))
            (repeat \0))]
      (if (= op 99)
        mem
        (let [insn_entry (insns op)
              ac (insn_entry :ac)
              modes (take ac modes)
              impl (insn_entry :code)
              nxt_ip (+ ip ac 1)
              args (subvec mem (inc ip) nxt_ip)
              [res impl_ip] (impl mem args modes)
              nxt_ip (or impl_ip
                         nxt_ip)]
          (recur res nxt_ip))))))

(def prog (mapv
            read-string
            (filter #(not (empty? %))
                    (split (slurp "p05.txt") #","))))


