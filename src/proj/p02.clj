(ns proj.p02
  (:require [clojure.string :refer [split]]))

(def insns
  {1  {:ac   3
       :code (fn [mem [a1 a2 a3]]
               (println "add %" a1 " %" a2 " -> %" a3)
               (assoc mem a3 (+ (mem a1)
                                (mem a2))))}
   2  {:ac   3
       :code (fn [mem [a1 a2 a3]]
               (println "mul %" a1 " %" a2 " -> %" a3)
               (assoc mem a3 (* (mem a1)
                                (mem a2))))}
   99 {:ac   0
       :code (fn [mem _] [mem])}})

(defn run [prog]
  (loop [mem prog
         ip 0]
    (let [op (mem ip)]
      (if (= op 99)
        mem
        (let [insn_entry (insns op)
              ac (insn_entry :ac)
              impl (insn_entry :code)
              nxt_ip (+ ip ac 1)
              args (subvec mem (inc ip) nxt_ip)]
          (recur (impl mem args)
                 nxt_ip))))))

(def prog (mapv
            read-string
            (filter #(not (empty? %))
                    (split (slurp "p02.txt") #","))))

(def req 19690720)

(defn patch-ip [n v]
  (assoc prog 1 n 2 v))

(defn s []
  (doseq [noun (range 100)
          verb (range 100)]
    (let [fixed_prog (patch-ip noun verb)
          op (first (run fixed_prog))]
      (if (= req op)
        (println (+ (* 100 noun)
                    verb))))))
