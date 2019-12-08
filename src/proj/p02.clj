(ns proj.p02
  (:require [clojure.string :refer [split]]
            [proj.comp :refer [run read-prog]]
            [clojure.core.async :refer [<!!]]))

(def prog (read-prog "p02.txt"))

(def req 19690720)

(defn patch-ip [n v]
  (assoc prog 1 n 2 v))

(defn s []
  (first
    (filter (fn [[noun verb]]
              (let [fixed_prog (patch-ip noun verb)
                    op (first (<!! (run fixed_prog nil nil "")))]
                (= req op)))
            (for [noun (range 100)
                  verb (range 100)]
              [noun verb]))))
