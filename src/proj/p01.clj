(ns proj.p01
  (:require [clojure.string :as string]))

(defn fuel-req [m]
  (let [res (- (unchecked-divide-int m 3)
               2)]
    (if (> res
           0)
      res
      0)))

(defn s []
  (let [ip (map
             read-string
             (filter #(not (empty? %))
               (string/split-lines (slurp "p01.txt"))))
        module_req (reduce +
                           (map fuel-req
                                ip))
        fuel_req (loop [last module_req
                        added 0]
                   (let [more (fuel-req last)]
                     (println more)
                     (if (= more 0)
                       added
                       (recur more (+ added more)))))
        total (+ module_req fuel_req)]
    total))

(defn s_single [m]
  (let [module_req (fuel-req m)
        fuel_req (loop [last module_req
                        added 0]
                   (let [more (fuel-req last)]
                     (println more)
                     (if (= more 0)
                       added
                       (recur more (+ added more)))))
        total (+ module_req fuel_req)]
    total))

(defn s2 []
  (let [ip (map
             read-string
             (filter #(not (empty? %))
                     (string/split-lines (slurp "p01.txt"))))
        total (reduce +
                      (map s_single ip))]
    total))