(ns proj.core)

(defn zip-colls [& cs]
  (partition (count cs)
             (apply interleave cs)))

(defn iter-n [coll n]
  (apply zip-colls
         (map
           #(drop % coll)
           (range n))))

(defn -main [day]
  (let [fl (str "p"
                (if (= 1 (count day))
                      "0" "")
                day)
        namespace (symbol (str "proj." fl))]
    (require namespace)
    (print "part 1:")
    (println (time ((ns-resolve namespace (symbol "p1")))))
    (print "part 2:")
    (println (time ((ns-resolve namespace (symbol "p2")))))))