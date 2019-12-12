(ns proj.core)

(defn -main [day]
  (let [fl (str "p"
                (if (= 1 (count day))
                      "0" "")
                day)
        namespace (symbol (str "proj." fl))]
    (require namespace)
    (println "part 1:")
    (println (time ((ns-resolve namespace (symbol "p1")))))
    (println "part 2:")
    (println (time ((ns-resolve namespace (symbol "p2")))))))