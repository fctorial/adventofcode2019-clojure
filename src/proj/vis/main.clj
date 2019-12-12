(ns proj.vis.main
  (:import (java.net ServerSocket)
           (javax.swing JFrame)))

(defn -main [& args]
  (let [fr (new JFrame)
        server (new ServerSocket "5000")
        socket (.accept server)]))
