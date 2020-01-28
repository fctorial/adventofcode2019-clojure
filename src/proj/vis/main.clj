(ns proj.vis.main
  (:import (java.net ServerSocket)
           (javax.swing JFrame SwingUtilities JPanel)
           (java.awt Graphics Color Dimension)
           (java.awt.image BufferedImage))
  (:require [clojure.core.async :as async :refer [>!! <!! >! <! poll! go go-loop chan close! sliding-buffer]]
            [proj.utils :refer []]
            [clojure.data :refer [diff]]))

(defn _chan->seq [ch]
  (let [val (poll! ch)]
    (if val
      (lazy-seq (cons val (_chan->seq ch))))))

(defn chan->seq [ch]
  (let [val (<!! ch)]
    (if val
      (lazy-seq (cons val (_chan->seq ch))))))

(defn create-window [xres yres bsize]
  (let [w (* xres bsize)
        h (* yres bsize)
        img (atom nil)
        cmder (chan (* xres yres))
        canvas (proxy [JPanel] []
                 (paintComponent [g]
                   (.drawImage g @img 0 0 nil)))
        frame (new JFrame (str xres "x" yres))]

    (.setSize frame w h)
    (.setResizable frame false)
    (.setSize canvas w h)
    (.add frame canvas)
    (.setVisible frame true)
    (reset! img (.createImage canvas w h))

    (go-loop []
      (let [cmds (chan->seq cmder)]
        (doseq [cmd cmds]
          (let [[x y c] cmd]
            (when (and (< -1 x xres)
                       (< -1 y yres))
              (let [g (.getGraphics @img)]
                (.setColor g c)
                (.fillRect g (* x bsize) (* y bsize) bsize bsize)
                (.dispose g)))))
        (.repaint canvas)
        (recur)))
    cmder))

(defn synced-window [xres yres bsize cmap xoff yoff]
  (let [w (* xres bsize)
        h (* yres bsize)
        img (atom nil)
        last-frame (atom {})
        cmder (chan (* xres yres))
        canvas (proxy [JPanel] []
                 (paintComponent [g]
                   (.drawImage g @img 0 0 nil)))
        frame (new JFrame (str xres "x" yres))]

    (.setSize frame w h)
    (.setResizable frame false)
    (.setSize canvas w h)
    (.add frame canvas)
    (.setVisible frame true)
    (reset! img (.createImage canvas w h))

    (go-loop []
      (let [frame (last (chan->seq cmder))
            [changes _ _] (diff frame last-frame)]
        (doseq [[[_x _y] c] changes]
          (let [x (+ _x xoff)
                y (+ _y yoff)]
            (when (and (< -1 x xres)
                       (< -1 y yres))
              (let [g (.getGraphics @img)]
                (.setColor g (cmap c))
                (.fillRect g (* x bsize) (* y bsize) bsize bsize)
                (.dispose g)))))
        (.repaint canvas)
        (recur)))
    cmder))
