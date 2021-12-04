(ns advent-of-code.day2
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input (->> ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"]
                     (map #(let [[direction value] (str/split % #" ")]
                             (vector direction (Integer/parseInt value))))))

(def input
  (->> "day2.txt"
       (io/resource)
       (io/reader)
       (line-seq)
       (map #(let [[direction value] (str/split % #" ")]
               (vector direction (Integer/parseInt value))))))

(defn next-position [[horizontal depth] [direction value]]
  (let [new-horizontal (if (= direction "forward") (+ horizontal value) horizontal)
        new-depth (case direction
                    "up" (- depth value)
                    "down" (+ depth value)
                    depth)] (vector new-horizontal new-depth)))

(defn part-one [input]
  (->> input
       (reduce next-position [0 0])
       (reduce *)))

(defn next-position-two [[horizontal depth aim] [direction value]]
  (let [new-horizontal (if (= direction "forward") (+ horizontal value) horizontal)
        new-depth (if (= direction "forward") (+ depth (* value aim)) depth)
        new-aim (case direction
                  "up" (- aim value)
                  "down" (+ aim value)
                  aim)]
    (vector new-horizontal new-depth new-aim)))


(defn part-two [input]
  (let [[horizontal depth] (reduce next-position-two [0 0 0] input)]
    (* horizontal depth)))

(part-one input)
(part-two input)

