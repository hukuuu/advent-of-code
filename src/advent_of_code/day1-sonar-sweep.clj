(ns advent-of-code.day1
  (:gen-class)
  (:require [clojure.java.io :as io]))

(def demo-input [199 200 208 210 200 207 240 269 260 263])

(def input
  (map #(Integer/parseInt %)  (line-seq (io/reader (io/resource "day1.txt")))))

(defn increasing? [[a b]] (> b a))

(defn part-one [input]
  (->> (partition 2 1 input)
       (filter increasing?)
       count))

(defn part-two [input]
  (->> (partition 3 1 input)
       (map #(apply + %))
       (partition 2 1)
       (filter increasing?)
       count))

(part-one input)

(part-two input)