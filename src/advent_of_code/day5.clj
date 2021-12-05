(ns advent-of-code.day5
  (:require [clojure.string :as str]
            [advent-of-code.utils :refer [log]]))

(def input
  (->> "resources/day5.txt"
       (slurp)
       (#(str/split % #"\n"))
       (mapcat #(str/split % #" -> "))
       (mapcat #(str/split % #","))
       (map #(Integer/parseInt %))
       (partition 2)
       (partition 2)))

(defn orthogonal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn get-orthogonal [lines]
  (filter orthogonal? lines))

(defn get-points [[[x1 y1] [x2 y2]]]
  (loop [px x1 py y1 points []]
    (let [pts (conj points [px py])]
      (if (and (= px x2) (= py y2))
        pts
        (recur
         ((if (> px x2) dec (if (< px x2) inc identity)) px)
         ((if (> py y2) dec (if (< py y2) inc identity)) py)
         pts)))))

(defn add-point [cache point]
  (assoc cache point (inc (get cache point 0))))

(defn add-points [points cache]
  (reduce add-point cache points))

(defn part-one [input]
  (->> input
       (get-orthogonal)
       (reduce #(add-points (get-points %2) %1) {})
       (seq)
       (filter #(> (nth % 1) 1))
       (count)))

(defn part-two [input]
  (->> input
       (reduce #(add-points (get-points %2) %1) {})
       (seq)
       (filter #(> (nth % 1) 1))
       (count)))


(part-one input)
(part-two input)

