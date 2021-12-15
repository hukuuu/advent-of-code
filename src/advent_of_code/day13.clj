(ns advent-of-code.day13
  (:require [clojure.string :as str]))

(defn parse-paper [input]
  (->> input
       (#(str/split % #"\n\n"))
       (first)
       (#(str/split % #"\n"))
       (mapv #(str/split % #","))
       (mapv (fn [v] (mapv #(Integer/parseInt %) v)))
       (mapv (comp vec reverse))))

(defn parse-instructions [input]
  (->> input
       (#(str/split % #"\n\n"))
       (last)
       (#(str/split % #"\n"))
       (map #(last (str/split % #" ")))
       (map #(str/split % #"="))
       (mapv #(vector (first %) (Integer/parseInt (last %))))))

(defn find-frame [paper]
  (->> paper
       (reduce #(vector (max (first %1) (first %2)) (max (last %1) (last %2))))
       (#(vector (inc (first %)) (inc (last %))))))

(defn empty-paper [height width]
  (into [] (repeatedly height #(into [] (repeat width ".")))))

(defn print-paper [paper]
  (let [[height width] (find-frame paper)
        p (empty-paper height width)
        updated (reduce #(assoc-in %1 %2 "#") p paper)]
    (str/join "\n" (map #(str/join "" %) updated))))

(defn fold-y [paper axis]
  (map
   (fn [[y x]]
     (if (> y axis)
       [(- axis (- y axis)) x]
       [y x]))
   paper))

(defn fold-x [paper axis]
  (map
   (fn [[y x]]
     (if (> x axis)
       [y (- axis (- x axis))]
       [y x]))
   paper))

(defn fold [paper [axis val]]
  (if (= "x" axis)
    (fold-x paper val)
    (fold-y paper val)))

(defn count-dots [paper]
  (count (reduce #(conj %1 (str/join "-" %2)) #{} paper)))

(defn part-one [paper instruction]
  (count-dots (fold paper instruction)))

(defn part-two [paper instructions]
  (reduce fold paper instructions))

(def input
  (->> "resources/day13.txt"
       (slurp)))

(comment
  (part-one (parse-paper input) (first (parse-instructions input)))
  (spit "resources/13-answer" (print-paper (part-two (parse-paper input) (parse-instructions input)))))



