(ns advent-of-code.day7
  (:require [clojure.string :as str]))

(def input
  (->> "resources/day7.txt"
       slurp
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn increasing-consumption [distance]
  (.intValue (* (inc distance) (/ distance 2))))

(defn total-consumption [consumption target positions]
  (reduce #(+ %1 (consumption (Math/abs (- %2 target)))) 0 positions))

(defn brute-force [input consumption]
  (let [sorted (sort input)]
    (->> (range (first sorted) (inc (last sorted)))
         (map  #(total-consumption consumption % input))
         (map-indexed #(vector  (inc %1) %2))
         (sort #(- (last %1) (last %2)))
         (first)
         (second))))

;; doesnt work
;; (defn part-one [input]
;;   (loop [in input]
;;     (if (= 1 (count in))
;;       (first in)
;;       (let [high (reduce max in)
;;             low (reduce min in)
;;             half (.intValue (Math/ceil (/ (- high low) 2)))
;;             candidates ((juxt filter remove) #(<= % half) in)
;;             cluster (if (> (count (first candidates))
;;                            (count (second candidates)))
;;                       (first candidates)
;;                       (last candidates))]
;;         ;; (println input high low half candidates cluster)
;;         (if (= high low)
;;           (first in)
;;           (recur cluster))))))

(defn part-one [input]
  (brute-force input identity))

(defn part-two [input]
  (brute-force input increasing-consumption))

(comment
  (part-one input)
  (part-two input))








