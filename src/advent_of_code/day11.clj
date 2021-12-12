(ns advent-of-code.day11
  (:require [clojure.string :as str]
            [advent-of-code.utils :refer [log]]))


(def input
  (-> "resources/day11.txt"
      (slurp)
      (str/split #"\n")
      (->>
       (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #"")))))))

(defn increment
  ([grid] (mapv #(mapv inc %) grid))
  ([grid pts m] (reduce #(update-in %1 %2 (fn [v] (if (> v 10) (inc v) (min (inc v) m)))) grid pts)))

(defn neighbours [grid pts]
  (reduce
   (fn [res, [x y]]
     (concat res
             (for [i [-1 0 1]
                   j [-1 0 1]
                   :let [x' (+ x i)
                         y' (+ y j)]
                   :when (not (and (= i 0) (= j 0)))
                   :when (< -1 x' (count grid))
                   :when (< -1 y' (count (first grid)))]
               [x' y']))) [] pts))

(defn find-in [grid pred]
  (for [i (range (count grid))
        j (range (count (first grid)))
        :when (pred (get-in grid [i j]))]
    [i j]))

(defn just-flashed [grid]
  (find-in grid #(= 10 %)))

(defn all-flashed [grid]
  (find-in grid #(> % 9)))

(defn reset-flashed [grid]
  (let [flashed (all-flashed grid)
        grid' (reduce #(update-in %1 %2 (fn [_] 0)) grid flashed)]
    [grid' (count flashed)]))

(defn reset-flashed2 [grid]
  (let [flashed (all-flashed grid)]
    (reduce #(update-in %1 %2 (fn [_] 0)) grid flashed)))

(defn cascade [grid]
  (loop [g grid]
    (let [flashed (just-flashed g)
          g' (increment g flashed 11)]
      (if (empty? flashed)
        g'
        (recur (increment g' (neighbours g' flashed) 10))))))

(defn step [[grid flashed]]
  (let [[grid' fd] (->> grid
                        (increment)
                        (cascade)
                        (reset-flashed))]
    [grid' (+ flashed fd)]))

(def step2 (comp reset-flashed2 cascade increment))

(defn all-flashed? [grid]
  (every? zero?
          (for [i (range (count grid))
                j (range (count (first grid)))]
            (get-in grid [i j]))))

(defn part-two [input]
  (loop [i 0
         g input]
    (if (all-flashed? g)
      i
      (recur (inc i) (step2 g)))))

(comment
  (last (last (take 101 (iterate step [input 0]))))
  (part-two input))





