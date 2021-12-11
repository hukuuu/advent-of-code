(ns advent-of-code.day9
  (:require [clojure.string :as str]
            [clojure.set :refer [union]]))

(def input
  (->> "resources/day9.txt"
       (slurp)
       (#(str/split % #"\n"))
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [%] (Integer/parseInt %)) %))))

(defn getm [matrix [x y]]
  (nth (nth matrix x) y))

(defn coords [matrix]
  (for [i (range (count matrix))
        j (range (count (first matrix)))]
    [i j]))

(defn ncoords [matrix [x y]]
  (let [max-x (count matrix)
        max-y (count (first matrix))]
    (->>
     [[(inc x) y]
      [(dec x) y]
      [x (inc y)]
      [x (dec y)]]
     (filterv
      (fn [[x y]] (and (< -1 x max-x) (< -1 y max-y)))))))

(defn neighbours [matrix [x y]]
  (->>
   (ncoords matrix [x y])
   (mapv (partial getm matrix))))

(defn low-pt? [matrix [x y]]
  (let [n (getm matrix [x y])
        neighbours (neighbours matrix [x y])]
    (every? #(> % n) neighbours)))

(defn low-pts [matrix]
  (->>
   matrix
   (coords)
   (filter (partial low-pt? matrix))))

(defn part-one [input]
  (->>
   input
   (low-pts)
   (mapv (partial getm input))
   (reduce (partial + 1) 0)))


(defn increasing? [matrix pt1 pt2]
  (and (< (getm matrix pt2) 9)
       (> (getm matrix pt2) (getm matrix pt1))))

(defn increasing-neighbours [matrix pt]
  (->>
   pt
   (mapcat (fn [pt] (mapv #(vector pt %) (ncoords matrix pt))))
   (filterv #(increasing? matrix (first %) (last %)))
   (map last)
   (into #{})))

(defn basin [matrix pt]
  (loop [acc #{pt} candidates [pt]]
    (if (empty? candidates)
      acc
      (let [in (increasing-neighbours matrix candidates)]
        (recur (union acc in) in)))))

(defn part-two [input]
  (->>
   input
   (low-pts)
   (map (partial basin input))
   (map count)
   (sort)
   (reverse)
   (take 3)
   (reduce *)))

(comment
  (part-one input)
  (part-two input))


