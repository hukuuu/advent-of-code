(ns advent-of-code.day12
  (:require [clojure.string :as str]))

(defn build-map [paths]
  (let [all-paths (concat paths (mapv reverse paths))]
    (reduce #(update %1 (first %2) (fnil conj []) (last %2)) {} all-paths)))

(defn small? [cave]
  (> (int (first cave)) 90))

(defn end? [cave]
  (= cave "end"))

(defn start? [cave]
  (= cave "start"))

(defn traverse [m visited cave]
  (cond
    (not (nil? (visited cave)))
    false

    (end? cave)
    true

    (empty? (m cave))
    false

    :else (mapv (partial traverse m (if (small? cave) (conj visited cave) visited)) (m cave))))

(defn has [v el]
  (< -1 (.indexOf v el)))

(defn traverse2 [m visited twice cave]
  (cond

    (end? cave)
    true

    (empty? (m cave))
    false

    (not (small? cave))
    (mapv (partial traverse2 m (conj visited cave) twice) (m cave))

    (has visited cave)
    (if (or (start? cave) twice)
      false
      (mapv (partial traverse2 m (conj visited cave) true) (m cave)))

    :else
    (mapv (partial traverse2 m (conj visited cave) twice) (m cave))))

(defn part-one [input]
  (let [m (build-map input)
        paths (traverse m #{} "start")]
    (->> paths
         (flatten)
         (filter true?)
         (count))))

(defn part-two [input]
  (let [m (build-map input)
        paths (traverse2 m [] false "start")]
    (->> paths
         (flatten)
         (filter true?)
         (count))))


(def input
  (->> "resources/day12.txt"
       (slurp)
       (#(str/split % #"\n"))
       (mapv #(str/split % #"-"))))

input
(comment
  (build-map input)
  (part-one input)
  (part-two input))



