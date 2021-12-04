(ns advent-of-code.day3
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input (map #(map (fn [vec] (Integer/parseInt vec)) (str/split % #"")) ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]))

(def input
  (->> "day3.txt"
       (io/resource)
       (io/reader)
       (line-seq)
       (map #(map (fn [vec] (Integer/parseInt vec)) (str/split % #"")))))

(defn make-zeros [n]
  (repeat n 0))

(defn add-vecs [a b]
  (map (fn [[x y]] (+ x y)) (partition 2 (interleave a b))))

(defn gamma-rate [input]
  (let [size (count (first input))]
    (->> input
         (reduce add-vecs (make-zeros size))
         (map #(if (> % (/ (count input) 2)) 1 0)))))

(defn epsilon-rate [input]
  (let [size (count (first input))]
    (->> input
         (reduce add-vecs (make-zeros size))
         (map #(if (< % (/ (count input) 2)) 1 0)))))

(defn vec-to-num [vec]
  (Integer/parseInt (str/join "" vec) 2))

;; (gamma-rate demo-input)
;; (epsilon-rate demo-input)

(defn part-one [input]
  (* (vec-to-num (gamma-rate input)) (vec-to-num (epsilon-rate input))))

(part-one input)

(defn one? [n] (= 1 n))

(defn reduce-by [fn input]
  (loop [items input
         i 0]
    (println i " - " items)
    (if (->> items (count) (= 1))
      (first items)
      (let [[ones zeros] ((juxt filter remove) #(one? (nth % i)) items)]
        (recur
         (fn ones zeros)
         (inc i))))))

(defn co-scrubber-rating [ones zeros]
  (let [nones (count ones)
        nzeros (count zeros)]
    (if (< nones nzeros) ones zeros)))

(defn oxygen-generator-rating [ones zeros]
  (let [nones (count ones)
        nzeros (count zeros)]
    (if (> nones nzeros) ones (if (> nzeros nones) zeros ones))))

(defn part-two [input]
  (let [oxygen (reduce-by oxygen-generator-rating input)
        co2 (reduce-by co-scrubber-rating input)]
    (* (vec-to-num oxygen) (vec-to-num co2))))

(part-two input)




