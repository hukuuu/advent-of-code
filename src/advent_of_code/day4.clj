(ns advent-of-code.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn log [value]
  (println value)
  value)

(def raw-input (->> "day4.txt"
                    (io/resource)
                    (io/reader)
                    (line-seq)
                    (remove #(zero? (count %)))))

(defn parse-draw [raw]
  (->> raw
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(parse-draw (first raw-input))

(defn parse-boards [raw]
  (->> raw
       (partition 5)
       (map (fn [board]
              (->> board
                   (interpose " ")
                   (reduce str)
                   (#(str/split % #" +"))
                   (remove #(zero? (count %)))
                   (map #(Integer/parseInt %)))))))

;; (parse-boards (rest raw-input))

(defn input [raw-input]
  [(parse-draw (first raw-input)) (parse-boards (rest raw-input))])

;; (input raw-input)

(defn find-row [i]
  (let [offset (* i 5)]
    (range offset (+ offset 5))))

(defn find-col [i]
  (for [n (range 5)]
    (+ i (* 5 n))))

(defn check-all [xs board]
  (every? true?
          (for [i xs]
            (= -1 (nth board i)))))

(defn check-board [board]
  (some true?
        (flatten
         (for [i (range 5)]
           [(check-all (find-row i) board) (check-all (find-col i) board)]))))

(defn get-winner [boards]
  (first (filter check-board boards)))

(defn update-board [n board]
  (map #(if (= % n) -1 %) board))

(defn update-boards [n boards]
  (map #(update-board n %) boards))

(defn get-score [board multiplier]
  (->> board
       (remove #(= % -1))
       (reduce +)
       (* multiplier)))

(defn part-one [[draw boards]]
  (loop [i 0
         bds (update-boards (nth draw i) boards)]
    (let [winner (get-winner bds)]
      (if winner
        (get-score winner (nth draw i))
        (recur (inc i) (update-boards (nth draw (inc i)) bds))))))

(defn remove-winners [boards]
  (remove check-board boards))

(defn part-two [[draw boards]]
  (loop [i 0
         bds (update-boards (nth draw i) boards)]
    (let [losers (remove-winners bds)]
      (if (= 1 (count losers))
        (part-one [draw losers])
        (recur (inc i) (update-boards (nth draw (inc i)) losers))))))

(part-one (input raw-input))
(part-two (input raw-input))
