(ns advent-of-code.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (->>  "resources/day8.txt"
        (slurp)
        (#(str/split % #"\n"))
        (mapv #(str/split % #" \| "))
        (mapv (fn [s] (mapv #(str/split % #" ") s)))))

(defn one? [digit] (= 2 (count digit)))
(defn four? [digit] (= 4 (count digit)))
(defn seven? [digit] (= 3 (count digit)))
(defn eight? [digit] (= 7 (count digit)))

(defn has [string sub]
  (every? #(not (nil? (str/index-of string %))) sub))

(defn find-five [input s56]
  (let [res (first (filter #(and (= 5 (count %)) (has % s56)) input))]
    res))

(defn find-two [input s2]
  (first (filter #(and (= 5 (count %)) (not (has % s2))) input)))

(defn find-six [input s1]
  (first (filter #(and (= 6 (count %)) (not (has % s1))) input)))

(defn as-set [string]
  (into #{} (str/split string #"")))

(defn subtract [d1 d2]
  (let [s1 (as-set d1)
        s2 (as-set d2)]
    (reduce str (set/difference s1 s2))))

(defn intersection [d1 d2]
  (let [s1 (as-set d1)
        s2 (as-set d2)]
    (reduce str (set/intersection s1 s2))))

(defn sort-digit [digit]
  (reduce str (sort digit)))

(defn find-digits [input]
  (let [d1 (first (filter one? input))
        d4 (first (filter four? input))
        d7 (first (filter seven? input))
        d8 (first (filter eight? input))
        s0 (subtract d7 d1)
        s56 (subtract d4 d1)
        d5 (find-five input s56)
        s2 (intersection d5 d1)
        s1 (subtract d1 s2)
        d6 (find-six input s1)
        s4 (subtract d6 d5)
        d9 (find-six input s4)
        s3 (subtract (subtract d9 d4) s0)
        d2 (find-two input s2)
        s5 (subtract (subtract d8 d2) s2)
        s6 (subtract (subtract d4 d1) s5)
        d3 (find-five input d1)
        d0 (subtract d8 s6)]
    {(sort-digit d1) 1
     (sort-digit d2) 2
     (sort-digit d3) 3
     (sort-digit d4) 4
     (sort-digit d5) 5
     (sort-digit d6) 6
     (sort-digit d7) 7
     (sort-digit d8) 8
     (sort-digit d9) 9
     (sort-digit d0) 0}))

(defn decode-digit [input]
  (let [digits (find-digits (first input))]
    (Integer/parseInt (str/join (map #(get digits (sort-digit %)) (last input))))))

(comment
  (->> input
       (map decode-digit)
       (reduce +)))