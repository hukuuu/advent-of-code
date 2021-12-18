(ns advent-of-code.day14
  (:require [clojure.string :as str]))


(defn parse [input]
  (let [[template ins] (str/split input #"\n\n")
        rules  (into {} (mapv #(into [] (rest %)) (re-seq #"([A-Z]+) -> ([A-Z])" ins)))]
    [template rules]))

(defn combine [rules template]
  (->> (partition 2 1 template)
       (map (partial apply str))
       (map (fn [s] (str (first s) (rules s))))
       (#(str (apply str %) (last template)))))

(defn part-one [input n]
  (let [[template rules] (parse input)
        freq (frequencies (nth (iterate (partial combine rules) template) n))
        [most least] (let [ar (map last freq)]
                       [(reduce max ar) (reduce min ar)])]
    (- most least)))

(defn prep [template]
  (->> (partition 2 1 template)
       (map (partial apply str))
       (reduce #(update %1 %2 (fnil inc 0)) {})))

(defn count-letters [solution a b]
  (->>
   (conj
    (mapcat #(vector (vector (first (first %)) (last %)) (vector (last (first %)) (last %))) solution)
    [a 1] [b 1])
   (reduce #(update %1 (first %2) (fn [v] (if (nil? v) (last %2) (+ (last %2) v)))) {})
   (map #(/ (second %) 2))
   (sort)))

(defn step [rules m]
  ;; (prn "step")
  (reduce-kv
   (fn [acc key val]
    ;;  (prn acc key val "->" (str (first key) (rules key)) (str (rules key) (last key)))
     (let [acc' (-> acc
                    (update (str (first key) (rules key)) (fnil + 0) val)
                    (update (str (rules key) (last key)) (fnil + 0) val))]
      ;;  (prn (reverse (sort-by second acc')))
       acc'))
   {} m))

(defn part-two [input n]
  (let [[template rules] (parse input)
        res (nth (iterate #(step rules %) (prep template)) n)
        counted (count-letters res (first template) (last template))]
    (- (last counted) (first counted))))

(def input (slurp "resources/day14.txt"))

(comment

  (part-two input 40)

  ;; doesnt work on big input
  (part-one input 10))

