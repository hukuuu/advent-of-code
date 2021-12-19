(ns advent-of-code.day15
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn parse [input]
  (->> input
       (#(str/split % #"\n"))
       (mapv (fn [line] (mapv #(Character/digit % 10) line)))))

(defn neighbours [board [x y]]
  (let [height (count board)
        width (count (first board))
        all [[(dec x) y]
             [(inc x) y]
             [x (dec y)]
             [x (inc y)]]]
    (filterv
     (fn [[x y]]
       (and
        (< -1 x height)
        (< -1 y width)))
     all)))

(defn path [board]
  (let [target [(dec (count board)) (dec (count (first board)))]]
    (loop [open  [{:coord [0 0] :cost 0 :parent nil}]
           visited #{}]
      (if (empty? open)
        nil
        (let [node (first open)]
          (cond

            (visited (:coord node))
            (recur (rest open) visited)

            (= target (:coord node))
            node

            :else
            (let [nbs (remove visited (neighbours board (:coord node)))
                  candidates (map (fn [c] {:coord c :parent node :cost (+ (:cost node) (get-in board c))}) nbs)]
              (recur (sort-by :cost (concat (rest open) candidates)) (conj visited (:coord node))))))))))

(defn inc-risk [v]
  (let [v' (inc v)]
    (if (= v' 10)
      1
      v')))

(defn scale-x [row]
  (vec (apply concat (take 5 (iterate #(mapv inc-risk %) row)))))

(defn scale [board]
  (let [board' (mapv scale-x board)]
    (vec (apply concat (take 5 (iterate #(mapv (fn [row] (mapv inc-risk row)) %) board'))))))

(def input (slurp "resources/day15.txt"))

(comment
  (pprint (parse input))
  (pprint (scale-x (parse input)))
  (:cost (path (scale (parse input)))))
