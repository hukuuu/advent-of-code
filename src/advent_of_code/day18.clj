(ns advent-of-code.day18
  (:require [clojure.zip :as z]
            [clojure.string :as str]))

(defn find-first [loc pred]
  (loop [l loc]
    (cond

      (z/end? l)
      nil

      (pred l)
      l

      :else
      (recur (z/next l)))))

(defn first-right [loc]
  (if (nil? loc)
    nil
    (let [right (z/right loc)]
      (if (nil? right)
        (first-right (z/up loc))
        (find-first right (comp number? z/node))))))

(defn nested-pair? [loc]
  (let [[_ {pnodes :pnodes}] loc]
    (and
     (= 4 (count pnodes))
     (not (number? (z/node loc))))))

(defn find-last [loc pred]
  (if (number? loc)
    loc
    (if (z/branch? loc)
      (find-last (z/right (z/down loc)) pred)
      loc)))

(defn first-left [loc]
  (if (nil? loc)
    nil
    (let [left (z/left loc)]
      (if (nil? left)
        (first-left (z/up loc))
        (find-last left (comp number? z/node))))))

(defn explode [n]
  (let [root (z/vector-zip n)
        deep (find-first root nested-pair?)]
    (if (nil? deep)
      n
      (let [[[l r] deep] ((juxt z/children #(z/replace % 0)) deep)
            left (first-left deep)
            deep (if (nil? left) deep (z/edit left + l))
            right (first-right (if (nil? left) deep (first-right deep)))
            deep (if (nil? right) deep (z/edit right + r))]
        (z/root deep)))))

(explode [[[[0,7],4],[7,[[8,4],9]]],[1,1]])


(defn split? [loc]
  (let [node (z/node loc)]
    (and (number? node) (> node 9))))

(defn split [n]
  (let [loc (z/vector-zip n)
        s (find-first loc split?)]
    (if (nil? s)
      n
      (as-> loc l
        (find-first l split?)
        (z/replace l (as-> l u
                       (z/node u)
                       (/ u 2)
                       ((juxt #(Math/floor %) #(Math/ceil %)) u)
                       (mapv int u)))
        (z/root l)))))

(defn reduce-n [n]
  (loop [current n]
    (let [new (explode current)]
      (if (= new current)
        (let [new (split current)]
          (if (= new current)
            current
            (recur new)))
        (recur new)))))

(defn add [a b]
  (let [res (reduce-n [a b])]
    res))

(defn magnitude [n]
  (if (vector? n)
    (+ (* 3 (magnitude (first n)))
       (* 2 (magnitude (last n))))
    n))

(defn part-one [xs]
  (magnitude (reduce add xs)))

(defn part-two [xs]
  (let [combinations (remove
                      nil?
                      (for [i xs j xs]
                        (if (= i j) nil [i j])))
        all (concat combinations (mapv reverse combinations))]
      (->> all
        (map #(magnitude (add (first %) (last %))))
        (sort)
        (last))))


(defn parse [input]
  (->> input
       (str/split-lines)
       (map read-string)))

(def input (slurp "resources/day18.txt"))

(comment
  (part-one (parse input))
  (part-two (parse input)))
