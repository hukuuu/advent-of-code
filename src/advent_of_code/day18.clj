(ns advent-of-code.day18
  (:require [clojure.zip :as z]
            [clojure.string :as str]))

(defn find-first [loc pred]
  (loop [l loc]
    ;; (prn l)
    (cond

      (z/end? l)
      nil

      (pred l)
      l

      :else
      (recur (z/next l)))))

(defn first-right [loc]
  ;; (prn "search right")
  (if (nil? loc)
    ;; root reached
    nil
    (let [right (z/right loc)]
      ;; (prn loc right)
      (if (nil? right)
        (first-right (z/up loc))
        (find-first right (comp number? z/node))))))

(defn nested-pair? [loc]
  (let [[_ {pnodes :pnodes}] loc]
    (and
     (= 4 (count pnodes))
     (not (number? (z/node loc))))))

(defn find-last [loc pred]
  ;; TODO - bug?
  (if (number? loc)
    loc
    (if (z/branch? loc)
      (find-last (last (z/children loc)) pred)
      loc)))

(defn first-left [loc]
  ;; (prn "search left")
  (if (nil? loc)
    ;; root reached
    nil
    (let [left (z/left loc)]
      ;; (prn loc left)
      (if (nil? left)
        (first-left (z/up loc))
        (find-last left (comp number? z/node))))))

(defn explode [n]
  ;; (prn "explode" n)
  (let [root (z/vector-zip n)
        deep (find-first root nested-pair?)]
    (if (nil? deep)
      n
      (let [[[l r] deep] ((juxt z/children #(z/replace % 0)) deep)
            ;; _ (prn "res" (z/root deep))
            left (first-left deep)
            ;; _ (prn "left" (z/node left))
            deep (if (nil? left) deep (z/edit left + l))
            ;; _ (prn "res" (z/root deep))
            right (first-right (if (nil? left) deep (first-right deep)))
            ;; _ (prn "right" (z/root right))
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
    (prn res)
    res))

(defn part-one [xs]
  (reduce add xs))

(def input (slurp "resources/day18.txt"))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map read-string)))

(comment
    
  (part-one (parse input))
  (reduce-n [[[[2,[3,5]],[8,7]],[[9,3],2]] [[3,[3,7]],[[3,6],[[1,1],7]]]])

  (let [n [[[[0,7],4],[15,[0,13]]],[1,1]]]
    (split (z/vector-zip n)))

  (reduce-n [[[[[9,8],1],2],3],4])
  (reduce-n [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])

  (explode [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]])

  (explode (explode [[[[[9,8],1],2],3],4]))
  (z/vector-zip [[[[[9,8],1],2],3],4])

  (explode [7,[6,[5,[4,[3,2]]]]])
  (explode [[6,[5,[4,[3,2]]]],1])
  (explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
  (explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]))
