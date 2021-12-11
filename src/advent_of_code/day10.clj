(ns advent-of-code.day10
  (:require [clojure.string :as str]))

(def input
  (-> "resources/day10.txt"
      (slurp)
      (str/split #"\n")))

(def match-token {\) \( \] \[ \} \{ \> \< \( \) \[ \] \{ \} \< \>})

(def opent? (partial contains? #{\( \[ \{ \<}))

(defn synt-score [corrupted]
  ({\) 3 \] 57 \} 1197 \> 25137} corrupted))

(defn first-corrupted [line]
  (loop [i 0
         stack []]
    (if (= i (count line))
      nil
      (let [token (nth line i)]
        (if (opent? token)
          (recur (inc i) (conj stack token))
          (if (= (last stack)
                 (match-token token))
            (recur (inc i) (into [] (butlast stack)))
            token))))))

(defn part-one [input]
  (->> input
       (map first-corrupted)
       (remove nil?)
       (map synt-score)
       (reduce +)))

(defn completion-score [completion]
  (reduce
   (fn
     [score c]
     (->> score
          (* 5)
          (+ ({\) 1 \] 2 \} 3 \> 4} c))))
   0
   completion))

(defn clean [line]
  (loop [i 0
         stack []]
    (if (= i (count line))
      stack
      (let [token (nth line i)]
        (recur
         (inc i)
         (if (opent? token)
           (conj stack token)
           (into [] (butlast stack))))))))

(defn complete [line]
  (loop [stack (into [] line)
         completion []]
    (if (empty? stack)
      completion
      (recur (into [] (butlast stack)) (conj completion (match-token (last stack)))))))

(defn part-two [input]
  (->> input
       (filter #(nil? (first-corrupted %)))
       (map clean)
       (map complete)
       (map completion-score)
       (sort)
       (#(nth % (-> (count %) (/ 2) (.intValue))))))

(comment
  (part-one input)
  (part-two input)
  (complete "[<><")
  (complete "[({(<(())[]>[[{[]{<()<>>")
  (clean "[({(<(())[]>[[{[]{<()<>>")
  (clean "<><<>")
  (synt-score "[({(<(())[]>[[{[]{<()<>>")
  (synt-score "{[]")
  (conj [] 1 2 3)
  (into [] "123")
  (-> 7 (/ 2) (.intValue))
  (.intValue (Math/floor (/ 7 2))))


