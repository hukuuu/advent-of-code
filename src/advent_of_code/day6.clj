(ns advent-of-code.day6
  (:require [clojure.string :as str]
            [advent-of-code.utils :refer [log]]))

(def input
  (->> "resources/day6.txt"
       (slurp)
       (#(str/split  % #","))
       (map #(Integer/parseInt %))
       (frequencies)))

;; THIS IS TOO SLOW FOR 256
;; ------------------------
;; (defn evolve [fish]
;;   (dotimes [i (count fish)]
;;     (let [f (.get fish i)]
;;       (.set fish i
;;             (case f
;;               0 (do
;;                   (.add fish 8)
;;                   6)
;;               (dec f))))))

;; (defn part-one [input days]
;;   (let [fish  (java.util.ArrayList. input)]
;;     (dotimes [i days] (evolve fish))
;;     (count fish)))

;; props to Tonsky
(defn progress [fish]
  (reduce-kv
   (fn [acc day count]
     (if (zero? day)
       (-> acc
           (assoc 8 count)
           (update 6 (fnil + 0) count))
       (-> acc
           (update (dec day) (fnil + 0) count))))
   {}
   fish))

(defn part-one [input days]
  (loop [day 0
         fish input]
    (if (>= day days)
      (reduce + (vals fish))
      (recur (inc day) (progress fish)))))

(comment
  (part-one input 80)
  (part-one input 256))


