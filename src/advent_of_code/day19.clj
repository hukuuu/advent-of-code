(ns advent-of-code.day19
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> input
       (#(str/replace % #"--- scanner \d+ ---\n" ""))
       (#(str/split % #"\n\n"))
       (mapv
        (fn [beacons]
          (->> beacons
               (str/split-lines)
               (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #",")))))))))

(defn combine [f]
  (fn [a b]
    (mapv #(apply f %) (partition 2 (interleave a b)))))
    
(def add (combine +))
(def subtract (combine -))

(defn zmap [fn a b]
  (->> (interleave a b)
       (partition 2)
       (mapv #(apply fn %))))

;; props to tonsky
(defn rotations [scanner]
  (reduce
   (fn [acc f] (conj acc [f (mapv f scanner)]))
   []
   [(fn [p] [(p 0)     (p 1)      (p 2)])
    (fn [p] [(p 0) (- (p 2))     (p 1)])
    (fn [p] [(p 0) (- (p 1))  (- (p 2))])
    (fn [p] [(p 0)     (p 2)  (- (p 1))])

    (fn [p] [(- (p 0))     (p 1)   (- (p 2))])
    (fn [p] [(- (p 0))     (p 2)      (p 1)])
    (fn [p] [(- (p 0)) (- (p 1))      (p 2)])
    (fn [p] [(- (p 0)) (- (p 2))  (- (p 1))])

    (fn [p] [(p 1) (- (p 0))      (p 2)])
    (fn [p] [(p 1) (- (p 2))  (- (p 0))])
    (fn [p] [(p 1)     (p 0)  (- (p 2))])
    (fn [p] [(p 1)     (p 2)      (p 0)])

    (fn [p] [(- (p 1))     (p 0)      (p 2)])
    (fn [p] [(- (p 1)) (- (p 2))      (p 0)])
    (fn [p] [(- (p 1)) (- (p 0))  (- (p 2))])
    (fn [p] [(- (p 1))     (p 2)  (- (p 0))])

    (fn [p] [(p 2)     (p 1)  (- (p 0))])
    (fn [p] [(p 2)     (p 0)     (p 1)])
    (fn [p] [(p 2) (- (p 1))     (p 0)])
    (fn [p] [(p 2) (- (p 0))  (- (p 1))])

    (fn [p] [(- (p 2))     (p 1)     (p 0)])
    (fn [p] [(- (p 2)) (- (p 0))     (p 1)])
    (fn [p] [(- (p 2)) (- (p 1))  (- (p 0))])
    (fn [p] [(- (p 2))     (p 0)  (- (p 1))])]))

(defn find-origin [s1 s2]
  (->>
   (rotations s2) ;; rotate s2 to all 24 positions
   (mapv (fn [[rot-fn pts]] (for [p1 s1 p2 pts] (vector rot-fn (zmap - p1 p2))))) ;; combinations
   (mapv #(last (sort-by second (frequencies %))))
   (sort-by second)
   (last)
   ((fn [[origin count]] (if (> count 11) origin nil)))))

(defn transform [s [rot-fn origin]]
  (mapv #(add origin (rot-fn %)) s))

(defn transforms [scanners]
  (let [combinations
        (for [i (range (count scanners))
              j (range (count scanners))
              :when (not= i j)]
          [i j (nth scanners i) (nth scanners j)])]
    (reduce
     (fn [acc [i j s1 s2]]
       (let [origin (find-origin s1 s2)]
         (if (nil? origin)
           acc
            (assoc-in acc [i j]  origin))))
     {}
     combinations)))

(defn gen-paths [tmap c]
  (loop [known {0 [[identity [0 0 0]]]}
         [j & rest] (range 1 c)]
    (if (nil? j)
      known
      (let [k (first (filter #(not (nil? (get-in tmap [(key %) j]))) known))]
        (if (nil? k)
          (recur known (conj (vec rest) j))
          (let [[i origins] k
                orig (get-in tmap [i j])]
            (recur (assoc known j (conj origins orig)) rest)))))))

(defn apply-transforms [transforms scanner]
  (reduce
   (fn [scanner t]
     (transform scanner t))
   scanner
   (reverse transforms)))

(defn part-one [scanners]
  (let [tmap (transforms scanners)
        paths (gen-paths tmap (count scanners))
        points (for [i (range (count scanners))]
                 (apply-transforms (paths i) (nth scanners i)))
        unique (reduce into #{} points)]
    (count unique)))
  
(defn distance [a b]
  (reduce + (map #(Math/abs %) (subtract a b))))
  
(defn part-two [scanners]
  (let [paths (gen-paths (transforms scanners) (count scanners))
        origins (mapv (fn [path]
                        (->> path
                          (val)
                          (reverse)
                          (reduce transform [[0 0 0]])
                          (first)))
                      paths)
        distances (for [i (range (count origins))
                        j (range (count origins))
                        :when (< i j)]
                    (distance (nth origins i) (nth origins j)))]
    (->> distances sort last)))

(def input (slurp "resources/day19.txt"))

(comment

  (time (part-one (parse input))) ;; 21 sec ¯\_(ツ)_/¯

  (part-two (parse input)))
  
  
