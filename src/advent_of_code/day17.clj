(ns advent-of-code.day17
  (:require [clojure.string :as string]))

(def input (->> "resources/day17.txt"
                (slurp)))

(defn parse [input]
  (let [[_ x1 x2 y1 y2] (re-find #"x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" input)]
    (mapv #(Integer/parseInt %) [x1 x2 y1 y2])))

(defn draw [trajectory [x1 x2 y1 y2] margin]
  (let [yabs (Math/abs y1)
        height (* margin yabs)
        ymarg (* (dec margin) yabs)
        board (vec
               (map-indexed
                (fn [i _]
                  (vec
                   (map-indexed
                    (fn [j _]
                      (if (and
                           (and (>= i (+ ymarg (Math/abs y2))) (<= i (+ ymarg (Math/abs y1))))
                           (and (>= j x1) (< j x2)))
                        "T"
                        "."))
                    (range  x2))))
                (range (inc height))))
        board-tr (reduce (fn [b [x y]] (if (> y ymarg) b (assoc-in b [(+ ymarg (* -1 y)) x] "#"))) board trajectory)]
    (doseq [line board-tr]
      (prn (string/join "" line)))))

(defn in-bounds? [[x y] [x1 x2 y1 y2]]
  (and (<= x x2) (>= y y1)))

(defn hit? [[x y] [x1 x2 y1 y2]]
  (and
   (>= x x1)
   (<= x x2)
   (>= y y1)
   (<= y y2)))

(defn drag [x]
  (cond
    (= x 0) 0
    (> x 0) (dec x)
    :else (inc x)))

(defn step [[[px py] [vx vy]]]
  [[(+ px vx) (+ py vy)] [(drag vx) (dec vy)] (max py (+ py vy))])

(defn traj [probe target]
  (take-while #(in-bounds? (first %) target) (iterate step probe)))

(defn sim [coords target]
  (let [trajectory (traj [[0 0] coords 0] target)
        height (reduce max (map last trajectory))]
    height))
    ;; (prn height)
    ;; (prn trajectory)
    ;; (draw tr target 8)))

(defn part-one [input]
  ;; We can use a trick here.
  ;; We dont care about X at all, we just want to find the maximum Y.
  ;; No matter what Y we choose, it will always hit 0, because we start at 0.
  ;; e.g. x=0,y=3 -> 00, 03, 05, 06, 06, 05, 03, 00 -> its mirrored on Y axis.
  ;; Next Y will be exactly -(y+1) (e.g. -4) If this is deeper than tne target, we will never hit it.
  ;; Converseley, the maximum hight will be achieved by the last Y that doesnt skip the target -> the targetg depth - 1
  (let [target (parse input)]
    (sim [5 (dec (Math/abs (nth target 2)))] target)))

(defn sumof [n]
  (* (inc n) (/ n 2)))

(defn sim2 [coords target]
  (let [trajectory (traj [[0 0] coords 0] target)]
    trajectory))

(defn part-two [input]
  (let [target (parse input)
        [x1 x2 y1 y2] target
        xs (remove #(<= (sumof %) x1) (range (inc x2)))
        ys (range y1 (* -1 y1))
        all (for [x xs
                  y ys]
              [x y])]
    (count (filter (fn [vel] (hit? (first (last (sim2 vel target))) target)) all))))

(comment
  (part-one input)
  (part-two input))
