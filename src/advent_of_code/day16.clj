(ns advent-of-code.day16
  (:require [clojure.string :as str]))

(defn as-binary [hex]
  (-> hex (Integer/parseInt 16) (Integer/toBinaryString) (Integer/parseInt) (->> (format "%04d"))))

(defn as-int [binary]
  (Long/parseLong binary 2))

(defn parse [input]
  (str/join (map (comp as-binary str) input)))

(defn read-header [at msg]
  (let [end (+ at 3)]
    [(-> msg (subs at end) (as-int)) end]))

(defn read-number [at msg]
  (loop [pos at bytes ""]
    (let [next (inc pos)
          end (+ pos 5)
          continue (subs msg pos next)]
      (if (= "0" continue)
        [(as-int (str bytes (subs msg next end))) end]
        (recur end (str bytes (subs msg next end)))))))

(defn read-int [at end msg]
  [(as-int (subs msg at end)) end])

(defn read-packet [at msg]
  (let [[version i] (read-header at msg)
        [type i] (read-header i msg)]
    (cond

      (= type 4)
      (let [[number i] (read-number i msg)]
        {:ver version :typ type :val number :next i})

      :else
      (let [[ltype i] (read-int i (inc i) msg)]
        (case ltype

          0
          (let [[total i] (read-int i (+ 15 i) msg)
                end (+ i total)
                packets (loop [i i
                               packets []]
                          (if (< i end)
                            (let [p (read-packet i msg)]
                              (recur (:next p) (conj packets p)))
                            packets))]
            {:ver version :typ type :val packets :next end})

          1
          (let [[npackets i] (read-int i (min (dec (count msg)) (+ 11 i)) msg)
                [packets i] (loop [n 0
                                   i i
                                   packets []]
                              (if (< n npackets)
                                (let [p (read-packet i msg)]
                                  (recur (inc n) (:next p) (conj packets p)))
                                [packets i]))]
            {:ver version :typ type :val packets :next i}))))))

(def input (slurp "resources/day16.txt"))

(defn count-version [packet]
  (+ (:ver packet) (if (= 4 (:typ packet)) 0 (reduce + (map count-version (:val packet))))))

(defn eval-packet [packet]
  (case (:typ packet)
    0
    (reduce + (map eval-packet (:val packet)))

    1
    (reduce * (map eval-packet (:val packet)))

    2
    (reduce min (map eval-packet (:val packet)))

    3
    (reduce max (map eval-packet (:val packet)))

    4
    (:val packet)

    5
    (let [[p1 p2] (:val packet)]
      (if (> (eval-packet p1) (eval-packet p2)) 1 0))

    6
    (let [[p1 p2] (:val packet)]
      (if (< (eval-packet p1) (eval-packet p2)) 1 0))

    7
    (let [[p1 p2] (:val packet)]
      (if (= (eval-packet p1) (eval-packet p2)) 1 0))))


(comment

  (count-version (read-packet 0 (parse input)))
  (eval-packet (read-packet 0 (parse input))))



