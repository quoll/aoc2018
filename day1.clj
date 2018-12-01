(ns day1
  (:require [clojure.string :refer [split]]))

(defn star
  [input-file]
  (as-> input-file d
    (slurp d)
    (split d #"\n")
    (map #(Integer/parseInt %) d)
    (apply + d)))

(defn star2
  [input-file]
  (as-> input-file d
    (slurp d)
    (split d #"\n")
    (map #(Integer/parseInt %) d)
    (loop [[n & rn] (cycle d) f 0 acc #{}]
      (let [f (+ f n)]
        (if (acc f)
          f
          (recur rn f (conj acc f)))))))


(println (star "input1.txt"))
(println (star2 "input1.txt"))

