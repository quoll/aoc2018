(ns day1
  (:require [clojure.string :refer [split]]))

(defn lines [input-file]
  (split (slurp input-file) #"\n"))

(defn star
  [input-file]
  (->> (lines input-file)
       (map #(Integer/parseInt %))
       (apply +)))

(defn star2
  [input-file]
  (let [numbers (->> (lines input-file) (map #(Integer/parseInt %)))]
    (loop [[n & rn] (cycle numbers) f 0 acc #{}]
      (let [f (+ f n)]
        (if (acc f)
          f
          (recur rn f (conj acc f)))))))

(println (star "input1.txt"))
(println (star2 "input1.txt"))

