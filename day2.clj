(ns day2
  (:require [clojure.string :refer [split]]))

(defn lines [input-file]
  (split (slurp input-file) #"\n"))

(defn nums [s]
  (let [cs (->> s
                (group-by identity)
                vals
                (map count))]
    [(some #(= 2 %) cs) (some #(= 3 %) cs)]))

(defn star
  [input-file]
  (let [tt (->> (lines input-file)
                (map nums))]
    (* (count (filter first tt)) (count (filter second tt)))))

(defn close [a b]
  (let [sim (filter identity (map #(when (= %1 %2) %1) a b))]
    (when (= (count sim) (dec (count a))) sim)))

(defn cmp-close [ll]
  (loop [[f & fr] ll]
    (when (seq fr)
      (or
        (loop [[s & sr] fr]
          (when s
            (or (close f s)
                (recur sr))))
        (recur fr)))))

(defn star2
  [input-file]
  (let [ll (lines input-file)]
    (apply str (cmp-close ll))))


(println (star "input2.txt"))
(println (star2 "input2.txt"))

