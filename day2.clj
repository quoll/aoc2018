(ns day2
  (:require [clojure.string :refer [split]]))

(defn lines [input-file]
  (split (slurp input-file) #"\n"))

(defn counts [[two-count three-count] s]
  (let [cs (-> s frequencies vals)]
    [(if (some #(= 2 %) cs) (inc two-count) two-count)
     (if (some #(= 3 %) cs) (inc three-count) three-count)]))

(defn star
  [input-file]
  (let [[two-count three-count] (->> (lines input-file) (reduce counts [0 0]))]
    (* two-count three-count)))

#_(defn nearly= [left right]
  (loop [[l & xl] left [r & xr] right diffs 0]
    (if (nil? l)
      (apply str (filter identity (map #(#{%1} %2) left right)))
      (if (not= l r)
        (when (zero? diffs)
          (recur xl xr 1))
        (recur xl xr diffs)))))

(defn nearly= [left right]
  (let [same (filter identity (map #(#{%1} %2) left right))]
    (when (= (count same) (dec (count left))) (apply str same))))

(defn compare-lines [ll]
  (loop [[line & xlines] ll]
    (when (seq line)
      (or
       (some (partial nearly= line) xlines)
       (recur xlines)))))

(defn star2
  [input-file]
  (let [ll (lines input-file)]
    (compare-lines ll)))


(println (star "input2.txt"))
(println (star2 "input2.txt"))

