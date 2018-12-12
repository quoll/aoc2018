(ns day12
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]))

(defn data [f]
  (let [text (s/split (slurp f) #"\n")
        init (subs (first text) 15)
        map-data (map (fn [s] [(subs s 0 5) (nth s 9)]) (drop 2 text))]
    [init (into {} map-data)]))

(defn gen
  [data mapping]
  (let [data' (str "...." data "....")
        result (apply str
                      (map
                       (fn [n]
                         (mapping (subs data' n (+ 5 n)) \.))
                       (range (+ 4 (count data)))))]
    (subs result 0 (inc (s/last-index-of result \#)))))

(defn calc
  [input-file ending]
  (let [[init mapping] (data input-file)
        [final offset] (loop [data init g 0 offset 0 previous "" offset-gradient 0]
                         (cond (= ending g) [data offset]
                               (= data previous) [data (+ offset (* offset-gradient (- ending g)))]
                               :default
                               (let [fh (s/index-of data \#)
                                     offset' (+ fh offset)
                                     data' (subs data fh)]
                                 (recur (gen data' mapping) (inc g) (- offset' 2) data (- fh 2)))))]
    (->> final
         (map-indexed #(vector (+ %1 offset) %2))
         (filter #(= \# (second %)))
         (map first)
         (apply +))))


(defn star
  [input-file]
  (calc input-file 20))

(defn star2
  [input-file]
  (calc input-file 50000000000))

(println (time (star "input12.txt")))
(println (time (star2 "input12.txt")))
