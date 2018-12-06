(ns day6
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]))

(defn coords [s]
  (mapv #(Long/parseLong %) (s/split s #", ")))

(defn closest
  [pts x y]
  (let [[ids min-dist] (reduce
                        (fn [[mids mind :as mins] id]
                          (let [[idx idy] (pts id)
                                dist (+ (Math/abs (- x idx)) (Math/abs (- y idy)))]
                            (if (< dist mind) [[id] dist]
                                (if (= dist mind) [(conj mids id) dist] mins))))
                        [[] 355] (range (count pts)))]
    (if (= 1 (count ids))
      (first ids)
      -1)))

(defn ninc [x] (if x (inc x) 1))

(defn get-points [input-file]
  (mapv coords (s/split (slurp input-file) #"\n")))

(defn matrix-reduce
  [f s m]
  (->> 
       (reduce f s)))

(defn star
  [input-file]
  (let [points (get-points input-file)
        top-x (+ 2 (apply max (map first points)))
        top-y (+ 2 (apply max (map second points)))
        field (vec (for [y (range top-y)]
                     (vec (for [x (range top-x)]
                            (closest points x y)))))
        ids (as-> (set (range (count points))) ids
              (apply disj ids (first field))
              (apply disj ids (last field))
              (apply disj ids (map first field))
              (apply disj ids (map last field)))]
    (->> (for [y (range top-y) x (range top-x)] ((field y) x))
         (reduce (fn [countmap id] (if (ids id) (update countmap id ninc) countmap)) {})
         vals
         (apply max))))

(defn low-dist
  [pts d p]
  (->> pts
       (map first)
       (map (Math/abs (- )))))

(defn star2
  [input-file]
  (let [points (get-points input-file)
        top-x (+ 2 (apply max (map first points)))
        top-y (+ 2 (apply max (map second points)))
        low-dist? (fn [x y]
                    (> 10000
                       (->> points
                            (map (fn [[px py]] (+ (Math/abs (- x px)) (Math/abs (- y py)))))
                            (apply +))))]
    (->>  (for [y (range top-y) x (range top-x)] (low-dist? x y))
          (filter identity)
          count)))


(println (time(star "input6.txt")))
(println (time (star2 "input6.txt")))
