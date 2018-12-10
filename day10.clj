(ns day10
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def re #"position=< *([-\d]+), *([-\d]+)> velocity=< *([-\d]+), *([-\d]+)>")

(defn get-data
  [input-file]
  (letfn [(line-fn [l] (->> (re-seq re l) first rest (map #(Long/parseLong %))))]
    (map line-fn (s/split (slurp input-file) #"\n"))))

(defn render
  [coords [minx miny width height]]
  (let [blank (into [] (repeat height (into [] (repeat width \.))))
        field (reduce (fn [field [x y]] (assoc-in field [(- y miny) (- x minx)] \#)) blank coords)]
    (s/join "\n" (map (partial apply str) field))))
 
(defn dims
  [coords]
  (let [xs (map first coords)
        ys (map second coords)
        minx (apply min xs)
        maxx (apply max xs)
        miny (apply min ys)
        maxy (apply max ys)
        width (inc (- maxx minx))
        height (inc (- maxy miny))]
    [minx miny width height]))

(defn area
  [[_ _ width height]]
  (* width height))

(defn coordinates
  [data t]
  (map (fn [[x y xv yv]] [(+ x (* t xv)) (+ y (* t yv))]) data))

(defn find-time
  [data]
  (let [[[_ y1 _ y1v] [_ y2 _ y2v]] data
        estimated-t (Math/abs (long (/ (- (Math/abs (- y1 y2)) 10) (- y1v y2v))))
        c0 (coordinates data estimated-t)
        d0 (dims c0)
        c1 (coordinates data (inc estimated-t))
        d1 (dims c1)
        [dir start-time start-coords start-dims] (if (< (area d0) (area d1))
                                                   [-1 estimated-t c0 d0]
                                                   [1 (inc estimated-t) c1 d1])]
    (loop [t start-time last-coords start-coords last-dims start-dims]
      (let [nt (+ t dir)
            coords (coordinates data nt)
            ds (dims coords)]
        (if (> (area ds) (area last-dims))
          [t (render last-coords last-dims)]
          (recur nt coords ds))))))

(defn star
  [input]
  (let [data (get-data input)]
    (second (find-time data))))

(defn star2
  [input]
  (let [data (get-data input)]
    (first (find-time data))))

(println (star "input10.txt"))
(println (star2 "input10.txt"))
