(ns day11
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def serial 1788)

(defn hundreds
  [^long n]
  (let [t (* 1000 (long (/ n 1000)))]
    (long (/ (- n t) 100))))

(def power
  (memoize
   (fn [x y]
     (let [rack (+ x 10)]
       (- (hundreds (* rack (+ serial (* y rack)))) 5)))))

(def power-at
  (memoize
   (fn [sz x y]
     (if (< sz 4)
       (apply + (for [yy (range y (+ y sz)) xx (range x (+ x sz))] (power xx yy)))
       (if (even? sz)
         (let [block (/ sz 2)]
           (+ (power-at block x y) (power-at block (+ x block) y)
              (power-at block x (+ y block)) (power-at block (+ x block) (+ y block))))
         (+ (power-at (dec sz) x y)
            (apply + (for [yy (range y (+ y sz))] (power (dec (+ x sz)) yy)))
            (apply + (for [xx (range x (dec (+ x sz)))] (power xx (dec (+ y sz)))))))))))

(defn max-square
  [sz]
  (let [power-at-sz (partial power-at sz)
        extent (- 301 sz)]
    (reduce (fn [[maxpoint maxpower :as maxes] [x y :as point]]
              (let [p (power-at-sz x y)]
                (if (> p maxpower) [point p] maxes)))
            [nil -1]
            (for [y (range 1 extent) x (range 1 extent)] [x y]))))

(defn star
  []
  (let [[[mx my] _] (max-square 3)]
    (str mx "," my)))

(defn star2
  []
  (let [[[mx my] size pwr] (reduce (fn [[mpoint msize mpower :as maxes] sz]
                                     (let [[pt p] (max-square sz)]
                                       (if (> p mpower) [pt sz p] maxes)))
                                   [nil 0 -1]
                                   (range 1 301))]
    (str mx "," my "," size)))

(println (time (star)))
(println (time (star2)))
