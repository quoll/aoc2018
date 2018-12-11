(ns day11
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def serial 1788)

(defn hundreds
  [^long n]
  (long (/ (mod n 1000) 100)))

(def power
  (memoize
   (fn [x y]
     (let [rack (+ x 10)]
       (- (hundreds (* rack (+ serial (* y rack)))) 5)))))

(def power-vslice
  (memoize
   (fn [len x y]
     (let [ll (dec len)]
       (if (zero? ll)
         (power x y)
         (+ (power-vslice ll x y) (power x (+ y ll))))))))

(def power-hslice
  (memoize
   (fn [len x y]
     (let [ll (dec len)]
       (if (zero? ll)
         (power x y)
         (+ (power-hslice ll x y) (power (+ x ll) y)))))))

(def power-at
  (memoize
   (fn [sz x y]
     (case sz
       1 (power x y)
       2 (let [xx (inc x) yy (inc y)]
           (+ (power x y) (power xx y) (power x yy) (power xx yy)))
       (if (even? sz)
         (let [block (/ sz 2)]
           (+ (power-at block x y) (power-at block (+ x block) y)
              (power-at block x (+ y block)) (power-at block (+ x block) (+ y block))))
         (let [dsz (dec sz)]
           (+ (power-at dsz x y)
              (power-vslice sz (+ dsz x) y)
              (power-hslice dsz x (+ dsz y)))))))))

(defn max-square
  [sz]
  (let [power-at-sz (partial power-at sz)
        extent (- 301 sz)
        grid (for [y (range 1 extent) x (range 1 extent)] [x y])
        all-powers (map (fn [[x y :as coord]] [coord (power-at-sz x y)]) grid)]
    (reduce (fn [[maxpoint maxpower :as maxes] [_ pwr :as point-pwr]]
              (if (> pwr maxpower) point-pwr maxes))
            [nil -1]
            all-powers)))

(defn star
  []
  (let [[[mx my] _] (max-square 3)]
    (str mx "," my)))

(defn star2
  []
  (let [max-squares (pmap #(vector (max-square %) %) (range 1 301))
        [[mx my] size pwr] (reduce (fn [[mpoint msize mpower :as maxes] [[pt p :as ms] sz]]
                                     (if (> p mpower) [pt sz p] maxes))
                                   [nil 0 -1]
                                   (take-while ffirst max-squares))]
    (str mx "," my "," size)))

(println (star))
(println (star2))

(shutdown-agents)
