(ns day9fast
  (:require [clojure.string :as s]
            [clojure.pprint :as pp])
  (:import [java.util LinkedList ListIterator]))

(def re #"(\d+) players; last marble is worth (\d+) points")

(defn get-data
  [input]
  (let [[[all players points]] (re-seq re input)]
    [(Long/parseLong players) (Long/parseLong points)]))

(defn ++ [a b] (if a (+ a b) b))

(defn print-state [player pos c]
  (let [v (.previous pos)
        _ (.next pos)
        d (map #(if (= v %) (str "(" % ")") (str %)) c)]
    (println (str "[" player "] ") (s/join " " d))))

(defn ^ListIterator nextp
  [^LinkedList c ^ListIterator p]
  (if (.hasNext p)
    (do (.next p) p)
    (.listIterator c 1)))

(defn ^ListIterator back7
  [^LinkedList c ^ListIterator p]
  (loop [i 0 position p]
    (if (= i 7)
      position
      (do
        (.previous position)
        (if (.hasPrevious position)
          (recur (inc i) position)
          (recur (inc i) (.listIterator c (count c))))))))

(defn calc
  [players final-points]
  (time
   (let [circle (LinkedList. [0])]
     (loop [position (.listIterator circle 1) pts 1 player 0 scores {}]
       #_(print-state player position circle)
       (if (= pts (inc final-points))
         (apply max (vals scores))
         (if (zero? (mod pts 23))
           (let [position (back7 circle position)
                 removed (.previous position)]
             (.remove position)
             (.next position)
             (recur position
                    (inc pts)
                    (mod (inc player) players)
                    (update scores player ++ (+ pts removed))))
           (let [position (nextp circle position)]
             (.add position pts)
             (recur position
                    (inc pts)
                    (mod (inc player) players)
                    scores))))))))

(defn star
  [input]
  (let [[players last-marble] (get-data input)]
    (calc players last-marble)))

(defn star2
  [input]
  (let [[players last-marble] (get-data input)]
    (calc players (* 100 last-marble))))

#_(println (star "9 players; last marble is worth 25 points"))
#_(println (star "10 players; last marble is worth 1618 points"))
#_(println (star "13 players; last marble is worth 7999 points"))
#_(println (star "17 players; last marble is worth 1104 points"))
#_(println (star "21 players; last marble is worth 6111 points"))
#_(println (star "30 players; last marble is worth 5807 points"))
(println (star (slurp "input9.txt")))
(println (star2 (slurp "input9.txt")))
