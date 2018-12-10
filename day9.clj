(ns day9
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def re #"(\d+) players; last marble is worth (\d+) points")

(defn get-data
  [input]
  (let [[[all players points]] (re-seq re input)]
    [(Long/parseLong players) (Long/parseLong points)]))

(defn ++ [a b] (if a (+ a b) b))

(defn insert-after
  [coll p v]
  (let [pp (inc p)]
    (into [] (concat (take pp coll) [v] (drop pp coll)))))

(defn remove-at
  [coll p]
  [(nth coll p)
   (into [] (concat (take p coll) (drop (inc p) coll)))])

(defn print-state [player pos c]
  (let [d (map-indexed #(if (= pos %1) (str "(" %2 ")") (str %2)) c)]
    (println (str "[" player "] ") (s/join " " d))))

(defn calc
  [players final-points]
  (time
   (loop [circle [0] position 0 pts 1 player 0 scores {}]
     (if (= pts (inc final-points))
       (apply max (vals scores))
       (if (zero? (mod pts 23))
         (let [position' (mod (- position 7) (count circle))
               [removed circle'] (remove-at circle position')]
           (recur circle'
                  position'
                  (inc pts)
                  (inc player)
                  (update scores player ++ (+ pts removed))))
         (let [position' (mod (inc position) (count circle))
               circle' (insert-after circle position' pts)]
           (recur circle'
                  (inc position')
                  (inc pts)
                  (mod (inc player) players)
                  scores)))))))

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
