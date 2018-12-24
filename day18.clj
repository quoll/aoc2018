(ns day18
  (:require [clojure.string :as s]))

(def ^:dynamic *max-x*)
(def ^:dynamic *max-y*)

(defn read-map
  [text]
  (into [] (map vec (s/split text #"\n"))))

(defn print-map
  [f]
  (doseq [row f]
    (println (apply str row))))

(def neighbors
  (memoize
   (fn [[y x]]
     (for [ry (range (max 0 (dec y)) (min *max-y* (+ 2 y)))
           rx (range (max 0 (dec x)) (min *max-x* (+ 2 x)))
           :when (or (not= y ry) (not= x rx))]
       [ry rx]))))

(defn acre-change
  [field coord ch]
  (let [adjacent (neighbors coord)
        adjacent-contents (map #(get-in field %) adjacent)]
    (case ch
      \. (let [trees (filter #(= \| %) adjacent-contents)]
           (if (>= (count trees) 3) \| \.))
      \| (let [lumber (filter #(= \# %) adjacent-contents)]
           (if (>= (count lumber) 3) \# \|))
      \# (let [lumber (filter #(= \# %) adjacent-contents)
               trees (filter #(= \| %) adjacent-contents)]
           (if (and (> (count lumber) 0) (> (count trees) 0)) \# \.))
      )))

(defn map-matrix-indexed
  [f matrix]
  (vec (map-indexed (fn [y row] (vec (map-indexed (fn [x ch] (f [y x] ch)) row))) matrix)))

(defn reduce-matrix
  [f init matrix]
  (reduce (fn [acc row] (reduce f acc row)) init matrix))

(defn update-state
  [field]
  (map-matrix-indexed (partial acre-change field) field))

(defn resource-value
  [text minutes]
  (let [field (read-map text)]
    (binding [*max-x* (count (first field))
              *max-y* (count field)]
      (let [generations (iterate update-state field)
            minutes-gen (nth generations minutes)]
        (* (reduce-matrix #(if (= \| %2) (inc %1) %1) 0 minutes-gen)
           (reduce-matrix #(if (= \# %2) (inc %1) %1) 0 minutes-gen))))))

(defn print-add
  [old new]
  (if old
    (let [r (conj old new)] (println r) r)
    [new]))

(defn stable-resource-value
  [text minutes]
  (let [field (read-map text)]
    (binding [*max-x* (count (first field))
              *max-y* (count field)]
      (let [generations (iterate update-state field)
            cycle-map (loop [[g1 & gr] generations gen-nr 0 gen-map {}]
                        (let [iters (gen-map g1 [])]
                          (if (>= (count iters) 2)
                            gen-map
                            (recur gr (inc gen-nr) (assoc gen-map g1 (conj iters gen-nr))))))
            cycle-map (into {} (filter #(= 2 (count (second %))) cycle-map))
            cycle-length (count cycle-map)
            final-mod (mod 1000000000 cycle-length)
            [[result [n1 n2]]] (filter #(= final-mod (mod (first (second %)) cycle-length)) cycle-map)]
            (* (reduce-matrix #(if (= \| %2) (inc %1) %1) 0 result)
               (reduce-matrix #(if (= \# %2) (inc %1) %1) 0 result))))))

(defn star
  [text]
  (resource-value text 10))

(defn star2
  [text]
  (stable-resource-value text 1000000000))

(println (star (slurp "input18.txt")))
(println (star2 (slurp "input18.txt")))
