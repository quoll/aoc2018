(ns day3
  "Run using deps.edn. e.g.
   $ clj day3.clj"
  (:require [clojure.string :refer [split]]
            [clojure.core.matrix :refer :all]))
(set-current-implementation :ndarray)

(defn lines [input-file]
  (split (slurp input-file) #"\n"))

(defn as-long [s] (Long/parseLong s))

(defn destruct [s]
  (let [[all & params] (re-find #"#(\S+) @ (\d+),(\d+): (\d+)x(\d+)" s)]
    (when all (map as-long params))))

(defn star
  [input-file]
  (let [ll (lines input-file)
        field (new-matrix 1000 1000)]
    (loop [[[id col row w h] & xlines] (map destruct ll)]
      (if-not id
        field
        (let [sm (submatrix field row h col w)]
          (emap! #(if (zero? %) id -1) sm)
          (recur xlines))))
    (ereduce + (eq field -1))))


(defn star2
  [input-file]
  (let [ll (lines input-file)
        field (new-matrix 1000 1000)]
    (loop [[[id col row w h] & xlines] (map destruct ll) ids #{}]
      (if-not id
        (first ids)
        (let [sm (submatrix field row h col w)
              ids' (ereduce #(if (zero? %2) %1 (disj %1 %2 id)) (conj ids id) sm)]
          (assign! sm id)
          (recur xlines ids'))))))


(println (star "input3.txt"))
(println (star2 "input3.txt"))

