(ns day4
  (:require [clojure.string :refer [split]]))

(defn lines [input-file]
  (split (slurp input-file) #"\n"))

(defn as-long [s] (Long/parseLong s))

(def build-table
  (memoize (fn [input-file]
             (loop [[line & xlines] (sort (lines input-file)) guard 0 table {}]
               (if-not line
                 table
                 (if (= "Guard" (subs line 19 24))
                   (recur xlines (-> (subs line 26) (split #" ") first as-long) table)
                   (let [sleep (as-long (subs line 15 17))
                         wake (as-long (subs (first xlines) 15 17))
                         table' (reduce (fn [t minute]
                                          (update-in t [guard minute] #(if % (inc %) 1)))
                                        table (range sleep wake))]
                     (recur (rest xlines) guard table'))))))))

(defn max-finder
  [vfn]
  (fn [[_ mv :as mkv] [_ v :as kv]] (if (> (vfn v) (vfn mv)) kv mkv)))

(defn star
  [input-file]
  (let [table (build-table input-file)
        [guard times] (reduce (max-finder count) [0 []] table)
        [max-time _] (reduce (max-finder identity) [0 0] times)]
    (* guard max-time)))

(defn star2
  [input-file]
  (let [table (build-table input-file)
        ident-finder (max-finder identity)
        guards-maxes (map (fn [[g tm]] [g (reduce ident-finder [0 0] tm)]) table)
        [guard [minute _]] (reduce (max-finder second) [0 [0 0]] guards-maxes)]
    (* guard minute)))

(println (time (star "input4.txt")))
(println (time (star2 "input4.txt")))
