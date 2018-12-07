(ns day7
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer :all]))

(defn get-data [input-file]
  (->> (s/split (slurp input-file) #"\n")
       (map (fn [l] [(nth l 5) (nth l 36)]))))

(defn star
  [input-file]
  (let [data (get-data input-file)
        graph (->> data
                   (group-by first)
                   (map (fn [[k v]] [k (sort (map second v))]))
                   (into {}))
        depends (->> data
                     (group-by second)
                     (map (fn [[k v]] [k (sort (map first v))]))
                     (into {}))
        outs (set (map second data))
        ready (sort (set/difference (set (keys graph)) outs))]
    (apply
     str
     (loop [output [] [n & queue] ready]
       (if-not n
         output
         (let [out (conj output n)]
           (recur out
                  (->> (graph n)
                       (filter #(every? (set out) (depends %)))
                       (concat queue) sort dedupe))))))))

(def workers 5)
(def base-time 61)

(defn add-set [s v] (if s (conj s v) #{v}))

(defn star2
  [input-file]
  (let [data (get-data input-file)
        graph (->> data
                   (group-by first)
                   (map (fn [[k v]] [k (sort (map second v))]))
                   (into {}))
        depends (->> data
                     (group-by second)
                     (map (fn [[k v]] [k (sort (map first v))]))
                     (into {}))
        outs (set (map second data))
        ready (sort (set/difference (set (keys graph)) outs))]
    (loop [output [] in-progress {} tm 0 queue ready]
      (if (and (empty? queue) (empty? in-progress))
        tm
        (let [out (concat output (in-progress tm))
              nready (->> (in-progress tm) ;; completed
                          (mapcat graph)   ;; next to do
                          set
                          (filter #(every? (set out) (depends %)))
                          sort
                          dedupe)
              next-queue (concat queue nready)
              next-in-progress (dissoc in-progress tm)
              nr-free (- workers (apply + (map count (vals next-in-progress))))
              [next-in-progress next-queue] (reduce
                                             (fn [[nip [nq & rq] :as state] _]
                                               (if nq
                                                 [(update nip
                                                          (+ tm base-time (- (int nq) (int \A)))
                                                          add-set nq)
                                                  rq]
                                                 state))
                                             [next-in-progress next-queue]
                                             (range nr-free))]
          (recur out next-in-progress (if (seq next-in-progress) (apply min (keys next-in-progress)) tm) next-queue))))))

(println (star "input7.txt"))
(println (star2 "input7.txt"))
