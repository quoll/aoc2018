(ns day8
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(defn get-data [input-file]
  (->> (s/split (slurp input-file) #"\s")
       (map #(Long/parseLong %))))

(defn build-node
  [[nr-kids md-count & data]]
  (let [[kids remaining] (reduce (fn [[children lst] _]
                                   (let [[n l] (build-node lst)]
                                     [(conj children n) l]))
                                 [[] data]
                                 (range nr-kids))
        md (take md-count remaining)]
    [{:children kids :metadata md} (drop md-count remaining)]))

(defn sum-md [{:keys [children metadata]}]
  (apply + (apply + metadata) (map sum-md children)))

(defn star
  [input-file]
  (let [data (get-data input-file)
        [tree _] (build-node data)]
    (sum-md tree)))

(defn node-value [{:keys [children metadata]}]
  (if-not (seq children)
    (apply + metadata)
    (apply + (map (fn [i]
                    (let [ni (dec i)]
                      (if (and (>= ni 0) (< ni (count children)))
                          (node-value (nth children ni))
                          0)))
                  metadata))))

(defn star2
  [input-file]
  (let [data (get-data input-file)
        [tree _] (build-node data)]
    (node-value tree)))

(println (star "input8.txt"))
(println (star2 "input8.txt"))
