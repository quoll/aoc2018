(ns day5
  (:require [clojure.string :as s]))

(defn opposite
  [a b]
  (= 32 (Math/abs (- (int a) (int b)))))

(defn polymer
  [[f & xf]]
  (when f
    (let [[s & r] xf]
      (if-not s
        [f]
        (do
          (if (opposite f s)
            (recur r)
            (cons f (lazy-seq (polymer xf)))))))))

(defn fixedlength [f s]
  (loop [s s]
    (let [r (f s)]
      (if (= (count r) (count s)) r (recur r)))))

(defn star
  [input-file]
  (let [input (s/replace (slurp input-file) #"\s" "")]
    (count (fixedlength polymer input))))

(defn star2
  [input-file]
  (let [input (apply str (fixedlength polymer (s/replace (slurp input-file) #"\s" "")))]
    (->> (range (int \A) (inc (int \Z)))
         (map #(re-pattern (str \[ (char %) (char (+ 32 %)) \])))
         (map #(s/replace input % ""))
         (map #(count (fixedlength polymer %)))
         (apply min))))


(println (time(star "input5.txt")))
(println (time (star2 "input5.txt")))
