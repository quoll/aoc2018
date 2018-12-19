(ns day16
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn to-long [s] (Long/parseLong s))

(defn read-op [line] (vec (map to-long (s/split line #" "))))

(defn parse-sample
  [[before operation after _]]
  {:before (read-string (subs before 8))
   :operation (read-op operation)
   :after (read-string (subs after 8))})

(defn read-samples
  [text]
  (let [sample-texts (take-while #(s/starts-with? (first %) "Before:") (partition 4 text))]
    (map parse-sample sample-texts)))

(defn read-program
  [text]
  (->> (map vector (drop 1 text) text)
       (drop-while (fn [[l1 l2]] (not= "" l1 l2)))
       (map first)
       (drop-while empty?)
       (map read-op)
       (map vec)))

(defn read-file
  [filename]
  (s/split (slurp filename) #"\n"))


(def ops
  {:addr (fn [[op a b c] regs] (assoc regs c (+ (nth regs a) (nth regs b))))
   :addi (fn [[op a b c] regs] (assoc regs c (+ (nth regs a) b)))

   :mulr (fn [[op a b c] regs] (assoc regs c (* (nth regs a) (nth regs b))))
   :muli (fn [[op a b c] regs] (assoc regs c (* (nth regs a) b)))

   :banr (fn [[op a b c] regs] (assoc regs c (bit-and (nth regs a) (nth regs b))))
   :bani (fn [[op a b c] regs] (assoc regs c (bit-and (nth regs a) b)))

   :borr (fn [[op a b c] regs] (assoc regs c (bit-or (nth regs a) (nth regs b))))
   :bori (fn [[op a b c] regs] (assoc regs c (bit-or (nth regs a) b)))

   :setr (fn [[op a b c] regs] (assoc regs c (nth regs a)))
   :seti (fn [[op a b c] regs] (assoc regs c a))

   :gtir (fn [[op a b c] regs] (assoc regs c (if (> a (nth regs b)) 1 0)))
   :gtri (fn [[op a b c] regs] (assoc regs c (if (> (nth regs a) b) 1 0)))
   :gtrr (fn [[op a b c] regs] (assoc regs c (if (> (nth regs a) (nth regs b)) 1 0)))

   :eqir (fn [[op a b c] regs] (assoc regs c (if (= a (nth regs b)) 1 0)))
   :eqri (fn [[op a b c] regs] (assoc regs c (if (= (nth regs a) b) 1 0)))
   :eqrr (fn [[op a b c] regs] (assoc regs c (if (= (nth regs a) (nth regs b)) 1 0)))})

(defn trial-ops
  [{:keys [before operation after]}]
  (->> (vals ops)
       (map #(% operation before))
       (filter #(= after %))
       count))

(defn star
  [text]
  (->> (read-samples text)
       (map trial-ops)
       (filter #(>= % 3))
       count))

(defn add-to
  [m k v]
  (update m k #(if % (conj % v) #{v})))

(defn extract-singles
  [multimap]
  (reduce (fn [[sng mm] [k v]]
            (let [c (count v)]
              (case c
                0 [sng (dissoc mm k)]
                1 [(assoc sng k (first v)) (dissoc mm k)]
                [sng mm])))
          [{} multimap] multimap))

(defn reverse-mm
  [mm]
  (reduce (fn [acc [k v]] (add-to acc k v)) {} (for [k (keys mm) v (mm k)] [v k])))

(defn reduce-redundancies
  [[singles multimap]]
  (let [[singles' multimap'] (extract-singles multimap)
        reverse-map (reverse-mm multimap')
        singles (into singles singles')
        minimized-reversed-map (apply dissoc reverse-map (vals singles))

        [reverse-singles minimized-reversed-map'] (extract-singles minimized-reversed-map)
        singles (into singles (map (fn [[k v]] [v k]) reverse-singles))
        rebuilt-multimap (reverse-mm minimized-reversed-map)
        next-multimap (apply dissoc rebuilt-multimap (keys singles))]
    [singles next-multimap]))

(defn intersect
  [s1 s2]
  (if (nil? s1) s2 (set/intersection s1 s2)))

(defn speculate
  [speculations {:keys [before operation after]}]
  (let [possible-ops (keep (fn [[op-name op-fn]]
                             (when (= after (op-fn operation before)) op-name))
                           ops)]
    (update speculations (first operation) intersect (set possible-ops))))

(defn execute
  [opmap program regs]
  (reduce (fn [rg [op-code a b c :as instruction]]
            ((ops (opmap op-code)) instruction rg))
          regs program))

(defn star2
  [text]
  (let [samples (read-samples text)
        speculations (reduce speculate {} samples)
        reducing-generations (iterate reduce-redundancies [{} speculations])
        [op-map _] (->> reducing-generations
                        (drop-while #(seq (second %)))
                        first)
        program (read-program text)]
    (first (execute op-map program [0 0 0 0]))))

(println (star (read-file "input16.txt")))
(println (star2 (read-file "input16.txt")))
