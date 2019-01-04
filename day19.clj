(ns day17
  (:require [clojure.string :as s]
            [clojure.set :as set]))

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

(defn to-long [s] (Long/parseLong s))

(defn read-op [line]
  (let [[opcode & args] (s/split line #" ")]
    (->> args
         (map to-long)
         (cons (ops (keyword opcode)))
         vec)))

(defn read-program
  [[ip-line & text]]
  (let [ip-ref (to-long (subs ip-line 4))
        program (vec (map read-op text))]
    [ip-ref program]))

(defn read-file
  [filename]
  (s/split (slurp filename) #"\n"))


(defn star
  [text]
  (let [[ip-ref program] (read-program text)]
    (loop [regs [0 0 0 0 0 0]]
      (let [ip (get regs ip-ref)
            [op a b c :as instr] (get program ip)]
        (if (nil? instr)
          (first regs)
          (let [next-regs (op instr regs)]
            (recur (update next-regs ip-ref inc))))))))

(defn factors
  [n]
  (let [end (inc (long (Math/sqrt n)))]
    (->> (range 1 end 2)
         (filter #(zero? (mod n %)))
         (mapcat (fn [v] [v (/ n v)])))))

(defn star2
  [text]
  (let [[ip-ref program] (read-program text)]
    (loop [regs [1 0 0 0 0 0] cntr 0]
      (let [ip (get regs ip-ref)
            [op a b c :as instr] (get program ip)]
        (if (= ip 1)
          (apply + (factors (get regs 5)))
          (let [next-regs (op instr regs)]
            (recur (update next-regs ip-ref inc) (inc cntr))))))))


(println (star (read-file "input19.txt")))
(println (star2 (read-file "input19.txt")))
