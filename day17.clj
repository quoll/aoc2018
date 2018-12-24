(ns day17
  (:require [clojure.string :as s]))

(defn to-long [s] (Long/parseLong s))

(defn line->data
  [line]
  (let [[[_ first-num low-range high-range]] (re-seq #"[xy]=(\d+), [xy]=(\d+)\.\.(\d+)" line)
        f (to-long first-num)
        coord (if (= \x (first line)) (fn [v] (vector v f)) (fn [v] (vector f v)))]
    (for [r (range (to-long low-range) (inc (to-long high-range)))] (coord r))))

(defn load-scan
  [text]
  (->> (s/split text #"\n")
       (mapcat line->data)))

(def ^:dynamic *min-y*)
(def ^:dynamic *max-y*)

(defn print-scan
  ([coords] (print-scan coords nil))
  ([coords err]
   (println)
   (let [xs (map second (keys coords))
         min-x (apply min xs)
         max-x (apply max xs)]
     (doseq [y (range *min-y* (inc *max-y*))]
       (println (apply str (map (fn [x]
                                  (let [c [y x]]
                                    (if (= err c)
                                      \@
                                      (get coords [y x] \.))))
                                (range min-x (inc max-x)))))))))

(defn fill-char
  [rendering [y x]]
  (let [start (loop [left (dec x)] (if (= \~ (get rendering [y left])) (recur (dec left)) (inc left)))
        end (loop [right (inc x)] (if (= \~ (get rendering [y right])) (recur (inc right)) right))]
    (reduce #(assoc %1 %2 \|) rendering (for [rx (range start end)] [y rx]))))

(declare fill)

(defn flood-side
  "Fill to the side defined by dir. Return the next filled rendering state, and whether the area is flooded"
  [dir rendering [y x :as coord]]
  (loop [rendering rendering [ny nx :as next-coord] [y (+ x dir)]]
    ;(print-scan rendering next-coord)
    (let [next-square (get rendering next-coord)
          under-next (get rendering [(inc ny) nx])]
      (case next-square
        \| [rendering (#{\~ \#} under-next)]
        (\# \~) [rendering true]
        (case under-next
          (\# \~) (recur (assoc rendering next-coord \~) [ny (+ nx dir)])
          \| [rendering false]
          (let [next-rendering (assoc rendering next-coord \|)
                [fill-rendering filled?] (fill next-rendering next-coord)]
            (if filled?
              [fill-rendering true]
              [fill-rendering false])))))))

(defn fill
  "fill down. Return the next filled rendering state, and whether the area is flooded"
  [rendering start-coord]
  (let [flood-left (partial flood-side -1)
        flood-right (partial flood-side 1)]
    (loop [rendering rendering [y x :as coord] start-coord]
      ;(print-scan rendering coord)
      (let [down [(inc y) x]
            next-square (get rendering down)]
        (case next-square
          \| [rendering false]                            ;; fell into falling water
          (\# \~) (let [[left-rendering fill-left?] (flood-left rendering coord)
                        [filled-rendering fill-right?] (flood-right left-rendering coord)]
                    (if (and fill-left? fill-right?)
                      (let [up [(dec y) x]
                            up-square (get filled-rendering up)
                            fill-up-rendering (assoc filled-rendering coord \~)]
                        ;(print-scan fill-up-rendering)
                        (if (= up-square \|)
                          (recur fill-up-rendering up)
                          [fill-up-rendering true]))
                      (let [top-rendering (fill-char filled-rendering coord)]
                        [top-rendering false])))
          (if (> (first down) *max-y*)
            [rendering false]
            (recur (assoc rendering down \|) down)))))))

(defn count-water
  [rendering]
  (reduce-kv #(if-not (= \# %3) (inc %1) %1) 0 rendering))

(defn count-standing-water
  [rendering]
  (reduce-kv #(if (= \~ %3) (inc %1) %1) 0 rendering))

(defn run-reservior
  [text]
  (let [the-scan (load-scan text)
        ys (map first the-scan)]
    (binding [*max-y* (apply max ys)
              *min-y* (apply min ys)]
      (let [rendering (reduce #(assoc %1 %2 \#) {} the-scan)
            start [*min-y* 500]
            rendering (assoc rendering start \+)
            [filled _] (fill rendering start)]
        ;(print-scan filled)
        filled))))

(defn star
  [text]
  (count-water (run-reservior text)))

(defn star2
  [text]
  (count-standing-water (run-reservior text)))


(println (star (slurp "input17.txt")))
(println (star2 (slurp "input17.txt")))




