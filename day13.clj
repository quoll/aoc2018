(ns day13
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]))

(def left [-1 0])
(def right [1 0])
(def down [0 1])
(def up [0 -1])

(def left-turn {left down, right up, up left, down right})
(def right-turn {left up, right down, up right, down left})
(def straight-turn identity)
(def turn-rotations {left-turn straight-turn, straight-turn right-turn, right-turn left-turn})

(def dir-fns
  {\/ #(update % :dir {right up, left down, up right, down left})
   \\ #(update % :dir {right down, left up, up left, down right})
   \+ (fn [{:keys [turn-state] :as train}]
        (-> train
            (update :dir turn-state)
            (update :turn-state turn-rotations)))})

(def train-dir
  {\> right
   \< left
   \^ up
   \v down})

(defn read-data
  [input-file]
  (let [lines (s/split (slurp input-file) #"\n")
        track-line-fn (fn [y line]
                        (keep-indexed (fn [x c]
                                        (if-let [df (dir-fns c)]
                                          [[x y] df])) line))
        tracks (->> lines
                    (keep-indexed track-line-fn)
                    (apply concat)
                    (into {}))
        train-fn (fn [y line]
                   (keep-indexed (fn [x c]
                                   (if-let [td (train-dir c)]
                                     {:id x :dir td :coord [x y] :turn-state left-turn}))
                                 line))
        trains (->> lines
                    (keep-indexed train-fn)
                    (apply concat)
                    (reduce (fn [m {[x y] :coord :as train}]
                              (assoc-in m [y x] train)) {}))]
    [tracks trains]))

(defn move-train
  [{[dx dy] :dir :as train}]
  (update train :coord (fn [[x y]] [(+ dx x) (+ dy y)])))

(defn remove-train
  [trains x y]
  (let [t (update trains y dissoc x)]
    (if (empty? (t y)) (dissoc t y) t)))

(defn move
  [tracks init-trains]
  (loop [[y & ys] (sort (keys init-trains)) all-trains init-trains]
    (if-not y
      [all-trains nil]
      (let [[next-trains crash]
            (loop [[x & xs] (sort (keys (init-trains y))) local-trains all-trains]
              (if-not x
                [local-trains nil]
                (let [train (get-in local-trains [y x])
                      {[tx ty] :coord :as train'} (move-train train)]
                  (if (get-in local-trains [ty tx])
                    [nil [tx ty]]
                    (let [turn (or (tracks [tx ty]) identity)]
                      (recur xs (-> local-trains
                                    (remove-train x y)
                                    (assoc-in [ty tx] (turn train')))))))))]
        (if crash
          [nil crash]
          (recur ys next-trains))))))

(defn star
  [input-file]
  (let [[tracks init-trains] (read-data input-file)
        move (partial move tracks)]
    (s/join ","
            (loop [[trains crash] [init-trains nil] c 0]
              (or crash
                  (recur (move trains) (inc c)))))))

(defn move-clear
  [tracks init-trains]
  (loop [[y & ys] (sort (keys init-trains)) all-trains init-trains]
    (if-not y
      all-trains
      (let [next-trains
            (loop [[x & xs] (sort (keys (init-trains y))) local-trains all-trains]
              (if-not x
                local-trains
                (if-let [train (get-in local-trains [y x])]
                  (let [{[tx ty] :coord :as train'} (move-train train)]
                    (if (get-in local-trains [ty tx])
                      (recur xs (-> local-trains
                                    (remove-train x y)
                                    (remove-train tx ty)))
                      (let [turn (or (tracks [tx ty]) identity)]
                        (recur xs (-> local-trains
                                      (remove-train x y)
                                      (assoc-in [ty tx] (turn train')))))))
                  (recur xs local-trains))))]
        (recur ys next-trains)))))

(defn count-trains
  [trains]
  (apply + (map (fn [[y x->train]] (count x->train)) trains)))

(defn star2
  [input-file]
  (let [[tracks init-trains] (read-data input-file)
        move (partial move-clear tracks)]
    (s/join ","
            (loop [trains init-trains]
              (if (= 1 (count-trains trains))
                (-> trains first second first second :coord)
                (recur (move trains)))))))


(println (time(star "input13.txt")))
(println (time(star2 "input13.txt")))
