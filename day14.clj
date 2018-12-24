(ns day14)

(defn star
  [ip]
  (let [end (+ 10 ip)]
    (loop [a 0 b 1 recipes [3 7]]
      (let [a-val (nth recipes a)
            b-val (nth recipes b)
            total (+ a-val b-val)
            recipes' (if (> total 9)
                       (conj (conj recipes 1) (mod total 10))
                       (conj recipes total))
            len-recipes (count recipes')]
        (if (>= len-recipes end)
          (apply str
                 (if (= len-recipes end)
                   (take-last 10 recipes')
                   (butlast (take-last 11 recipes'))))
          (recur (mod (+ 1 a-val a) len-recipes)
                 (mod (+ 1 b-val b) len-recipes)
                 recipes'))))))

(defn star2
  [ip]
  (let [digits (into [] (map #(- (long %) (long \0)) (str ip)))
        last-digit (last digits)
        nr-digits (count digits)]
    (loop [a 0 b 1 recipes [3 7]]
      (let [a-val (nth recipes a)
            b-val (nth recipes b)
            total (+ a-val b-val)
            total_ (mod total 10)
            recipes' (if (> total 9)
                       (conj (conj recipes 1) total_)
                       (conj recipes total))
            len-recipes (count recipes')]
        (if (and (= total_ last-digit)
                 (= (subvec recipes' (max 0 (- (count recipes') nr-digits))) digits))
          (- (count recipes') nr-digits)
          (if (and (> total 9)
                   (= (subvec recipes'
                              (max 0 (- (count recipes') nr-digits 1))
                              (dec (count recipes')))
                      digits))
            (- (count recipes') nr-digits 1)
            (recur (mod (+ 1 a-val a) len-recipes)
                   (mod (+ 1 b-val b) len-recipes)
                   recipes')))))))


(println (time (star 607331)))

(println (time (star2 607331)))


