(ns day15
  (:require [clojure.string :as s]))

(defn read-cavern
  [input-file]
  (let [text (vec (s/split (slurp input-file) #"\n"))
        read-line (fn [cavern-map y line]
                    (letfn [(read-char [cm x ch]
                              (case ch
                                \# (update cm :walls conj [y x])
                                \E (assoc-in cm [:elves [y x]] 200)
                                \G (assoc-in cm [:goblins [y x]] 200)
                                cm))]
                      (reduce-kv read-char cavern-map (vec line))))]
    (reduce-kv read-line {:walls #{}} text)))

(defn print-cavern2
  [{:keys [elves goblins walls]}]
  (let [max-x (inc (apply max (map first walls)))
        max-y (inc (apply max (map second walls)))]
    (doseq [y (range max-y)]
      (println (apply str
                      (for [x (range max-x)]
                        (cond
                          (walls [y x]) \#
                          (goblins [y x]) \G
                          (elves [y x]) \E
                          :default \.)))
               (s/join ", "
                       (keep #(if-let [s (goblins [y %])]
                                (str "G(" s ")")
                                (if-let [s (elves [y %])]
                                  (str "E(" s ")"))) (range max-x)))))))

(defn add-step
  [n coords]
  (vec (map (fn [c] [c n]) coords)))

(defn neighbors
  ([[y x]] [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]])
  ([{walls :walls} coord]
   (->> (neighbors coord)
        (remove walls))))

(defn paths-for
  [step-map coord]
  (loop [paths [[coord]] step (step-map coord)]
    (let [nstep (dec step)]
      (if (zero? nstep)
        (set (map (fn [p] [(first p) coord (count p)]) paths))
        (let [next-paths (mapcat (fn [path]
                                   (->> (first path) neighbors (filter #(= nstep (step-map %))) (map #(cons % path))))
                                 paths)]
          (recur next-paths nstep))))))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn find-closest
  [{:keys [elves goblins] :as cavern-map} unit-group coord]
  (let [[opposing allied] (if (= :elves unit-group) [goblins elves] [elves goblins])
        neighboring #(->> (neighbors cavern-map %) (remove allied))
        immediate-neighbors (neighboring coord)
        work-queue (into empty-queue (add-step 1 immediate-neighbors))
        init-step-map (reduce #(assoc %1 %2 1) {coord 0} immediate-neighbors)]
    (when-not (some opposing immediate-neighbors)
      (loop [[[current-coord step] :as queue] work-queue step-map init-step-map paths nil]
        (if (or (empty? queue) (and (seq paths) (> step (nth (first paths) 2))))
          (let [sorted-paths (sort-by second paths)
                selected-tail (second (first sorted-paths))]
            (->> paths
                 (filter #(= selected-tail (second %)))
                 (sort-by first)
                 first))
          (let [next-queue (pop queue)
                surrounding (->> (neighboring current-coord) (remove step-map))]
            (if-not (seq surrounding)
              (recur next-queue step-map paths)
              (if (some opposing surrounding)
                (let [next-step-map (reduce #(assoc %1 %2 (inc step)) step-map (remove opposing surrounding))
                      new-paths (if (or (empty? paths) (= step (nth (first paths) 2)))
                                  (concat (paths-for next-step-map current-coord) paths)
                                  paths)]
                  (recur next-queue next-step-map new-paths))
                (let [next-step-map (reduce #(assoc %1 %2 (inc step)) step-map surrounding)
                      next-work-queue (into next-queue (add-step (inc step) surrounding))]
                  (recur next-work-queue next-step-map paths))))))))))

(defn in-map?
  [{:keys [elves goblins]} coord]
  (or (elves coord) (goblins coord)))

(defn move-attack
  [elf-attack {:keys [elves goblins walls] :as init-map}]
  (let [unit-coords (sort (concat (keys elves) (keys goblins)))
        move-unit (fn [{:keys [elves goblins] :as cavern-map} coord]
                    (let [unit-group (if (elves coord) :elves :goblins)
                          path (find-closest cavern-map unit-group coord)]
                      (if (seq path)
                        (let [unit (get-in cavern-map [unit-group coord])
                              new-coord (first path)]
                          [(-> cavern-map
                                (update unit-group dissoc coord)
                                (assoc-in [unit-group new-coord] unit))
                           new-coord])
                        [cavern-map coord])))
        attack-for-unit (fn [{:keys [elves goblins] :as cavern-map} finished-early? coord]
                          (if finished-early? 
                            [cavern-map finished-early?]
                            (let [opposing-group (if (elves coord) :goblins :elves)
                                  opposing (opposing-group cavern-map)
                                  targets (->> (neighbors init-map coord) (filter opposing))]
                              (if (empty? opposing)
                                [cavern-map true]
                                (if (seq targets)
                                  (let [sorted-targets (sort-by opposing targets)
                                        hp (opposing (first sorted-targets))
                                        target (->> sorted-targets
                                                    (filter #(= hp (opposing %)))
                                                    sort
                                                    first)
                                        new-hp (- hp (if (= :goblins opposing-group) elf-attack 3))]
                                    (if (pos? new-hp)
                                      [(assoc-in cavern-map [opposing-group target] new-hp) false]
                                      [(update cavern-map opposing-group dissoc target) false]))
                                  [cavern-map finished-early?])))))
        composed-actions (fn [[init-map finished? :as result-state] coord]
                           (if (in-map? init-map coord)
                             (let [[next-map next-coord] (move-unit init-map coord)] 
                               (attack-for-unit next-map finished? next-coord))
                             result-state))]
    (reduce composed-actions [init-map false] unit-coords)))

(defn star
  [input-file]
  (let [cavern-map (read-cavern input-file)]
    (loop [{:keys [elves goblins] :as cavern-map} cavern-map round 0]
      ; (println "After" round "rounds")
      ; (print-cavern2 cavern-map)
      (if (or (empty? elves) (empty? goblins))
        (let [survivors (if (empty? elves) goblins elves)]
          (* round (apply + (vals survivors))))
        (let [[next-cavern finished?] (move-attack 3 cavern-map)]
          (recur next-cavern (if finished? round (inc round))))))))

(defn star2
  [input-file]
  (let [initial-map (read-cavern input-file)
        elf-number (count (:elves initial-map))]
    (loop [{:keys [elves goblins] :as cavern-map} initial-map elf-attack 4 round 0]
      (cond
        (not= (count elves) elf-number) (recur initial-map (inc elf-attack) 0)
        (empty? goblins) (do ;(println "After" round "rounds")
                             ;(print-cavern2 cavern-map)
                             ;(println ">>>" elf-attack)
                             (* round (apply + (vals elves))))
        :default (let [[next-cavern finished?] (move-attack elf-attack cavern-map)]
                   (recur next-cavern elf-attack (if finished? round (inc round))))))))

(println (star "input15.txt"))
(println (star2 "input15.txt"))

