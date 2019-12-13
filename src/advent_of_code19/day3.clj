(ns advent-of-code19.day3
  [:require [clojure.string :as str]
            [clojure.set :as set]])

(def wires
  (let [lines (str/split-lines (slurp "resources/input/day3.txt"))
        wire1 (str/split (first lines) #",")
        wire2 (str/split (second lines) #",")]
    {:wire1 wire1 :wire2 wire2}))

(defn get-path [x y direction steps]
  (cond
    (= \R direction) (let [new-x (+ x 1)]
                       (for [i (range new-x (+ new-x steps))]
                         {:x i :y y}))
    (= \L direction) (sort-by :x > (for [i (range (- x steps) x)]
                                     {:x i :y y}))
    (= \U direction) (let [new-y (+ y 1)]
                       (for [i (range new-y (+ new-y steps))]
                         {:x x :y i}))
    (= \D direction) (sort-by :y > (for [i (range (- y steps) y)]
                                     {:x x :y i}))))

(defn calculate-position [vec idx]
  (let [path (get vec idx)
        direction (first path)
        steps (Integer/parseInt (subs path 1))
        coordinates (if (= idx 0) {:x 0 :y 0} (last (get vec (- idx 1))))
        x (:x coordinates)
        y (:y coordinates)
        path-coordinates (get-path x y direction steps)]
    (assoc vec idx path-coordinates)))

(defn get-coordinates [wire]
  (flatten (reduce #(calculate-position %1 %2) wire (range 0 (count wire)))))

(defn get-intersections [coordinates1 coordinates2]
  (let [set1 (set (map str coordinates1))
        set2 (set (map str coordinates2))]
    (set/intersection set1 set2)))

(defn intersection-distances [coordinate-str]
  (let [match (re-find #":x (-?\d+), :y (-?\d+)" coordinate-str)
        x (Integer/parseInt (nth match 1))
        y (Integer/parseInt (nth match 2))
        distance (+ (Math/abs x) (Math/abs y))]
    {:x x :y y :distance distance}))

(defn closest-intersection [coordinates1 coordinates2]
  (let [intersections (get-intersections coordinates1 coordinates2)]
    (first (sort-by :distance (map intersection-distances intersections)))))

(defn calculate-intersection-steps [intersection coordinates]
  (+ (.indexOf (map str coordinates) intersection) 1))

(defn least-steps-intersection [coordinates1 coordinates2]
  (let [intersections (get-intersections coordinates1 coordinates2)
        steps1 (mapv #(calculate-intersection-steps %1 coordinates1) intersections)
        steps2 (mapv #(calculate-intersection-steps %1 coordinates2) intersections)]
    (first (sort (reduce (fn [sums idx]
                           (let [val1 (get steps1 idx)
                                 val2 (get steps2 idx)]
                             (assoc sums idx (+ val1 val2))))
                         []
                         (range 0 (count steps1)))))))


(defn -main [& args]
  (println (closest-intersection (get-coordinates (:wire1 wires)) (get-coordinates (:wire2 wires))))
  (println (least-steps-intersection (get-coordinates (:wire1 wires)) (get-coordinates (:wire2 wires)))))



