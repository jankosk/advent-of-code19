(ns advent-of-code19.day4
  [:require [clojure.string :as str]])

(defn get-digit-sequence [num-str]
  (first (re-find #"(\d)\1+" num-str)))

(defn get-digit-sequences [num-str]
  (loop [ns num-str sequences []]
    (let [seq (get-digit-sequence ns)]
      (if (= ns "")
        sequences
        (if-not (boolean seq)
          sequences
          (recur (str/replace ns seq "") (conj sequences seq)))))))

(defn has-even-digit-sequences [num]
  (let [num-str (str num)
        sequences (get-digit-sequences num-str)]
    (some #(= 2 (count %1)) sequences)))

(defn is-increasing [num]
  (let [digits (map #(Integer/parseInt %1) (str/split (str num) #""))]
    (loop [digs digits]
      (if (empty? digs)
        true
        (if-not (every? #(>= %1 (first digs)) (rest digs))
          false
          (recur (rest digs)))))))

(defn valid-passwords [from to]
  (count
    (filter
      true?
      (for [num (range from (+ to 1))]
        (every? true? (vector
                        (is-increasing num)
                        (has-even-digit-sequences num)))))))


(defn -main [& args]
  (println (valid-passwords 356261 846303)))

