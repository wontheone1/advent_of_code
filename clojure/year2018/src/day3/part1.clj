(ns day3.part1)

(def input
  (-> (slurp "../../inputs/2018/Day3/input.txt")
      (clojure.string/split-lines)))

(defn string->int [position]
  (Integer/parseInt position))

(defn parse-claims [line]
  (let [[id start-position width&height] (clojure.string/split line #"\s@\s|:\s")
        id (->> id next (apply str))
        start-position (->> (clojure.string/split start-position #",")
                            (mapv string->int))
        width&height (->> (clojure.string/split width&height #"x")
                          (mapv string->int))]
    [id start-position width&height]))

(def claims (map parse-claims input))

(defn plot-claim [id [start-x start-y] [claim-width claim-height]]
  (let [plot-height (+ start-y claim-height)
        plot-weight (+ start-x claim-width)
        char-representaions (for [y (range plot-height)
                                  x (range plot-weight)]
                              (let [ch (if (and (>= x start-x) (>= y start-y))
                                         id
                                         ".")]
                                (if (= x (dec plot-weight))
                                  (str ch \newline)
                                  ch)))]
    (apply str char-representaions)))

(println (plot-claim "1" [1 3] [4 4]))
