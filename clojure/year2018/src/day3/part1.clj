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

(defn get-plot-width&plot-height [claim]
  (let [[_ [start-x start-y] [claim-width claim-height]] claim]
    [(+ start-x claim-width) (+ start-y claim-height)]))

(get-plot-width&plot-height ["1" [1 3] [4 4]])

(defn plot-claim [claim]
  (let [[id [start-x start-y]] claim
        [plot-width plot-height] (get-plot-width&plot-height claim)
        char-representaions (for [y (range plot-height)
                                  x (range plot-width)]
                              (let [ch (if (and (>= x start-x) (>= y start-y))
                                         id
                                         ".")]
                                (if (= x (dec plot-width))
                                  (str ch \newline)
                                  ch)))]
    (apply str char-representaions)))

(println (plot-claim ["1" [1 3] [4 4]]))

(defn plot-claims [claims]
  (let [plot-widths&heights (map get-plot-width&plot-height claims)
        [map-width map-height] [(apply max (first plot-widths&heights)) (apply max (second plot-widths&heights))]
        char-representations (into [] (for [y (range map-height)
                                            x (range map-width)]
                                        (if (= x (dec map-width))
                                          (str "." \newline)
                                          ".")))]
    char-representations))

(defn print-char-representations [char-representations]
  (println (apply str char-representations)))

(-> (plot-claims [["1" [1 3] [4 4]]
                  ["2" [3 1] [4 4]]
                  ["3" [5 5] [2 2]]])
    (print-char-representations))
