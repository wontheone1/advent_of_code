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

(defn update-char-in-representations [id index char-representations]
  (update char-representations
          index
          (fn [ch]
            (cond
              (= "." ch)
              id

              (= ".\n" ch)
              (str id \newline)

              (clojure.string/ends-with? ch "\n")
              "X\n"

              :else
              "X"))))

(update-char-in-representations "1" 2 ["." "." "." ".\n"
                                       "." "." "." ".\n"])
(update-char-in-representations "1" 3 ["." "." "." ".\n" "."
                                       "." "." ".\n"])
(update-char-in-representations "1" 2 ["." "." "2" ".\n"
                                       "." "." "." ".\n"])
(update-char-in-representations "1" 3 ["." "." "." "2\n"
                                       "." "." "." ".\n"])

(defn update-char-representations-with-claim [map-width char-representations claim]
  (let [[id [start-x start-y]] claim
        [plot-width plot-height] (get-plot-width&plot-height claim)]
    (loop [x start-x
           y start-y
           char-representations char-representations]
      (cond
        (< x (dec plot-width))
        (recur (inc x)
               y
               (update-char-in-representations id (+ (* y map-width) x) char-representations))

        (< y (dec plot-height))
        (recur start-x
               (inc y)
               (update-char-in-representations id (+ (* y map-width) x) char-representations))

        :else
        (update-char-in-representations id (+ (* y map-width) x) char-representations)))))

(update-char-representations-with-claim 4
                                        ["." "." "." ".\n"
                                         "." "." "." ".\n"]
                                        ["1" [0 0] [2 2]])

(update-char-representations-with-claim 4
                                        ["." "2" "." "3\n"
                                         "." "2" "." "3\n"]
                                        ["1" [1 0] [3 2]])

(defn plot-claims [claims]
  (let [plot-widths&heights (map get-plot-width&plot-height claims)
        [map-width map-height] [(apply max (map first plot-widths&heights)) (apply max (map second plot-widths&heights))]
        char-representations (into [] (for [y (range map-height)
                                            x (range map-width)]
                                        (if (= x (dec map-width))
                                          (str "." \newline)
                                          ".")))]
    (loop [[claim & rest-claims] claims
           char-representations char-representations]
      (if (seq rest-claims)
        (recur rest-claims (update-char-representations-with-claim map-width char-representations claim))
        (update-char-representations-with-claim map-width char-representations claim)))))

(defn print-char-representations [char-representations]
  (println (apply str char-representations)))

;#1 @ 1,3: 4x4
;#2 @ 3,1: 4x4
;#3 @ 5,5: 2x2
(-> (plot-claims [["1" [1 3] [4 4]]
                  ["2" [3 1] [4 4]]
                  ["3" [5 5] [2 2]]])
    (print-char-representations))

(defn count-overwrapping-claims [char-representations]
  (count (filter {"X" "X\n"} char-representations)))

(-> (plot-claims [["1" [1 3] [4 4]]
                  ["2" [3 1] [4 4]]
                  ["3" [5 5] [2 2]]])
    (count-overwrapping-claims))

(defn solve []
  (->> claims
      (plot-claims)
      (count-overwrapping-claims)
      (println "Area of fabric with overwrapping claims: ")))

'(solve)
