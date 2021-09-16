(ns day3.part1)

(def input
  (-> (slurp "../../inputs/2018/Day3/input.txt")
      (clojure.string/split-lines)))

(defn string->int [position]
  (Integer/parseInt position))

(defn parse-claims [line]
  (let [[id start-position width&height] (clojure.string/split line #"\s@\s|:\s")
        start-position (->> (clojure.string/split start-position #",")
                            (mapv string->int))
        width&height (->> (clojure.string/split width&height #"x")
                          (mapv string->int))]
    [id start-position width&height]))

(def claims (map parse-claims input))
