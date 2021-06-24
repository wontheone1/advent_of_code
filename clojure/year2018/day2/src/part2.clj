(ns part2)

(def input
  (slurp "../../../inputs/2018/Day2/input.txt"))

(defn string-differences [str1 str2]
  (->> (map (fn [char1 char2]
              (if (= char1 char2)
                0
                1))
            str1
            str2)
       (reduce +)))

(defn run [opts]
  (prn (string-differences "fghij" "fauij")))

; run with
; cd year2018/day2
; clj -X part2/run
