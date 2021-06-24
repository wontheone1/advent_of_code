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

(defn get-common-string-part [str1 str2]
  (apply str (keep-indexed
               (fn [i ch]
                 (when (= ch (get str1 i))
                   ch))
               str2)))

(defn run [opts]
  (prn (string-differences "fghij" "fauij")
       (get-common-string-part "fghij" "fghij")))

; run with
; cd year2018/day2
; clj -X part2/run
