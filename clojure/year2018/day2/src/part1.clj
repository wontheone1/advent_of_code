(ns part1)

(def input
  (slurp "../../../inputs/2018/Day2/input.txt"))

(defn run [opts]
  (let [[times-two-of-letter-detected
         times-three-of-letter-detected] (->> input
                                              clojure.string/split-lines
                                              (map (comp (fn [frequencies->chars]
                                                           [(if (frequencies->chars 2)
                                                              1
                                                              0)
                                                            (if (frequencies->chars 3)
                                                              1
                                                              0)])
                                                         #(group-by (fn [[_k v]] v) %)
                                                         frequencies))
                                              (reduce
                                                (fn [[times-two-of-letter-detected
                                                      times-three-of-letter-detected]
                                                     [two-of-letter-detected
                                                      three-of-letter-detected]]
                                                  [(+ times-two-of-letter-detected two-of-letter-detected)
                                                   (+ times-three-of-letter-detected three-of-letter-detected)])))]
    (prn (* times-two-of-letter-detected times-three-of-letter-detected))))

; run with
; cd year2018/day2
; clj -X part1/run
; => 7776
