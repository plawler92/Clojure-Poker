(import '(javax.swing JOptionPane))

(def card-nums (zipmap "23456789TJQKA" (iterate inc 2)))

(defn card-val [c] (card-nums (first c)))


(def deck (for [suit ["C" "H" "S" "D"]
                num ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"]]
            (str num suit)))

;(def deck (shuffle (remove (into #{} ["4C" "3C"]) (for [suit ["C" "H" "S" "D"]
 ;                                              num ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"]]
  ;                                         (str num suit)))))

;(def hand ["4C" "3C" (first deck) (second deck) (nth deck 2) (nth deck 3) (nth deck 4)])

;steps
;1.) get cards user is using -------------------> seems good
;2.) get hand (take from deck -minus player cards -------> seems good
;3.) check hand -------------------------------------> seems good
;4.) repeat
;is good
(defn get-cards []
  [(. JOptionPane showInputDialog nil "Enter First Card (enter 'T' for 10)")
   (. JOptionPane showInputDialog nil "Enter Second Card (enter 'T' for 10)")])
;is good
(defn get-hand [cards]
  (let [temp-deck (shuffle (remove (into #{} [cards])
                             deck))]
    (into cards [(first temp-deck) (second temp-deck) (nth temp-deck 2) (nth temp-deck 3) (nth temp-deck 4)])))

;is good
(defn flush? [hand]
  (let [suits (map second hand)]
    (if (>= (count (filter #{\D} suits)) 5)
      true
      (if (>= (count (filter #{\C} suits)) 5)
        true
        (if (>= (count (filter #{\S} suits)) 5)
          true
          (if (>= (count (filter #{\H} suits)) 5)
            true
            false))))))

;is good
(defn straight-check? [five-hand]
  (= 4 (- (last five-hand) (first five-hand))))

;is good
(defn straight? [hand]
  (let [vals  (into []
                (distinct (map #(card-val %) (sort #(< (card-val %1) (card-val %2)) hand))))]
    (if (< (count vals) 5)
      false
      (if (straight-check? (take 5 vals))
        true
        (if (> (count vals) 5)
          (if (straight-check? (take 5 (drop 1 vals)))
            true
            (if (> (count vals) 6)
              (if (straight-check? (take 5 (drop 2 vals)))
                true)
              false))
          false)))))



(defn n-of-a-kind? [n k hand]
  (let [values (map #(card-val %) hand)]
    (if (= k (count (filter #{n} (vals (frequencies values)))))
      true
      false)))

;need to get two pair working
(defn get-hand-ranking [hand]
  (cond
    (and (straight? hand) (flush? hand)) "Straight Flush"
    (n-of-a-kind? 4 1 hand) "Four of a Kind"
    (and (n-of-a-kind? 3 1 hand) (n-of-a-kind? 2 1 hand)) "Full House"
    (flush? hand) "Flush"
    (straight? hand) "Straight"
    (n-of-a-kind? 3 1 hand) "Three of a Kind"
    (n-of-a-kind? 2 2 hand) "Two of a Kind"
    (n-of-a-kind? 2 1 hand) "Pair"
    :else "High Card"))

;seems to be working
(defn get-probabilities []
  (let [cards (get-cards)]
    (let [hand-rank (frequencies (for [x (range 100)]
                                   (let [hand (get-hand cards)]
                                     (get-hand-ranking hand))))]
      (vector (keys hand-rank) (map #(double (/ % 100)) (vals hand-rank))))))

;seems fine
(defn print-probs [probs]
  (let [message (if (= (count (get probs 0)) 1)
                  (str (first (get probs 0)) ": " (first (get probs 1)))
                  (if (= (count (get probs 0)) 2)
                    (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                      (second (get probs 0)) ": " (second (get probs 1)))
                    (if (= (count (get probs 0)) 3)
                      (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                        (second (get probs 0)) ": " (second (get probs 1)) "\n"
                        (nth (get probs 0) 2) ": " (nth (get probs 1) 2))
                      (if (= (count (get probs 0)) 4)
                        (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                          (second (get probs 0)) ": " (second (get probs 1)) "\n"
                          (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                          (nth (get probs 0) 3) ": " (nth (get probs 1) 3))
                        (if (= (count (get probs 0)) 5)
                          (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                            (second (get probs 0)) ": " (second (get probs 1)) "\n"
                            (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                            (nth (get probs 0) 3) ": " (nth (get probs 1) 3) "\n"
                            (nth (get probs 0) 4) ": " (nth (get probs 1) 4))
                          (if (= (count (get probs 0)) 6)
                            (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                              (second (get probs 0)) ": " (second (get probs 1)) "\n"
                              (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                              (nth (get probs 0) 3) ": " (nth (get probs 1) 3) "\n"
                              (nth (get probs 0) 4) ": " (nth (get probs 1) 4) "\n"
                              (nth (get probs 0) 5) ": " (nth (get probs 1) 5))
                            (if (= (count (get probs 0)) 7)
                              (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                                (second (get probs 0)) ": " (second (get probs 1)) "\n"
                                (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                                (nth (get probs 0) 3) ": " (nth (get probs 1) 3) "\n"
                                (nth (get probs 0) 4) ": " (nth (get probs 1) 4) "\n"
                                (nth (get probs 0) 5) ": " (nth (get probs 1) 5) "\n"
                                (nth (get probs 0) 6) ": " (nth (get probs 1) 6))
                              (if (= (count (get probs 0)) 8)
                                (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                                  (second (get probs 0)) ": " (second (get probs 1)) "\n"
                                  (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                                  (nth (get probs 0) 3) ": " (nth (get probs 1) 3) "\n"
                                  (nth (get probs 0) 4) ": " (nth (get probs 1) 4) "\n"
                                  (nth (get probs 0) 5) ": " (nth (get probs 1) 5) "\n"
                                  (nth (get probs 0) 6) ": " (nth (get probs 1) 6) "\n"
                                  (nth (get probs 0) 7) ": " (nth (get probs 1) 7))
                                (if (= (count (get probs 0)) 9)
                                  (str (first (get probs 0)) ": " (first (get probs 1)) "\n"
                                    (second (get probs 0)) ": " (second (get probs 1)) "\n"
                                    (nth (get probs 0) 2) ": " (nth (get probs 1) 2) "\n"
                                    (nth (get probs 0) 3) ": " (nth (get probs 1) 3) "\n"
                                    (nth (get probs 0) 4) ": " (nth (get probs 1) 4) "\n"
                                    (nth (get probs 0) 5) ": " (nth (get probs 1) 5) "\n"
                                    (nth (get probs 0) 6) ": " (nth (get probs 1) 6) "\n"
                                    (nth (get probs 0) 7) ": " (nth (get probs 1) 7) "\n"
                                    (nth (get probs 0) 8) ": " (nth (get probs 1) 8) "\n"))))))))))]
    (. JOptionPane showMessageDialog nil message)))

(print-probs (get-probabilities))


