(ns four.hard)

;;Number Maze
(defn p106-1
  "This is Bad Way. We want only shortest count"
  ([x y] (p106-1 x y 1))
  ([x y n]
   (cond
    (= x y) n
    (< x y) (cond
             (<= (* x 2) y) (p106-1 (* x 2) y (inc n))
             (<= (+ x 2) y) (p106-1 (+ x 2) y (inc n))
             :else (p106-1 (* x 2) y (inc n)))
    :else (cond
           (odd? x) (p106-1 (* x 2) y (inc n))
           (or (even? (/ x 2))
               (= (/ x 2) y)) (p106-1 (/ x 2) y (inc n))
           (> (/ x 2) y) (p106-1 (+ x 2) y (inc n))
           (odd? y) (p106-1 (/ x 2) y (inc n))
           :else (p106-1 (+ x 2) y (inc n)) ))))

;;Gus' Quinundrum
(defn p125-1 []
  "This Solution is lisp stytle
   code : (fn [x] (str x x)) (quote (fn [x] (str x x)))"
  (= (str '(fn [x] (str x x)) (quote (fn [x] (str x x))))
     ((fn [x] (str x x)) (quote (fn [x] (str x x))))))

(fn [] (let [s "(fn [] (let [s "]))

(fn [] (str ((fn [x] (str (list 'fn [] (list 'str (list x (list 'quote x))))))
             '(fn [x] (str (list 'fn [] (list 'str (list x (list 'quote x)))))))))

((fn [] (str ((fn [x] (str (list 'fn [] (list 'str (list x (list 'quote x))))))
              '(fn [x] (str (list 'fn [] (list 'str (list x (list 'quote x))))))))))

;; Best Hand
(defn p178-1 [cards]
  (let [suits (map first cards)
        ranks (map (comp {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
                          \T 10 \J 11 \Q 12 \K 13 \A 14}
                         second)
                   cards)

        flush? (= 1 (count (distinct suits)))

        max-ranks (apply max ranks)
        straight? (or (= (map #(- max-ranks %) (sort ranks))
                         [4 3 2 1 0])
                      (= (sort ranks)
                         [2 3 4 5 14]))

        pairs (condp = (sort (vals (frequencies ranks)))
                [1 1 1 2] :pair
                [1 2 2]   :two-pair
                [1 1 3]   :three-of-a-kind
                [1 4]     :four-of-a-kind
                [2 3]     :full-house
                nil)]
    (if pairs
      pairs
      (cond
        (and straight? flush?) :straight-flush
        straight? :straight
        flush? :flush
        :else :high-card))))
