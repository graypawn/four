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
