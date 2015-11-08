(ns four.easy)

;;Last Element
(defn p19 [c]
  (first (reverse c)))

;;Penultimate Element
(defn p20 [c]
  ((comp last butlast) c))

;;Nth Element
(defn p21 [c n]
  (first (drop n c)))

;;Count a Sequence
(defn p22 [c]
  (reduce (fn [x y] (inc x)) 0 c))

;;Reverse a Sequence
(defn p23 [c]
  (vec (into (list) c)))

;;Sum It All Up
(defn p24 [c]
  (reduce + c))

;;Find the odd numbers
(defn p25 [c]
  (filter odd? c))

;;Fibonacci Sequence
(defn p26 [n]
  (->> [1 0]
       (iterate (fn [[x y]] [(+ x y) x]))
       (take n)
       (map first)))

;;Palindrome Detector
(defn p27 [c]
  (= (reverse c) (seq c)))

;;Flatten a Sequence

;;Get the Caps
(defn p29 [s]
  (apply str (re-seq #"[A-Z]" s)))

;;Compress a Sequence
(defn p30 [c]
  (map first (partition-by identity c)))

;;Pack a Sequence
(defn p31 [c]
  (partition-by identity c))

;;Duplicate a Sequence
(defn p32 [c]
  (interleave c c))

;;Replicate a Sequence
(defn p33 [c n]
  (mapcat (partial repeat n) c))

;;Implement range
(defn p34 [from to]
  (take-while #(< % to) (iterate inc from)))

;;Maximum value
(defn p38 [& xs]
  (reduce (fn [x y] (if (< x y) y x)) xs))

;;Interleave Two Seqs
(defn p39 [& cs]
  (apply mapcat list cs))

;;Interpose a Seq
(defn p40 [x c]
  (rest (interleave (repeat x) c)))

;;Drop Every Nth Item
(defn p41 [c n]
  (apply concat (partition-all (dec n) n c)))

;;Factorial Fun
(defn p42 [n]
  (reduce * (range n 1 -1)))

;;Split a sequence
(defn p49 [n c]
  ((juxt take drop) n c))

;;Map Construction
(defn p61 [ks vs]
  (into {} (map vector ks vs)))

;;Re-implement Iterate
(defn p62 [f x]
  (cons x (lazy-seq
           (p62 f (f x)))))

;;Group a Sequence
(defn p63 [f c]
  (into {} (for [x (set (map f c))]
             [x (filter #(= x (f %)) c)])))

;;Greatest Common Divisor
(defn p66 [x y]
  (if (= y 0)
    x
    (p66 y (mod x y))))

;;Set Intersection
(defn p81 [x y]
  ((comp set filter) x y))

;;A Half-Truth
(defn p83 [& xs]
  (apply not= xs))

;;Symmetric Difference
(defn p88 [x y]
  (set (concat (remove x y) (remove y x))))

;;Cartesian Product
(defn p90 [s1 s2]
  (set (for [x s1 y s2]
         [x y])))

;;Product Digits
(defn p99 [x y]
  (map (comp read-string str) (str (* x y))))

;;Simple closures
(defn p107 [n]
  (fn [x] (reduce * (repeat n x))))

;;Read a binary number
(defn p122 [s]
  (reduce #(+ (* % 2) ({\0 0 \1 1} %2)) 0 s))

;;Comparisons
(defn p166 [f x y]
  (cond (f x y) :lt
        (f y x) :gt
        :else :eq))

