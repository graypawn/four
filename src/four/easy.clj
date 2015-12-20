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

;;dot Product
(defn p143 [x y]
  (reduce + (map * x y)))

;;Infix Calculator
(defn p135 [x & xs]
  (reduce (fn [a [op b]] (op a b)) x (partition 2 xs)))

;;Indexing Sequences
(defn p157 [v]
  (map-indexed #(vector %2 %1) v))

;;Pascal's Triangle
(defn [n]
  (reduce #(conj %1 (* (last %1) (/ (- n %2) %2 ))) [1] (range 1 n)))

;;Re-implement Map
(defn p118 [f c]
  (if (seq c)    
    (lazy-seq     
     (cons (f (first c)) (p118 f (rest c))))))

;;To Tree, or not to Tree
(defn p95 [[k l r :as t]]
  (and (= 3 (count t))
       (if (coll? l) (p95 l) (nil? l))
       (if (coll? r) (p95 r) (nil? r))))

;;Sum of square of Digits
(defn p120 [c] (count (filter       
          (fn [n] (< n (reduce + (map (comp #(* % %) #(- (int %) 48)) (str n))))) c)))

;;Recoginize Playing Cards
(defn [[s r]]
  {:suit ({\D :diamond \H :heart \C :club \S :spade} s) 
   :rank (if (> 58 (int r)) (- (int r) 50) ({\T 8 \J 9 \Q 10 \K 11 \A 12} r))})

;;Least Common Multiple
(defn p100 [& xs]
  #(letfn [(gcd
          ([x y] (if (= y 0) x (recur y (mod x y))))
          ([x y & more] (reduce gcd (gcd x y) more)))]
     (/ (reduce * xs) (apply gcd xs))))

;;Pascal's Trapezoid
(defn p147 [s]
  (iterate #(mapv +' `(0 ~@%) `(~@% 0))))

;;Beauty is Symmetry
(defn p96 [[_ ll rr]]
  (= rr ((fn it [[k l r]] [k (if r (it r) nil)
                            (if l (it l) nil)]) ll)))
;;Trees into tables
(defn p146 [t]
  (into {} (for [[kk m] t
                 [k v] m]
             [[kk k] v])))

;;Pairwise Disjoint Sets
(defn p153 [s]
  (= (count (set (apply concat s)))
     (reduce (fn [x y] (+ x (count y)))
             0 s)))

