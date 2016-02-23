(ns four.medium)

;;Flipping out
(defn p46 [f]
  (fn [x y] (f y x)))

;;Rotate Sequence
(defn p44 [n s]
  (mapcat (fn [f] (f (mod n (count n 2)) s))
          [drop take]))

;;Reverse Interleave
(defn p43 [s n]
  (apply map list (partition n s)))

;;Split by Type
(defn p50 [s]
  (vals (group-by type s)))

;;Count Occurrences
(defn p55 [s]
  (into {}
        (map (fn [[k v]]
               [k (count v)])
             (group-by identity s))))

;;Find Distinct Items
(defn p56 [s]
  (reduce (fn [result x]
            (if ((set result) x)
              result
              (conj result x)))
          [] s))

;;Function Composition
(defn p58 [& fs]
  (fn [& xs]
    (reduce #(%2 %1) (apply (last fs) xs) (next (reverse fs)))))

;;Partition a Sequence
(defn p54 [n s]
  (if (> n (count s))
    []
    (cons (take n s)
          (p54 n (drop n s)))))

;;Juxtaposition
(defn p59 [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

;;Word Sorting
(defn p70 [s]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

;;Prime Numbers
(defn p67 [n]
  (take n
        (cons 2
              (filter (fn [x] (not-any? #(zerop? (mod x %))
                                        (range 2 (inc (Math/sqrt x)))))
                      (iterate #(+ 2 %) 3)))))

;;Filter Perfect Squares
(defn p74 [s]
  (clojure.string/join
   ","
   (filter (fn [n] (== (mod (Math/sqrt n) 1)
                       0))
           (map bigint (.split s ",")))))

;;Black Box Testing
(defn p65 [s]
  (condp = (empty s)
    #{} :set
    {} :map
    (first (conj (empty s)
                 :vector :list))))

;;Perfect Numbers
(defn p80 [n]
  (= n
     (reduce +
             (filter #(zero? (mod n %))
                     (range 1 n)))))

;;Anagram Finder
(defn p77 [c]
  (set (map set
            (remove #(= 1 (count %))
                    (vals (group-by sort c))))))

;;Sequence Reductions
(defn p60
  ([f [x & xs]] (p60 f x xs))
  ([f init xs]
   (if (seq xs)
     (lazy-seq
      (cons init
            (p60 f
                 (f init (first xs))
                 (rest xs))))
     [init])))

;;Merge with a Function
(defn p69 [f & ms]
  (reduce (fn [o m]
            (reduce #(if (contains? % (key %2))
                       (assoc % (key %2) (f (% (key %2))
                                            (val %2)))
                       (conj % %2))
                    o m))
          {} ms))

;;intoCamelCase
(defn p102 [s]
  (let [[x & xs] (.split s "-")]
    (apply str x
           (map #(clojure.string/capitalize %) xs))))

;;Euler's Totient Function
(defn p75 [n]
  (if (= n 1)
    1
    (count
     (filter #(= 1 ((fn [x y]
                      (if (zero? y)
                        x
                        (recur y (mod x y))))
                    n %))
             (range 1 n)))))

;;Happy numbers
(defn p86 [m]
  (= 1
     (some #{1 4}
           (iterate (fn [k] (reduce #(+ % (let [c (- (int %2) 48)]
                                            (* c c)))
                                    0
                                    (str k)))
                    m))))

;;Reimplement Trampoline
(defn p78 [f x]
  (fn it [g]
    (if (fn? g)
      (it (g))
      (f x))))

;;; The Balance of N
(defn p115 [n]
  (letfn [(half [s] (drop (quot (count s) 2) s))
          (sum [s] (reduce + (map #(- (int %) 48) (half s))))]
    (= (sum (str n)) (sum (reverse (str n))))))

;;; Power Set
(defn p85 [s]
  ((fn i [c]
     (if (contains? c s)
       c
       (i (reduce (fn [v x] (into v (map #(conj x %) s))) c c))
       )) #{#{}}))

;;; Equivalence Classes
(defn p98 [x y]
  (set (map set (vals (group-by x y)))))

;;; Identify keys and values
(defn p105 [v]
  (reduce (fn [m [ks vs]] 
            (into (assoc m (last ks) (vec vs)) 
                  (apply hash-map (interleave (butlast ks) (repeat (vec nil))))))
          {} (partition 2 (partition-by keyword? v))))

;;; Digits and bases
(defn p137 [n b]
  (if (= 0 (quot n b))
    [(mod n b)]
    (conj (p137 (quot n b) b) (mod n b))))

;;; Sequence of pronunciations
(defn p110 [v]
  (rest (iterate #(mapcat (juxt count first)
                          (partition-by identity %))
                 v)))

;;; Oscilrate
(defn p144 [init & fs]
  (reductions #(%2 %1) init (cycle fs)))

;;; Decurry
(defn p158 [f]
  (fn [& z]
    (reduce #(% %2) f z)))

;;; Lazy Searching
(defn p108 [x & z]
  ((fn it [c]
     (if (every? (fn [a]
                   (if (< (first c) (first a)) false
                       (= (first c) (first (drop-while #(< % (first c)) a))))) z)
       (first c)
       (it (rest c)))) x))

;;; Partially Flatten a Sequence
(defn p93 [x]
  (if (not-any? coll? x)
    [x] (mapcat p93 x)))

;;; Global take-while
(defn p114 [n p s]
  ((comp butlast second)
   (reduce (fn [[c v] x]
             (if (>= c n)
               [c v]
               (if (p x)
                 [(inc c) (conj v x)]
                 [c (conj v x)])))
           [0 []] s)))

;;; Insert between two items
(defn p132 [p k v]
  ((fn it [[x y & z]]
     (if y
       (lazy-seq
        (cons x (if (p x y)
                  (cons k (it (cons y z)))
                  (it (cons y z)))))
       (if x [x] []))) v))

;;; Write Roman Numerals
(fn p104 [n]
  (let [chart {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 
               50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}
        which (first (drop-while #(< n %) [1000 900 500 400 100 90 50 40 10 9 5 4 1 0]))]
    (if (zero? n) "" (str (chart which) (p104 (- n which))))))

;;; Generating k-combinations
(defn p103 [n s]
  (letfn [(i [c w]
     (if (contains? c w)
       c
       (i (reduce (fn [v x] (into v (map #(conj x %) s))) c c) w)
       ))]
    (set (filter #(= n (count %)) (i #{#{}} s)))))

;;; Prime Sandwich
(defn p116 [n] 
  (let [[a b] (split-with #(> n %)
                          (cons 2 (filter (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))
                                          (iterate #(+ 2 %) 3))))]
    (if (< n 3) false (= n (first b) (/ (+ (last a) (second b)) 2)))))

;;; Universal Computation Engine
(defn p121 [l]
  (fn [m]
    ((fn it [[f & z]]
       (apply ({'+ + '* * '/ / '- -} f)
              (map #(cond
                      (symbol? %) (m %)
                      (list? %) (it %)
                      :else %) z))) l)))

;;; The Big Divide
(defn p148 [n a b]
  ((fn [[x y z]] (- (+ x y) z))
   (map #(let [x (- (quot n %)
                    (if (zero? (mod n %)) 1 0))]
           (/ (*' x (+ x 1) %) 2))
        [a b (* a b)])))

;;; Intervals
(defn p171 [v]
  (let [[h :as s] (sort v)]
    (reverse
     (reduce (fn [[f & r] [x y :as z]]
               (if (or (= (inc x) y)
                       (= x y))
                 (conj r [(first f) y])
                 (conj r f [y y]) ))
             (if (empty? v) [] [[h h]])
             (partition 2 1 s)))))

;;; Balancing Brackets
(defn p177 [s]
  (let [cl (apply str (re-seq #"\{|\}|\[|\]|\(|\)" s))
        br #"\{\}|\[\]|\(\)"]
    ((fn it [s] (cond
                  (= "" s) true
                  (re-seq br s) (it (clojure.string/replace s br ""))
                  :else false)) cl)))

;;; Sum Some Set Subsets
(defn p131 [& sets]
  (letfn [(powerset [ls]
            (reduce(fn [acc elem] (into acc (map #(+ % elem) acc))) [0] ls))]
    (let [v (map powerset sets)
          s (apply clojure.set/intersection (map set v))]
      (if (= #{0} s)
        (every? #(= 2 (count (filter zero? %))) v)
        (not= 0 (count v))))))

;;; Tricky card games
(defn p141 [k]
  (fn [v]
    (last (sort-by (fn [{s :suit r :rank}]
                     (+ (s {:spade 30 :heart 25 :diamond 20 :club 15}) r))
                   (filter #(if k (= (:suit %) k) true) v)))))

;;; Infinite Matrix
(defn p168
  ([f] (p168 f 0 0))
  ([f n m]
   ((fn a [x]
      (lazy-seq (cons ((fn b [y]
                         (lazy-seq (cons (f x y) (b (inc y))))) m) (a (inc x))))) n))
  ([f n m p q] (take p (map #(take q %) (p168 f n m)))))
