(ns learning.4clojure)

(def f77
  (fn [words]
    (letfn [(worker [ret-map word]
              (let [root (sort (seq word))
                    st (get ret-map root [])]
                (conj ret-map [root (conj st word)])))
            (to-set [ret-map] (set (filter #(< 1 (count %))
                                           (map set (vals ret-map)))))]
      (loop [word (first words)
             words (next words)
             ret-map {}]
        (let [ret-map (worker ret-map word)]
          (if (nil? words)
            (to-set ret-map)
            (recur (first words) (next words) ret-map)))))))

(def f121-eval
  "forbidden due to eval :|"
  (fn [expr]
    (fn [binds]
      (let [binds# (vec (flatten (vec binds)))
            expr# expr]
        (eval `(let ~binds# ~expr#))))))

(def f121
  (fn [expr]
    (fn [binds]
      (letfn [(calc  [[op & args]]
                (cond
                 (nil? op) (list)
                 (= '+ op) (apply + (calc args))
                 (= '- op) (apply - (calc args))
                 (= '* op) (apply * (calc args))
                 (= '/ op) (apply / (calc args))
                 (seq? op) (cons (calc op) (calc args))
                 (nil? args) (list op)
                 :else (cons op (calc args))))
              (sub [args binds]
                (map (fn [arg] (if (list? arg)
                                 (sub arg binds)
                                 (get binds arg arg)))
                     args))]
        (calc (sub expr binds))))))

(def f80
  (fn [n]
    (letfn [(divisors [n]
              (for [d (range 1 n) :when (integer? (/ n d))] d))]
      (= n (reduce + (divisors n))))))

(def f74
  (fn [s]
    (let [nums (map #(Integer/parseInt %) (clojure.string/split s #","))]
      (clojure.string/join
       "," (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %)))
                   nums)))))

(def f59
  (fn [& fns]
    (fn [& args]
      (for [f fns] (apply f args)))))

(def f60
  (letfn [(worker
            ([f coll1 coll2 prev ret]
               (prn [coll1 coll2 prev ret])
               (cond
                (and (empty? coll1) (empty? coll2)) ret
                (empty? coll1) (worker f coll2 (empty coll2) prev ret)
                :else (let [result (f prev (first coll1))]
                        (worker f (next coll1) coll2 result
                                (conj ret result)))))
            ([f coll1 coll2 ret]
               (prn [coll1 coll2 ret])
               (let [prev (f (empty coll1) (first coll1))]
                 (worker f (next coll1) coll2 prev (conj ret prev)))))]
    (fn
      ([f coll] (worker f coll (empty coll) (empty coll)))
      ([f coll1 coll2] (worker f coll1 coll2 (empty coll1))))))

(defn my-reduce [f base-val coll results]
  (if (empty? coll)
    results ; base-val
    (cons results
          (lazy-seq (f (first coll)
                       (my-reduce f base-val (next coll) results))))))

(def f152
  (fn [vs]
    (let [len (apply max (map count vs))
          sqrs (squares vs)]
      (println :f152-len len :sqrs sqrs)
      (frequencies
       (map second (distinct
                    (for [sqr sqrs
                          row (range len)
                          col (range len)
                          size (range 2 (inc len))
                          dc (do (println :sqr sqr :row row :col col
                                          :size size) [1])
                          :when (latin-square? sqr row col size)]
                      [sqr size])))))))

(defn countdown [v]
  (map #(vec (range %)) (map inc v)))
        
(defn squares
  "create an IxJxK.. matrix of squares, where I, J, K, ... are each in
   correspondence with the difference in length between a vector in vs
   and the maximal length of a vector in vs.
   Every dimension holds a square which  is different from other squares
   in the dimenoffset of a single vector in vs."
  ([vs]
     (println "(squares " vs ")")
     (let [len (apply max (map count vs))
           padded (mapv #(pad len %) vs)
           diffs (map #(- len (count %)) vs)
           shifts (eval (let [syms (repeatedly (count diffs) gensym)]
                          `(for ~(vec
                                  (interleave syms `~(countdown diffs)))
                             ~(vec syms))))]
       (println :len len :padded padded :shifts shifts)
       (let [ret (map #(shift-square padded %) shifts)]
             (println "=> " ret)
             ret))))

;;; (map #(shift-square PADS %) shfts)

(defn pad [len v]
  (vec (concat (repeat (- len (count v)) nil) v)))

(defn shift-square [square shifts]
  ;; (println :square square :shifts shifts)
  ;; (assert (= (count square) (count shifts)))
  (vec (for [i (range (count square))]
         (shift (square i) (shifts i)))))

(defn latin-square? [sqr row col size]
  (println  "(latin-square?" sqr row col size ")")
  (let [ret
        (try
          (let [sqr (subvec2 sqr row col size)
                transposed-sqr (transpose sqr)
                members (set (filter not-nil (flatten sqr)))]
            (and
                                        ; exactly the right number of members
             (= (count members) size)   
                                        ; all rows and cols have all the members
             (every? identity
                     (for [i (range size)]
                       (and (= members (sqr i))
                            (= members (transposed-sqr i)))))))
          (catch Exception e false))]
    (println "latin-square? =>" ret)))
                                  

(defn subvec2 [sqr row col size]
  (let [rows (subvec sqr row (+ row size))]
    (vec (for [r rows] (subvec r col (+ col size))))))

(defn not-nil [x]
  (not (nil? x)))

(defn transpose [vs]
  (vec (apply map vector vs)))

(defn shift
  ([v] (shift v 1))
  ([v n]
     (vec (concat (subvec v n) (subvec v 0 n)))))
     ;; (vec (flatten (cons (subvec v n) (take n v))))))