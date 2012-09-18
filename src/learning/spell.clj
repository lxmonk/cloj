(ns learning.spell
  (:require [clojure.string :refer [lower-case]]))

(defn words
  "return only the non-digits from text"
  [txt]
  (re-seq #"[a-z]+" (lower-case txt)))

(defn train [features]
  (reduce (fn [model f] (assoc model f (inc (get model f 1)))) {} features))

(defn edits1
  [word]
  (let
      [alphabet "abcdefghijklmnopqrstuvwxyz"
       n (count word)
       splits (for [i (range (inc n))]
                [(subs word 0 i) (subs word i)])
       deletes (for [i (range n)] (str (first (nth splits i))
                                       (subs (second (nth splits i)) 1)))
       transposes (for [i (range n)]    ; unnecessary to (inc n)
                    (let [a (first (nth splits i)) b (second (nth splits i))]
                      (str a (second b) (first b) (if (< 2 (count b)) (subs b 2)))))
       replaces (for [i (range (inc n)) c alphabet :when (< 0 (count (second (nth splits i))))]
                  (let [a (first (nth splits i)) b (subs (second (nth splits i)) 1)]
                    (str a c b)))
       inserts  (for [i (range (inc n)) c alphabet]
                  (let [a (first (nth splits i)) b (second (nth splits i))]
                        (str a c b)))]
    (distinct (concat deletes transposes replaces inserts))))

(defn known-edits2
  [word nwords]
  (distinct (for [e1 (edits1 word) e2 (edits1 e1) :when (nwords e2)] e2)))

(defn known
  [words nwords]
  (seq (for [w words :when (nwords w)] w)))

(defn correct
  [word nwords]
  (reduce (fn [cur-max k]
            (if (>= (nwords cur-max 1) (nwords k 1))
              cur-max
              k))
          ""
          (or (known word nwords) (known (edits1 word) nwords)
              (known-edits2 word nwords) word)))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(def fib-seq (lazy-cat [0 1]
                       (map + fib-seq (rest fib-seq))))
