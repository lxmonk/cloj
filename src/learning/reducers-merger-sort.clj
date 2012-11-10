(ns learning.reduceSort
  (:require [clojure.core.reducers :as r]))

(defn combiner
  [col1 col2]
  (cond
   (not (seq col1)) col2
   (not (seq col2)) col1
   (<= (first col1) (first col2))
     (cons (first col1) (combiner (rest col1) col2))
   :else (cons (first col2) (combiner col1 (rest col2)))))
