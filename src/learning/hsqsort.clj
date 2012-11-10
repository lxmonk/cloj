(ns learning.hsqsort)

(defn qsort
  ([[x & xs]]
     (if (seq xs)
       (let [small (for [y xs :when (<= y x)] y)
             large (for [y xs :when (> y x)] y)]
         (do (prn :small small :x [x] :large large)
             (concat (qsort small) [x] (qsort large))))
       (if x [x] []))))

(defn qsort2 [[x & xs]]
  (prn :x x :xs xs)
  (when x
    (let [[small large] (partition-by #(<= % x) xs)]
      (do (prn :small small :x [x] :large large)
          (concat (qsort2 small) [x] (qsort2 large))))))
