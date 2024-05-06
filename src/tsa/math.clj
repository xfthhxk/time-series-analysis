(ns tsa.math
  (:require [tech.v3.datatype.functional :as dfn]
            [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [tech.v3.datatype.statistics :as stats])
  (:import (org.hawkular.datamining.forecast.stats AugmentedDickeyFullerTest)))

(set! *warn-on-reflection* true)


(def +double-array-class+ (type (double-array 0)))

(def +float-array-class+ (type (float-array 0)))


;; NB. dfn is smart enough to operate on columns,
;; and scalars and combinations


(defn mape
  "Mean Absolute Percentage Error."
  [observations predictions]
  (-> (dfn/- observations predictions)
      (dfn// observations)
      dfn/abs
      dfn/mean))


(defn diff
  [xs n]
  (let [c (tcc/column xs)]
    (tcc/- (tcc/slice c n)
           (tcc/slice c 0 (- (count c) n 1)))))


(seqable? (double-array [1 2 3]))


(defn augmented-dickey-fuller-test
  "`xs` is an array of doubles and `max-lag` is an int.
  Returns a map with keys `:statistic` and `:p-value`"
  [xs max-lag]
  (let [d-xs (cond
               (= +double-array-class+ (type xs)) xs
               (tcc/column? xs) (double-array xs)
               (seqable? xs) (double-array xs)
               :else (throw (ex-info "Unable to convert to double-array" {:type (type xs)})))
        adf (AugmentedDickeyFullerTest. d-xs max-lag)]
    {:statistic (.statistics adf)
     :p-value (.pValue adf)}))


;; Algo adopted from https://github.com/signaflo/java-timeseries/blob/master/timeseries/src/main/java/com/github/signaflo/data/visualization/Plots.java
(defn acf-bounds
  [xs]
  (let [n (count xs)
        a (/ -1 n)
        b (/ 2 (Math/sqrt n))
        upper (+ a b)
        lower (- a b)]
    {:upper upper
     :lower lower}))


(defn prepare-data-corr-plot
  [xs lags include-zero-lag?]
  (let [{:keys [irregular? lags]}
        (cond
          (not lags) (let [nobs (count xs) ;; ideally this would use (first (shape xs))
                           lim (min (-> (Math/log10 nobs)
                                        (* 10)
                                        Math/ceil)
                                    (int (/ nobs 2)))]
                       {:lags (range (if include-zero-lag? 0 1) (inc lim))})
          (number? lags) {:lags (range (if include-zero-lag? 0 1) (inc lags))} ; +1 for zero lag
          :else {:irregular? true
                 :lags (range (inc lags))})
        nlags (max lags)]
    {:lags lags :nlags nlags :irregular (or irregular? (not include-zero-lag?))}))

(defn plot-corr
  [acf-x])

(defn plot-acf
  []
  )
