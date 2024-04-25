(ns tsa.math
  (:require [tech.v3.datatype.functional :as dfn]
            [tech.v3.datatype.statistics :as stats])
  (:import (org.hawkular.datamining.forecast.stats AugmentedDickeyFullerTest)))

(set! *warn-on-reflection* true)


;; NB. dfn is smart enough to operate on columns,
;; and scalars and combinations


(defn mape
  "Mean Absolute Percentage Error."
  [observations predictions]
  (-> (dfn/- observations predictions)
      (dfn// observations)
      dfn/abs
      dfn/mean))


(defn augmented-dickey-fuller-test
  "`xs` is an array of doubles and `max-lag` is an int.
  Returns a map with keys `:statistic` and `:p-value`"
  [xs max-lag]
  (let [adf (AugmentedDickeyFullerTest. xs max-lag)]
    {:statistic (.statistics adf)
     :p-value (.pValue adf)}))
