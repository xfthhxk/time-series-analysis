(ns tsa.math
  (:require [tech.v3.datatype.functional :as dfn]
            [tech.v3.datatype.statistics :as stats]))


;; NB. dfn is smart enough to operate on columns,
;; and scalars and combinations


(defn mape
  "Mean Absolute Percentage Error."
  [observations predictions]
  (-> (dfn/- observations predictions)
      (dfn// observations)
      dfn/abs
      dfn/mean))
