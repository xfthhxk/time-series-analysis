;; This namespace follows https://github.com/marcopeix/TimeSeriesForecastingInPython/blob/master/CH03/CH03.ipynb
(ns tsa.ch3
  (:require [tablecloth.api :as tc]
            [tsa.math :as math]
            [nextjournal.clerk :as clerk]
            [fastmath.random :as random]
            [fastmath.stats :as stats]))

;; # Random Walk
;; A random walk is defined using equation:
(clerk/tex "
\\begin{alignedat}{2}
y_{t} = y_{t-1} + C + \\epsilon_{t}
\\end{alignedat}
")

;; where C is a constant and a random number epsilon represents white noise.
;; As an example, we'll start at `y = 0` and set `C = 0`, then the random walk
;; is essentially the sum of each epislon at time *t*.

(clerk/tex "
\\begin{alignedat}{2}
y_{t} = \\sum_{\\mathclap{t = 1}}^{T} \\epsilon_{t}
\\end{alignedat}
")

;; This is how we'd generate the steps in Clojure
(def steps-clj
  (let [d (-> random/default-normal
              (random/set-seed! 42))]
    (->> (repeatedly 1000 #(random/sample d))
         vec
         (into [0]))))


;; However we'll use the Python generated data with seed 42 since the seeds don't seem to produce the same data.
(def steps (-> "./resources/data/ch3-python-seed-42-std-normal.csv"
               (tc/dataset {:header-row? false})
               (tc/->array "column-0")
               vec
               (assoc 0 0)))

(def random-walk (reductions + steps))


(clerk/plotly
 {:data [{:type "scatter"
          :name "Random Walk"
          :x (range 1000)
          :y random-walk}]
  :layout {:title "Random Walk"
           :xaxis {:title "Timesteps"}
           :yaxis {:title "Value"}}})


;; A random walk is a series whose first difference is stationary and uncorrelated.
;; Stationary requires a constant mean, variance and autocorrelation over time.

;; **Differencing** is the simplest transformation that will make a time series stationary. The formula
;; is as follows and is essentially a delta between the y values.
(clerk/tex "
\\begin{alignedat}{2}
y'_{t} = y_{t} - y_{t-1}
\\end{alignedat}
")

;; Most of the time differencing once or twice is sufficient to make a series stationary.
;; Logarithms stabilize the variance.

;; Forecasting a random walk on a long horizon does not make sense.
;; Forecasting the next timestep of a random walk is the only reasonable situation to tackle.


(def max-lag 1)
(def adf-test (math/augmented-dickey-fuller-test (double-array random-walk) max-lag))


(def acf (stats/acf random-walk 20))

(clerk/plotly
 {:data [{:type "scatter"
          :name "ACF Plot"
          :x (range 20)
          :y acf}]})
