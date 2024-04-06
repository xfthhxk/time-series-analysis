;; This namespace follows https://github.com/marcopeix/TimeSeriesForecastingInPython/blob/master/CH03/CH03.ipynb
(ns tsa.ch3
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as column]
            [tech.v3.datatype.functional :as dfn]
            [nextjournal.clerk :as clerk]
            [fastmath.random :as random]
            ))

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


(def steps
  (let [d (-> random/default-normal
              (random/set-seed! 42))]
    (->> (repeatedly 1000 #(random/sample d))
         (drop 1)
         (into [0]))))


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
