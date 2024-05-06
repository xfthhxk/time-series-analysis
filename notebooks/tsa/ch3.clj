;; This namespace follows https://github.com/marcopeix/TimeSeriesForecastingInPython/blob/master/CH03/CH03.ipynb
(ns tsa.ch3
  (:require [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [tsa.math :as math]
            [clojure.string :as str]
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
(def adf-test (math/augmented-dickey-fuller-test random-walk max-lag))


(def lags 20)
(def acf (stats/acf random-walk lags))
(def acf-bounds (math/acf-bounds acf))

;; This plot does not look like the python plot but the core idea is that
;; the autocorrelation decreases slowly and is outside of the upper and lower
;; bounds which represents a confidence interval.
;; **TODO: Make a better plot.**
(clerk/plotly
 {:data [{:type "bar"
          :name "ACF Plot"
          :x (range lags)
          :y acf
          :width (repeat lags 0.1)}
         {:type "scatter"
          :name "Upper"
          :x (range lags)
          :y (repeat lags (:upper acf-bounds))}
         {:type "scatter"
          :name "Lower"
          :x (range lags)
          :y (repeat lags (:lower acf-bounds))}]
  :layout {:scattermode "group"
           :barcornerradius 50}})


(def diff-random-walk (math/diff random-walk 1))

(clerk/plotly
 {:data [{:type "bar"
          :x (range (count diff-random-walk))
          :y (.-data diff-random-walk)
          :width (repeat (count diff-random-walk) 0.1)}]})


;; The p-value is less than 0.05 and the statistic is negative so it is stationary.
;; NB. the book's ADF is -31.79
(def diff-adf-test (math/augmented-dickey-fuller-test diff-random-walk max-lag))

(def diff-acf (stats/acf diff-random-walk lags))

(def diff-acf-bounds (math/acf-bounds diff-acf))


;; Now there aren't any significant autocorrelation coefficients after lag 0 which will always be 1.
(clerk/plotly
 {:data [{:type "bar"
          :name "Diff ACF Plot"
          :x (range lags)
          :y diff-acf
          :width (repeat lags 0.1)}
         {:type "scatter"
          :name "Upper"
          :x (range lags)
          :y (repeat lags (:upper diff-acf-bounds))}
         {:type "scatter"
          :name "Lower"
          :x (range lags)
          :y (repeat lags (:lower diff-acf-bounds))}]
  :layout {:scattermode "group"
           :barcornerradius 50}})

;; ## GOOGL Example
;; Data from 27 April 2020 to 27 April 2021
;; Lowercase and eywordize keys. Also replace spaces with a '-' to turn "Adj Close" to `:adj-close`
(def googl-ds (tc/dataset "resources/data/GOOGL.csv"
                          {:key-fn (comp keyword
                                      #(str/replace % " " "-")
                                      str/lower-case)}))


(clerk/plotly
 {:data [{:type "scatter"
          :name "Closing Price"
          :x (map str (:date googl-ds))
          :y (.-data (:close googl-ds))}]
  :layout {:title "GOOGL 2020-04-27 to 2021-04-27"
           :xaxis {:title "Date"}
           :yaxis {:title "Close"}}})

;; Data looks to be trending. Let's confirm with ADF test.
;; NB. The book's ADF statistic is `0.16`. Regardless, we can see this is not a stationary series.
(def googl-adf-test (-> googl-ds
                        :close
                        (math/augmented-dickey-fuller-test max-lag)))


(def googl-diff-close (math/diff (:close googl-ds) 1))



;; Note the very tiny `p-value` after differencing
(def googl-diff-adf-test (math/augmented-dickey-fuller-test googl-diff-close max-lag))


;; Next look at the ACF
(def googl-acf (stats/acf googl-diff-close lags))
(def googl-acf-bounds (math/acf-bounds googl-acf))

(clerk/plotly
 {:data [{:type "bar"
          :name "GOOGL ACF Plot"
          :x (range lags)
          :y googl-acf
          :width (repeat lags 0.1)}
         {:type "scatter"
          :name "Upper"
          :x (range lags)
          :y (repeat lags (:upper googl-acf-bounds))}
         {:type "scatter"
          :name "Lower"
          :x (range lags)
          :y (repeat lags (:lower googl-acf-bounds))}]
  :layout {:scattermode "group"
           :barcornerradius 50}})


;; GOOGL first difference is stationary and ACF doesn't show any correlation so it is a random walk.

(def ds (tc/dataset {:value random-walk}))

(def train-ds (tc/head ds 800))
;; drop rows that are in the supplied seq
(def test-ds (tc/drop-rows ds (range 800)))


;; ## Baselines
;; ### Carry the mean forward
(def train-mean (tcc/mean (:value train-ds)))
(def test-ds-1 (tc/add-column test-ds :pred-mean (-> test-ds
                                                     tc/row-count
                                                     (repeat train-mean)
                                                     tcc/column)))

(tc/head test-ds-1)


;; ### Last known value of the train set carried forward
(def test-ds-2 (tc/add-column test-ds-1 :pred-last (-> test-ds-1
                                                       tc/row-count
                                                       (repeat (-> train-ds :value last))
                                                       tcc/column)))

;; ### Drift Method
;; Slope between the first and last values in the train dataset.
(def delta-x (dec 800))
(def delta-y (- (-> train-ds :value last) 0))
(def drift (/ delta-y delta-x))

(def x-vals (range 801 1001))

(def pred-drift (tcc/* drift x-vals))
(def test-ds-3 (tc/add-column test-ds-2 :pred-drift pred-drift))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (range (tc/row-count ds))
          :y (.-data (:value ds))}
         {:type "scatter"
          :name "Mean"
          :x (range 800 (+ 800 (tc/row-count test-ds-3)))
          :y (.-data (:pred-mean test-ds-3))}
         {:type "scatter"
          :name "Last"
          :x (range 800 (+ 800 (tc/row-count test-ds-3)))
          :y (.-data (:pred-last test-ds-3))}
         {:type "scatter"
          :name "Drift"
          :x (range 800 (+ 800 (tc/row-count test-ds-3)))
          :y (.-data (:pred-drift test-ds-3))}]
  :layout {:title "Forecast"
           :xaxis {:title "Timesteps"}
           :yaxis {:title "Value"}
           :shapes [{:type "rect"
                     :x0 800
                     :y0 (apply tcc/min (:value ds))
                     :x1 (tc/row-count ds)
                     :y1 (apply tcc/max (:value ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


(def mse-mean (stats/mse (:value test-ds-3) (:pred-mean test-ds-3)))
(def mse-last (stats/mse (:value test-ds-3) (:pred-last test-ds-3)))
(def mse-drift (stats/mse (:value test-ds-3) (:pred-drift test-ds-3)))


;; MSE of the forecasts
(clerk/plotly
 {:data [{:type "bar"
          :x ["Mean" "Last" "Drift"]
          :y [mse-mean mse-last mse-drift]
          :width (repeat 3 0.5)
          :text (map #(format "%.2f" %)
                     [mse-mean mse-last mse-drift])}]
  :layout {:title "MSE of Forecasts"
           :xaxis {:title "Methods"}
           :yaxis {:title "MSE"}}})


;; ### 3.3.2 Forecasting the next timestep

;; Create a new column named `:forecast` using `:value` shifted by 1
(def ds-shift (tc/shift ds :forecast :value 1))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Actual"
          :x (range (tc/row-count ds-shift))
          :y (.-data (:value ds-shift))}
         {:type "scatter"
          :name "Forecast"
          :x (range (tc/row-count ds-shift))
          :y (.-data (:forecast ds-shift))}]
  :layout {:title "Forecast"
           :xaxis {:title "Timesteps"}
           :yaxis {:title "Value"}}})
