;; This namespace follows https://github.com/marcopeix/TimeSeriesForecastingInPython/blob/master/CH02/CH02.ipynb
(ns tsa.ch2
  (:require
   [tablecloth.api :as tc]
   [tablecloth.column.api :as tcc]
   [fastmath.stats :as stats]
   [nextjournal.clerk :as clerk]))


;; ## 2.2 Forecasting Historical Mean
;; ### 2.2.1 Setup for baseline implementations

;; A `Dataset` is the logical equivalent of a Pandas `DataFrame` in Python. However, a dataset is
;; immutable where as the Pandas `DataFrame` is mutable.
(def ds (tc/dataset "./resources/data/jj.csv" {:key-fn keyword}))



;; Note that the dataset does not show the row index like the Pandas dataframe output.
;; You can configure that by updating the `:transform-fn` for the Clerk viewer.
^{:nextjournal.clerk/visibility #{:hide}}
(clerk/code
 '(def dataset-table-viewer
   {:name 'tech.v3.dataset/viewer
    :pred ds/dataset?
    :transform-fn
    (clerk/update-val (fn [ds]
                        (clerk/table {:head (cons :idx (ds/column-names ds))
                                      :rows (map-indexed cons (ds/rowvecs ds))})))}))


;; `ds/head` returns a new dataset of the first 5 rows of the input dataset.
;;  You can specify an optional count of how many rows to include from the input dataset.
(tc/head ds)


;; `ds/tail` returns a new dataset of the last 5 entries in the dataset by default;
;; specify a count to control the number of rows
(tc/tail ds)


;; All but the last 4 entries will make up the training dataset.
(def train-ds (tc/head ds (-> ds tc/row-count (- 4))))

;; You could also use `tc/drop-rows`
(tc/drop-rows ds (range (- (tc/row-count ds) 4) (tc/row-count ds)))


;; The last 4 entries will make up the test dataset
(def test-ds (tc/tail ds 4))


;; ### 2.2.2 Implementing the historical mean baseline

;; Instead of `numpy` we'll use the `tablecloth.column.api` namespace aliased as `tcc`
;; Functions in `tcc` are smart enough to operate on columns, scalars and combinations
(def historical-mean (tcc/mean (:data train-ds)))

;; A naive forecast with `historical-mean`. Here a new column is created with the name `:pred-data`
;; with the value `4` to match up with the rows in `test-ds`.  NB. A `(repeat historical-mean)` will causes
;; an `OutOfMemoryError`
(def naive-predictions-ds
  (tc/add-column test-ds :pred-data (tcc/column (repeat 4 historical-mean))))


;; The Mean Absolute Percentage Error (MAPE) will be used to judge prediction accuracy.
;; The `fastmath.stats` namespace contains error quantification functions including `mape`


;; The predictions deviate by 70% from the observations
(def mape-historical-mean
  (* 100 (stats/mape (:data naive-predictions-ds)
                     (:pred-data naive-predictions-ds))))



;; ### Visualizations
;; `:date` holds instances of `java.time.LocalDate` which is turned to a string for plotly
(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (map str (:date train-ds))
          :y (-> train-ds :data .-data)}
         {:type "scatter"
          :name "Test"
          :x (map str (:date naive-predictions-ds))
          :y (-> naive-predictions-ds :data .-data)}
         {:type "scatter"
          :name "Predicted"
          :x (map str (:date naive-predictions-ds))
          :y (-> naive-predictions-ds :pred-data .-data)}]
  :layout {:title "Naive Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in naive-predictions-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in naive-predictions-ds [:date (dec (tc/row-count naive-predictions-ds))]))
                     :y1 (apply tcc/max (:data naive-predictions-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


;; ## 2.3 Forecasting Last Year's Mean
(def last-year-mean (tcc/mean (:data (tc/tail train-ds 4))))

(def last-year-mean-ds
  (tc/add-column test-ds :pred-last-year-mean  (tcc/column (repeat 4 last-year-mean))))

(def mape-last-year-mean
  (* 100 (stats/mape (:data last-year-mean-ds)
                     (:pred-last-year-mean last-year-mean-ds))))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (map str (:date train-ds))
          :y (-> train-ds :data .-data)}
         {:type "scatter"
          :name "Test"
          :x (map str (:date last-year-mean-ds))
          :y (-> last-year-mean-ds :data .-data)}
         {:type "scatter"
          :name "Predicted"
          :x (map str (:date last-year-mean-ds))
          :y (-> last-year-mean-ds :pred-last-year-mean .-data)}]
  :layout {:title "Last Year's Mean Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in last-year-mean-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in last-year-mean-ds [:date (dec (tc/row-count last-year-mean-ds))]))
                     :y1 (apply tcc/max (:data last-year-mean-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


;; ## 2.4 Predicting Using Last Known Value
(def last-eps (get-in train-ds [:data (dec (tc/row-count train-ds))]))

(def last-eps-ds
  (tc/add-column test-ds :pred-last (tcc/column (repeat 4 last-eps))))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (map str (:date train-ds))
          :y (-> train-ds :data .-data)}
         {:type "scatter"
          :name "Test"
          :x (map str (:date last-eps-ds))
          :y (-> last-eps-ds :data .-data)}
         {:type "scatter"
          :name "Predicted"
          :x (map str (:date last-eps-ds))
          :y (-> last-eps-ds :pred-last .-data)}]
  :layout {:title "Last EPS Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in last-eps-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in last-eps-ds [:date (dec (tc/row-count last-eps-ds))]))
                     :y1 (apply tcc/max (:data last-eps-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


(def mape-last
  (* 100 (stats/mape (:data last-eps-ds)
                     (:pred-last last-eps-ds))))





;; ## 2.5 Naive Seasonal Forecast

;; This forecast projects 1979's Q1, Q2, Q3 and Q4 EPS to 1980's Q1, Q2, Q3 and Q4 EPS respectively

(def naive-seasonal-ds
  (tc/add-column test-ds :pred-last-season (-> train-ds (tc/tail 4) :data)))


(def mape-naive-seasonal
  (* 100 (stats/mape (:data naive-seasonal-ds)
                     (:pred-last-season naive-seasonal-ds))))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (map str (:date train-ds))
          :y (-> train-ds :data .-data)}
         {:type "scatter"
          :name "Test"
          :x (map str (:date naive-seasonal-ds))
          :y (-> naive-seasonal-ds :data .-data)}
         {:type "scatter"
          :name "Predicted"
          :x (map str (:date naive-seasonal-ds))
          :y (-> naive-seasonal-ds :pred-last-season .-data)}]
  :layout {:title "Last EPS Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in naive-seasonal-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in naive-seasonal-ds [:date (dec (tc/row-count naive-seasonal-ds))]))
                     :y1 (apply tcc/max (:data naive-seasonal-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})



;; ### MAPE of Baselines
(clerk/plotly
 {:data [{:type "bar"
          :x ["Historical Mean" "Last Year Mean" "Last" "Naive Seasonal"]
          :y [mape-historical-mean mape-last-year-mean mape-last mape-naive-seasonal]
          :text (map #(format "%.2f" %)
                     [mape-historical-mean mape-last-year-mean mape-last mape-naive-seasonal])}]
  :layout {:title "Baseline MAPEs"
           :xaxis {:title "Baselines"}
           :yaxis {:title "MAPE (%)"}}})
