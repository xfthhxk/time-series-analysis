;; This namespace follows https://github.com/marcopeix/TimeSeriesForecastingInPython/blob/master/CH02/CH02.ipynb
(ns tsa.ch2
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.column :as column]
            [tech.v3.datatype.functional :as dfn]
            [nextjournal.clerk :as clerk]))


;; ## 2.2 Forecasting Historical Mean
;; ### 2.2.1 Setup for baseline implementations

;; A `Dataset` is the logical equivalent of a Pandas `DataFrame` in Python. However, a dataset is
;; immutable where as the Pandas `DataFrame` is mutable.
(def ds (ds/->dataset "./resources/data/jj.csv" {:key-fn keyword}))



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
(ds/head ds)


;; `ds/tail` returns a new dataset of the last 5 entries in the dataset by default;
;; specify a count to control the number of rows
(ds/tail ds)


;; All but the last 4 entries will make up the training dataset.
(def train-ds (ds/head ds (-> ds ds/row-count (- 4))))

;; You could also use `ds/drop-rows`
(ds/drop-rows ds (range (- (ds/row-count ds) 4) (ds/row-count ds)))


;; The last 4 entries will make up the test dataset
(def test-ds (ds/tail ds 4))


;; ### 2.2.2 Implementing the historical mean baseline

;; Instead of `numpy` we'll use the `tech.v3.datatype.functional` namespace aliased as `dfn`
;; Functions in `dfn` are smart enough to operate on columns, scalars and combinations
(def historical-mean (dfn/mean (:data train-ds)))

;; A naive forecast with `historical-mean`. Here a new column is created with the name `:pred-data`
;; with the value `4` to match up with the rows in `test-ds`.  NB. A `(repeat historical-mean)` will causes
;; an `OutOfMemoryError`
(def naive-predictions-ds
  (ds/add-column test-ds (ds/new-column :pred-data (repeat 4 historical-mean))))


;; The Mean Absolute Percentage Error (MAPE) will be used to judge prediction accuracy.
(defn mape
  "Mean Absolute Percentage Error."
  [observations predictions]
  (-> (dfn/- observations predictions)
      (dfn// observations)
      dfn/abs
      dfn/mean
      (* 100)))


;; The predictions deviate by 70% from the observations
(def mape-historical-mean
  (mape (:data naive-predictions-ds)
        (:pred-data naive-predictions-ds)))



;; An extractor utility fn to get values from a dataset
(defn column-data
  ([ds col-name]
   (column-data ds col-name identity))
  ([ds col-name value-format-fn]
   (->> (get ds col-name)
        .-data
        (mapv value-format-fn))))


;; ### Visualizations
;; `:date` holds instances of `java.time.LocalDate` which is turned to a string for plotly
(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (column-data train-ds :date str)
          :y (column-data train-ds :data)}
         {:type "scatter"
          :name "Test"
          :x (column-data naive-predictions-ds :date str)
          :y (column-data naive-predictions-ds :data)}
         {:type "scatter"
          :name "Predicted"
          :x (column-data naive-predictions-ds :date str)
          :y (column-data naive-predictions-ds :pred-data)}]
  :layout {:title "Naive Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in naive-predictions-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in naive-predictions-ds [:date (dec (ds/row-count naive-predictions-ds))]))
                     :y1 (apply dfn/max (:data naive-predictions-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


;; ## 2.3 Forecasting Last Year's Mean
(def last-year-mean (dfn/mean (:data (ds/tail train-ds 4))))

(def last-year-mean-ds
  (ds/add-column test-ds (ds/new-column :pred-last-year-mean (repeat 4 last-year-mean))))

(def mape-last-year-mean (mape (:data last-year-mean-ds) (:pred-last-year-mean last-year-mean-ds)))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (column-data train-ds :date str)
          :y (column-data train-ds :data)}
         {:type "scatter"
          :name "Test"
          :x (column-data last-year-mean-ds :date str)
          :y (column-data last-year-mean-ds :data)}
         {:type "scatter"
          :name "Predicted"
          :x (column-data last-year-mean-ds :date str)
          :y (column-data last-year-mean-ds :pred-last-year-mean)}]
  :layout {:title "Last Year's Mean Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in last-year-mean-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in last-year-mean-ds [:date (dec (ds/row-count last-year-mean-ds))]))
                     :y1 (apply dfn/max (:data last-year-mean-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


;; ## 2.4 Predicting Using Last Known Value
(def last-eps (get-in train-ds [:data (dec (ds/row-count train-ds))]))

(def last-eps-ds
  (ds/add-column test-ds (ds/new-column :pred-last (repeat 4 last-eps))))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (column-data train-ds :date str)
          :y (column-data train-ds :data)}
         {:type "scatter"
          :name "Test"
          :x (column-data last-eps-ds :date str)
          :y (column-data last-eps-ds :data)}
         {:type "scatter"
          :name "Predicted"
          :x (column-data last-eps-ds :date str)
          :y (column-data last-eps-ds :pred-last)}]
  :layout {:title "Last EPS Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in last-eps-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in last-eps-ds [:date (dec (ds/row-count last-eps-ds))]))
                     :y1 (apply dfn/max (:data last-eps-ds))
                     :fillcolor "#d3d3d3"
                     :opacity 0.4
                     :line {:width 0}}]}})


(def mape-last (mape (:data last-eps-ds) (:pred-last last-eps-ds)))





;; ## 2.5 Naive Seasonal Forecast

;; This forecast projects 1979's Q1, Q2, Q3 and Q4 EPS to 1980's Q1, Q2, Q3 and Q4 EPS respectively

(def naive-seasonal-ds
  (ds/add-column test-ds
                 (-> train-ds
                     (ds/tail 4)
                     :data
                     (column/set-name :pred-last-season))))


(def mape-naive-seasonal (mape (:data naive-seasonal-ds) (:pred-last-season naive-seasonal-ds)))

(clerk/plotly
 {:data [{:type "scatter"
          :name "Train"
          :x (column-data train-ds :date str)
          :y (column-data train-ds :data)}
         {:type "scatter"
          :name "Test"
          :x (column-data naive-seasonal-ds :date str)
          :y (column-data naive-seasonal-ds :data)}
         {:type "scatter"
          :name "Predicted"
          :x (column-data naive-seasonal-ds :date str)
          :y (column-data naive-seasonal-ds :pred-last-season)}]
  :layout {:title "Last EPS Prediction"
           :xaxis {:title "Date"}
           :yaxis {:title "Earnings Per Share (USD)"}
           :shapes [{:type "rect"
                     :x0 (str (get-in naive-seasonal-ds [:date 0]))
                     :y0 0
                     :x1 (str (get-in naive-seasonal-ds [:date (dec (ds/row-count naive-seasonal-ds))]))
                     :y1 (apply dfn/max (:data naive-seasonal-ds))
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
