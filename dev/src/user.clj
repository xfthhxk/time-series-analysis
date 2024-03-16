(ns user
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [nextjournal.clerk.viewer :as v]
            [tech.v3.dataset :as ds]))

;; transform-fn runs on the server, render-fn on the ui
(def dataset-table-viewer
  {:name 'tech.v3.dataset/viewer
   :pred ds/dataset?
   :transform-fn (clerk/update-val (fn [ds]
                                     (clerk/table {:head (ds/column-names ds)
                                                   :rows (ds/rowvecs ds)})))})

(def local-date-viewer
  {:name 'java.time/local-date-viewer
   :pred (fn [x] (instance? java.time.LocalDate x))
   :transform-fn (clerk/update-val str)})



(defn add-viewers! [viewers]
  ;; add global viewers by using :default else only available in the namespace add-viewers is called from via `*ns*`
  (clerk/reset-viewers! :default (clerk/add-viewers (clerk/get-default-viewers) viewers)))


(add-viewers! [dataset-table-viewer local-date-viewer])


;; start Clerk's built-in webserver on the default port 7777, opening the browser when done

;; start with watcher and show filter function to enable notebook pinning
(clerk/serve! {:host "0.0.0.0"
               :port 7777
               :watch-paths ["notebooks" "src"]
               :show-filter-fn #(str/starts-with? % "notebooks")})

;; Build a html file from the given notebook notebooks.
;; See the docstring for more options.
;; (clerk/build! {:paths ["notebooks/rule_30.clj"]})
(comment





)
