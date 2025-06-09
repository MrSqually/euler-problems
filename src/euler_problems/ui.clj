;; ---------------------------------------------------------------------------|
(ns euler-problems.ui
  (:require [seesaw.core :as s]
            [clj-http.client :as client]
            [clojure.repl :as repl]
            [clojure.pprint :refer [pprint] :as pp]
            [clojure.string :as str]
            [hickory.core :as hick]
            [hickory.select :as hsel]))

;; ---------------------------------------------------------------------------|
;; Internal Data Model

(def default-delim #"\;\;==================\|")

(defn load-ns-functions [ns-fname delimiter]
  (-> ns-fname
      slurp
      (str/split delimiter)
      (#(map (fn [n] (str/replace n "\n\n" "")) %))
      next))

(def problem-set
  (zipmap (iterate inc 1) (load-ns-functions "src/euler_problems/1_10.clj" default-delim)))

;; ---------------------------------------------------------------------------|
;; Widgets

(def problem-list
  "Widget --- Listbox for problems"
  (s/listbox :model (->> problem-set keys sort)))

(def problem-text
  "Widget --- Problem Description"
  (s/text :multi-line? true

          :text (slurp "resource/welcome.txt")))

(def source-panel
  "Widget --- Solution Source Code"
  (s/text :multi-line? true
          :text "Choose a problem on the left"
          :font "MONOSPACED-PLAIN-18"))

(def interaction-panel
  "Widget --- User Interaction"
  (s/text :text "User Interaction"))

(def solution
  (s/top-bottom-split
   (s/scrollable problem-text)
   (s/left-right-split
    (s/scrollable source-panel)
    interaction-panel
    :divider-location 1/2)
   :divider-location 1/3))

(def main-window
  "Widget --- Main Display Window"
  (s/left-right-split problem-list solution :divider-location 1/10))

(def root
  "Primary GUI Frame"
  (s/frame :title "Project Euler - Squally Solutions"
           :content main-window
           :size [1920 :by 1080]))

;; ---------------------------------------------------------------------------|
;; Helper Functions
(defn parse-ep-page
  ""
  [url]
  (->> url
       client/get
       :body
       hick/parse
       hick/as-hickory
       (hsel/select (hsel/child (hsel/tag :div)
                                (hsel/id :content)))))

;; ---------------------------------------------------------------------------|
;; Initialization
(defn init-problem [p-num]
  (->> (str "https://projecteuler.net/problem=" p-num)
       parse-ep-page
       (s/text! problem-text)))

(defn init-solution [p-num]
  (let [soln (problem-set p-num)
        params (nth (read-string soln) 3)]
    (s/text! source-panel soln)
    (init-problem p-num)
    params));;TODO problem text - html

(defn init-listeners
  "Wrapper to initialize all event callbacks"
  []
  (do
    ;; Problem List
    (s/listen problem-list :selection (fn [e] (when-let [v (s/selection e)]
                                                (init-solution v))))))

(defn app [frame]
  (s/invoke-later
   (->
    frame
    s/show!)))

(init-listeners)

(app root)
