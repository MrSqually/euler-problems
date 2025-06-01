(ns euler-problems.ui
  (:require [seesaw.core :as s]))

(defn problem-list []
  (s/listbox
   :model (range 100)))

(defn solution-widget []
  (s/top-bottom-split
   (s/left-right-split
    (s/label "TODO - parameters")
    (s/label "TODO - viz (optional)"))
   (s/label "TODO - source code")
   :divider-location 3/4))

(defn problem-panel []
  (s/top-bottom-split
   (s/label "TODO - problem statement")
   (solution-widget)))

(defn main-window []
  (s/left-right-split
   (problem-list)
   (problem-panel)
   :divider-location 1/10))

(defn app [title content]
  (s/invoke-later
   (->
    (s/frame :title title :content content :size [1920 :by 1080])
    s/show!)))

(app "Hello" (main-window))
