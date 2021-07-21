(ns sevenguis.components
  (:require [sevenguis.utils :as utils]))

;; -------------------------
;; UI Components

(defn ui-task
  "Component to wrap a single task"
  [title task]
  [:div {:class "UI-task"}
   [:h2 title]
   [:div {:class "UI-task-container"} [task]]])


(defn ui-button
  "Button with an on-click event handler"
  ([title on-click]
   (ui-button title on-click false))
  ([title on-click disabled]
   [:input (merge
            {:type     "button"
             :class    "UI-button"
             :value    title
             :on-click #(on-click)}
            (when disabled
              {:disabled true}))]))


(defn ui-input
  "Simple input element"
  [input-type input-value]
  [:input {:type      input-type
           :value     @input-value
           :class     "UI-input"
           :on-change #(reset! input-value (-> % .-target .-value))}])


(defn ui-list-box
  "List box made of select and different options"
  [items on-change]
  [:select {:size      4
            :class     "UI-list-box"
            :on-change on-change}
   (for [index (range (count items))]
     (let [item      (get items index)
           item-name (get item :name)
           item-last (get item :surname)
           item-key  (str "item-"
                          (utils/escape-str item-name)
                          "-"
                          (utils/escape-str item-last))
           item-str  (str item-name ", " item-last)]
       [:option {:name  item-key
                 :key   item-key
                 :value index} item-str]))])


(defn input-change
  "An full input element with callback on change, class, validation, disable, etc."
  ([input-name input-type input-class input-value on-change-evt]
   (input-change input-name input-type input-class input-value on-change-evt true false))
  ([input-name input-type input-class input-value on-change-evt is-valid]
   (input-change input-name input-type input-class input-value on-change-evt is-valid false))
  ([input-name input-type input-class input-value on-change-evt is-valid disabled]
   [:input (merge {:type      input-type
                   :name      input-name
                   :value     @input-value
                   :class     (str "UI-input " input-class (when (false? is-valid) " input-error"))
                   :on-change #(on-change-evt %)}
                  (when disabled
                    {:disabled true}))]))


(defn input-range
  [min max value on-change]
  [:input {:type      "range"
           :min       min
           :max       max
           :value     value
           :on-change on-change}])
