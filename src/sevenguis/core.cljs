(ns sevenguis.core
  (:require
   [reagent.core         :as r]
   [reagent.dom          :as rdom]
   [clojure.string       :as str]
   [clojure.spec.alpha   :as s]
   [cljs.pprint          :as pprint]
   [sevenguis.utils      :as utils]
   [sevenguis.components :as components]))

;; -------------------------
;; Task - Counter

(def click-count1 (r/atom 0))


(defn counter []
  [:div.UI-task-center
   [:label.UI-counter @click-count1]
   [components/ui-button "Counter" #(swap! click-count1 inc)]])

;; -------------------------
;; Task - Temperature Converter

(def cels       (r/atom ""))
(def cels-valid (r/atom true))
(def fahr       (r/atom ""))
(def fahr-valid (r/atom true))


(defn cels->fahr
  [temp-str]
  (let [temp (utils/parse-int temp-str)]
    (Math/round (+ (* temp (/ 9 5)) 32))))


(defn fahr->cels [temp-str]
  (let [temp (utils/parse-int temp-str)]
    (Math/round (* (- temp 32) (/ 5 9)))))


(defn temperature?
  "Check if the input string is a valid value for temperature"
  [val]
  ;; The regex allows an initial `-` and at list one number
  (and (not (nil? val))
       (boolean (re-find #"^-?\d+$" val))))


(defn temp-converter []
  [:div.UI-task-center
   [:div.center-h.margin-h
    [components/input-change "input-celsius"
     "text"
     "UI-input-right"
     cels
     (fn [e]
       (let [input-value (-> e .-target .-value)
             valid-value (or (temperature? input-value)
                             (str/blank? input-value))]
         (reset! cels-valid valid-value)
         (reset! cels       input-value)
         (when (and valid-value
                    (not (str/blank? input-value)))
           (reset! fahr (cels->fahr input-value)))))
     @cels-valid]
    [:label.UI-label-big "C"]]
   [:div.center-h.margin-h
    [components/input-change "input-fahreneith"
     "text"
     "UI-input-right"
     fahr
     (fn [e]
       (let [input-value (-> e .-target .-value)
             valid-value (or (temperature? input-value)
                             (str/blank? input-value))]
         (reset! fahr-valid valid-value)
         (reset! fahr       input-value)
         (when (and valid-value
                    (not (str/blank? input-value)))
           (reset! cels (fahr->cels input-value)))))
     @fahr-valid]
    [:label.UI-label-big "F"]]])

;; -------------------------
;; Task - Flight Booker

(def booking-type     (r/atom "single")) ; or double

(def start-date (r/atom (utils/format-date utils/date-now)))
(def end-date   (r/atom (utils/format-date utils/date-now)))


(defn str-to-date
  "Transform a date string (dd-MM-yyyy) into a Date object"
  [date-str]
  (let [coll (str/split date-str #"\.")]
    (js/Date. (str (coll 2) "-" (coll 1) "-" (coll 0)))))


(defn valid-date?
  "Check if the string contains a valid formatted date"
  [val]
  ;; Regex for date-string like "dd.MM.yyyy"
  ;; The exercise doesn't require validation to check if the date is before today
  (boolean (re-find #"^[\d]{2}.[\d]{2}.[\d]{4}$" val)))


(defn end-after-start?
  "Check that end date is after start date"
  [start-date end-date]
  (>= (str-to-date end-date) (str-to-date start-date)))


(defn one-way?
  "True if the booking is single way"
  [booking-type]
  (= @booking-type "single"))


(defn flight-combo
  "Combobox for choose the type of flight"
  [booking-type]
  [:select {:name      "flight-combobox"
            :value     @booking-type
            :on-change #(reset! booking-type (-> % .-target .-value))}
   [:option {:value "single"} "one-way flight"]
   [:option {:value "double"} "return flight"]])


(defn flight-booker []
  (let [start-valid (valid-date? @start-date)
        end-valid   (valid-date? @end-date)
        btn-enabled (or (and (one-way? booking-type) start-valid)
                        (and (not (one-way? booking-type))
                             start-valid
                             end-valid
                             (end-after-start? @start-date @end-date)))]
    [:div.UI-task-center
     [flight-combo booking-type]
     [:div#spacer]
     [:div
      [:label "Start date: "]
      [components/input-change
       "start-date"
       "text"
       "UI-input-center"
       start-date
       #(reset! start-date (-> % .-target .-value))
       start-valid]]
     [:div
      [:label "End date: "]
      [components/input-change
       "end-date"
       "text"
       "UI-input-center"
       end-date
       #(reset! end-date (-> % .-target .-value))
       end-valid
       (one-way? booking-type)]]
     [:div#spacer]
     [:button.UI-button
      {:disabled (not btn-enabled)
       :on-click (fn []
                   (let [msg (str "You have booked a return flight from "
                                  @start-date
                                  (when (not (one-way? booking-type))
                                    (str " to " @end-date)))]
                     (js/alert msg)))}
      ;; Disable the button if the end-date is before the start-date or start-date is not valid
      "Book"]]))

;; -------------------------
;; Task - Timer

(def timer-duration (r/atom 15))
(def timer-value    (r/atom 0))


(defn round
  "Round a number to precision decimal values."
  [number, precision]
  (let [scale (Math/pow 10 precision)]
    (-> number
        (* scale)
        Math/round
        (/ scale))))


(defn display-timer []
  (let [time-str (.toFixed @timer-value 1)
        meter-value (if (> @timer-duration 0)
                      (/ @timer-value @timer-duration)
                      1)]
    [:div.UI-task-center
     [:div.UI-task-section
      [:span.UI-label "Elapsed Time"]
      [:meter {:value meter-value
               :style {:flex "1 1 0%"}}]]
     [:div.UI-task-section
      [:span (str time-str "s")]]
     [:div.UI-task-section
      [:span.UI-label "Duration"]
      [components/input-range 0 30 @timer-duration #(reset! timer-duration (-> % .-target .-value))]]
     [:div#spacer]
     [:div
      [components/ui-button "Reset Timer" #(reset! timer-value 0)]]]))


(defn update-timer []
  (swap! timer-value
         (fn [prev-value]
           ;; prevValue >= duration ? prevValue : round(prevValue + 0.1, 1)
           (if (>= prev-value @timer-duration)
             prev-value
             (round (+ prev-value 0.1) 1)))))


(defn timer []
  (js/setInterval update-timer 100)
  [display-timer])

;; -------------------------
;; Task - CRUD

(def crud-filter  (r/atom ""))
(def crud-name    (r/atom ""))
(def crud-surname (r/atom ""))
(def crud-items   (r/atom [{:name     "Harry"
                            :surname  "Potter"}
                           {:name     "Hermione"
                            :surname  "Granger"}
                           {:name     "Ron"
                            :surname  "Weasley"}]))
(def crud-selected-item (r/atom nil))


(defn filter-items [items, filter-str]
  (let [filter-lower (str/lower-case filter-str)]
    (into [] (filter #(or (str/includes? (str/lower-case (get % :surname)) filter-lower)
                          (str/includes? (str/lower-case (get % :name))    filter-lower))
                     items))))


(defn crud []
  [:div.UI-task-center
   [:div.UI-task-section
    [:span.UI-label "Filter prefix:"]
    [components/ui-input "text" crud-filter]]
   [:div.UI-task-section.UI-task-section--with-spaces
    [components/ui-list-box
     (filter-items @crud-items @crud-filter)
     #(reset! crud-selected-item (-> % .-target .-value))]
    [:div.UI-space-around
     [:div.UI-form-inline
      [:span.UI-label "Name:"]
      [components/ui-input "text" crud-name]]
     [:div.UI-form-inline
      [:span.UI-label "Surname:"]
      [components/ui-input "text" crud-surname]]]]
   [:div#spacer]
   [:div.UI-task-section
    [components/ui-button "Create" (fn []
                          (when (and (seq @crud-name)
                                     (seq @crud-surname))
                            (swap! crud-items
                                   conj {:name    @crud-name
                                         :surname @crud-surname})
                            (reset! crud-name "")
                            (reset! crud-surname "")))]
    [components/ui-button "Update" (fn []
                          (when (and (seq @crud-name)
                                     (seq @crud-surname))
                            (swap! crud-items
                                   assoc
                                   (int @crud-selected-item)
                                   {:name    @crud-name
                                    :surname @crud-surname})))]
    [components/ui-button "Delete" (fn []
                          (let [item-idx (int @crud-selected-item)]
                            (when (and (>= item-idx 0) (< item-idx (count @crud-items)))
                              (swap! crud-items utils/vec-remove item-idx))))]]])

;; -------------------------
;; Task - Circle Drawer

;; Circle map definition
;;   {:x  (-> e .-nativeEvent .-offsetX
;;    :y  (-> e .-nativeEvent .-offsetY)
;;    :d  30
;;    :id (.getTime (js/Date.)))}
(def circles-state   (r/atom {:history-idx 0
                              :history     [[]]}))

(def selected-circle (r/atom nil))

;; possible frames state are:
;; 0 - hidden
;; 1 - menu options
;; 2 - slider
(def current-diameter-frame (r/atom {:state    0
                                     :position {:x 0 :y 0}}))


(defn reset-diameter-frame []
  (reset! current-diameter-frame {:state    0
                                  :position {:x 0 :y 0}}))


(defn current-state
  "Return the current history state"
  []
  (get-in @circles-state [:history (:history-idx @circles-state)]))


(defn delete-future
  "Delete future history from the current index"
  []
  ;; check if there's future to delete, to avoid subvec errors
  (when (not= (count (:history @circles-state))
              (dec (:history-idx @circles-state)))
    (swap! circles-state update-in [:history] (fn [past-history]
                                                (subvec past-history
                                                        0
                                                        ;; We incr the history index because the first
                                                        ;; state an empty array, but count as 1
                                                        (inc (:history-idx @circles-state)))))))


(defn add-state
  "Create a state using the function and append it to the history"
  [create-state]
  (delete-future)
  (swap! circles-state update-in [:history-idx] inc)
  ;; The future is a function of the past - Rich Hickey
  ;; We run the creator function that get the last state of the app and append his modification
  ;; returning a new state that will be appended in the history.
  (swap! circles-state update-in [:history] (fn [past-history]
                                              (conj past-history
                                                    (create-state (peek past-history))))))


(defn undo
  "Decrement the history index if > 0"
  []
  (when (> (:history-idx @circles-state) 0)
    (swap! circles-state update-in [:history-idx] dec)))


(defn redo
  "Increment the history index if < length of history"
  []
  (when (< (inc (:history-idx @circles-state))
           (count (:history @circles-state)))
    (swap! circles-state update-in [:history-idx] inc)))


(defn add-circle
  "Create a new circle and push it into the application state history"
  [e]
  (let [new-circle {:x  (-> e .-nativeEvent .-offsetX)
                    :y  (-> e .-nativeEvent .-offsetY)
                    :d  30 ; default circle diameter
                    :id (.getTime (js/Date.))}]
    (add-state (fn [prev-state]
                 (conj prev-state new-circle)))))


(defn show-circle-menu [e circle]
  (let [pos {:x  (-> e .-nativeEvent .-pageX)
             :y  (-> e .-nativeEvent .-pageY)}]
    (utils/prevent-default e)
    (reset! selected-circle circle)
    (reset! current-diameter-frame {:state 1
                                    :position pos})))


(defn clone-current-history
  "Clone the current history in a new state"
  []
  (let [idx (.indexOf (current-state) @selected-circle)]
    (add-state (fn [prev-state] (assoc prev-state idx @selected-circle)))))


(defn update-current-range
  "Update the current range only into the history, without adding a new one"
  [e]
  (let [d   (int (-> e .-target .-value))                ; integer diameter
        idx (.indexOf (current-state) @selected-circle)] ; index of selected circle into app state
    (swap! circles-state assoc-in [:history (:history-idx @circles-state) idx :d] d)
    (swap! selected-circle assoc-in [:d] d)))


(defn circle-menu-2 []
  (r/with-let [pos        (:position @current-diameter-frame)
               ref        (atom nil) ; menu element ref
               handler    (fn [e]
                            (if (and @ref (not (.contains @ref (.-target e))))
                              (reset-diameter-frame)
                              (utils/prevent-default e)))
               _          (.addEventListener js/document "click" handler)]
    [:div.UI-menu {:style {:top  (:y pos) :left (:x pos)}
                   :ref   #(reset! ref %)}
     "Adjust Diameter"
     ;; Update the diameter in the history only when the user stop dragging the slider,
     ;; otherwhise update it only "locally"
     [:input {:type        "range"
              :min         2
              :max         100
              :value       (:d @selected-circle)
              :on-change   update-current-range
              :on-mouse-down clone-current-history}]]
    (finally
      (.removeEventListener js/document "click" handler))))


(defn circle-menu-1 []
  (r/with-let [pos        (:position @current-diameter-frame)
               ref        (atom nil) ; menu element ref
               handler    (fn [e]
                            (if (and @ref (not (.contains @ref (.-target e))))
                              (reset-diameter-frame)
                              (swap! current-diameter-frame update-in [:state] inc)))
               _          (.addEventListener js/document "click" handler)]
    [:div.UI-menu {:style {:top  (:y pos) :left (:x pos)}
                   :ref   #(reset! ref %)}
     "Adjust Diameter"]
    (finally
      (.removeEventListener js/document "click" handler))))


(defn circle-drawer []
  [:div.UI-circle-drawer
   [:div.UI-form-inline
    [components/ui-button "Undo" #(undo)]
    [components/ui-button "Redo" #(redo)]]
   [:div.UI-canvas {:on-click add-circle}
    (for [circle (current-state)]
      [:div.UI-circle {:key (:id circle)
                       :data-attr ()
                       :on-click #(show-circle-menu % circle)
                       :style {:top    (:y circle)
                               :left   (:x circle)
                               :width  (:d circle)
                               :height (:d circle)}}])]
   ;; show on not the diameter range
   (case (:state @current-diameter-frame)
     2 [circle-menu-2]
     1 [circle-menu-1]
     nil)])

;; -------------------------
;; Task - Cells

;; Define the table sizes as costants
(def table-size {:cols (map char (range (.charCodeAt \A 0) (inc (.charCodeAt \Z 0))))
                 :rows (range 0 100)})


;; Save the state of each cell using a keyword {:A0 {} :A1 {} ...}
;; in this way is cheap updating and retriving data using one single atom
(def table-state (r/atom {:A0 {:value "1" :computed 1}
                          :A1 {:value "2" :computed 2}
                          :A2 {:value "=SUM(A0:A1, 2)" :computed 5}}))

;; List of cells that a cell computed formula depends on
(def cell-dependencies (r/atom {}))

(def active-cell (r/atom nil))    ; Current active cell
(def edit-mode   (r/atom false)) ; Active cell is in editing mode


(defn table-cell [col row]
  (let [coord (str col row)]
    (get @table-state (keyword coord))))


(def operations
  {:SUM #(apply + %)
   :SUB #(apply - %)
   :MUL #(apply * %)
   :DIV #(apply / %)
   :AVG #(/ (apply + %)
            (count %))})


(defn formula?
  "True if the cell value is a formula"
  [s]
  (= (first s) "="))


;; A0, A0:B4
(defn coord?
  "True if the string is a valid coordinate"
  [c]
  (when (string? c)
    (boolean (re-find #"^[A-Z]{1}[0-9]{1,2}$" c))))


(defn operation?
  "True if the formula contains a valid operation"
  [formula]
  (and (s/valid? :formula/op (subs formula 1 4))
       (let [rest (subs formula 4)]
         (and (= (first rest) "(")
              (= (last rest) ")")))))


(defn range?
  "True if the string is a valid range"
  [r]
  (and (string? r)
       (= (count r) 5)
       (= (nth r 2) ":")
       (coord? (subs r 0 2))
       (coord? (subs r 3 5))))


;; Integer and Decimal number
(s/def :type/number
  #(boolean (re-find #"^\d*\.?\d+$" %)))


;; SUM, SUB, MUL, DIV, AVG
(s/def :formula/op
  (set (map #(subs (str %) 1) (keys operations))))


(defn cell-value
  "Get cell value using coordinates"
  [coord table-state]
  (let [cell     (get @table-state (keyword coord))
        computed (get cell :computed)
        value    (get cell :value)]
    (or computed
        (when (and (seq value)
                   (s/valid? :type/number value))
          value))))


(defn update-dependencies
  "Add a dependency to the cell dependencies state"
  [dep coord cell-dependencies]
  (swap! cell-dependencies update-in [(keyword coord)] conj dep))


(defn coord-from-range
  "Get column coordinate using spec map"
  [r]
  (let [col (pprint/char-code (first r))
        row (utils/parse-int (last r))]
    {:col col :row row}))


(defn parse-range
  "Parse and return a range"
  [value coord table-state cell-dependencies]
  (let [parts (str/split value #"\:")
        start (coord-from-range (nth parts 0))
        end   (coord-from-range (nth parts 1))]
    ;; iterate for each column,row store the cell value into an array
    (for [c (into [] (range (:col start) (inc (:col end))))
          r (into [] (range (:row start) (inc (:row end))))]
      (let [cell-coord (str (char c) r)
            value      (cell-value cell-coord table-state)]
        (update-dependencies cell-coord coord cell-dependencies)
        (or (when (or (number? value) (s/valid? :type/number value))
              (utils/parse-float value)) 0)))))


(defn parse-args
  "Parse operation arguments"
  [params coord table-state cell-dependencies]
  (let [args (str/split params #",")]
    (if (> (count args) 1)
      ;; parse all args and store them in a single vector
      ;; in case or range, `flatten` remove the nested vector
      (into [] (flatten (map #(parse-args % coord table-state cell-dependencies) args)))
      (let [arg (str/trim (first args))]
        ;; compute numbers, ranges, cells
        (cond
          (s/valid? :type/number arg) (utils/parse-float arg)
          (s/valid? range? arg)       (parse-range arg coord table-state cell-dependencies)
          (s/valid? coord? arg)       (do
                                        (update-dependencies arg coord cell-dependencies)
                                        ;; convert string number into numeric values
                                        (let [value (cell-value arg table-state)]
                                          (if (or (number? value) (s/valid? :type/number value))
                                            (utils/parse-float value)
                                            value))))))))


;; Formulas Example:
;; =A0
;; =SUM(A0:B12)
;; =SUM(A0:B4, 12)
(defn compute-formula
  "Return the computed cell formula content 
   and update the cell-dependencies state while parsing the arguments."
  [formula coord table-state cell-dependencies]
  (if (operation? formula)
    ;; if formula contains operations, apply them to the list of arguments
    (let [op     (get operations (keyword (subs formula 1 4)))
          params (subs formula 5 (dec (count formula)))
          args   (parse-args params coord table-state cell-dependencies)]
      (op args))
    ;; otherwise, if argument is cell-coord, get that cell value
    (let [arg (subs formula 1)]
      (when (s/valid? coord? arg)
        ((update-dependencies arg coord cell-dependencies)
         (println (cell-value arg table-state))
         (cell-value arg table-state))))))


(defn change-cell-value [col row value]
  (let [coord (str col row)]
    ;; update/create the cell value in the table-state
    (swap! table-state assoc-in [(keyword coord) :value] value)
    ;; reset cell dependencies array
    (swap! cell-dependencies assoc (keyword coord) [])
    (if (formula? value)
      ;; compute formula or set :computed to nil
      (let [computed (compute-formula value coord table-state cell-dependencies)
            formatted (if (float? computed)
                        (js/parseFloat (.toFixed computed 5))
                        computed)]
        (swap! table-state
               assoc-in
               [(keyword coord) :computed]
               formatted))
      (swap! table-state assoc-in [(keyword coord) :computed] nil))
    ;; re-compute all depented cell value
    (doseq [[k v] @cell-dependencies]
      ;; only when other cells are dependent on this one
      (when (some #(= coord %) v)
        (swap! table-state
               assoc-in
               [(keyword k) :computed]
               (compute-formula (get-in @table-state [(keyword k) :value])
                                k
                                table-state
                                cell-dependencies))))))


(defn handle-cell-change [col row value]
  (change-cell-value col row value)
  (reset! edit-mode false))


(defn set-active-cell [col row]
  (let [coord (str col row)]
    (reset! active-cell coord)
    (reset! edit-mode true)))


(defn cells []
  [:div
   [:div.UI-table-container
    [:table.UI-table
     [:tbody
      [:tr.UI-table-tr-th
       [:th.UI-table-th.UI-table-th-small]
       (doall
        (map
         (fn [col]
           [:th.UI-table-th {:key (str "header-" col)}
            [:div.UI-table-cell-header col]])
         (:cols table-size)))]
      ;; used doall for performance reasons
      (doall
       (map
        (fn [row]
          [:tr {:key (str "row-" row)}
           [:td.UI-table-header-vertical row]
           (doall
            (map
             (fn [col]
               (let [cell-coord (str col row)
                     cell       (table-cell col row)
                     cell-value (get cell :value)
                     cell-comp  (get cell :computed)
                     is-active  (= @active-cell cell-coord)]
                 [:td.UI-table-td {:key (str  "cell-" col "-" row)}
                  [:div.UI-table-cell {:on-click #(set-active-cell col row)}
                   (if (and is-active @edit-mode)
                     ;; show input if cell is active and is edit mode
                     [:input.UI-table-cell-input
                      {:default-value cell-value
                       :auto-focus    true
                       :on-blur       #(handle-cell-change col row (-> % .-target .-value))}]
                     (or cell-comp cell-value))]]))
             (:cols table-size)))])
        (:rows table-size)))]]]
   [:div.UI-table-desc
    (str "Click inside a cell to edit its formula."
         " Press enter to apply. Click outside the cell or press escape to cancel."
         " Here are some example contents:"
         " '5.5', 'Some text', '=A1', '=SUM(B2:C4)', '=DIV(C1, 5)'."
         " Supported operations are SUM, SUB, MUL, DIV, AVG.")]])

;; -------------------------
;; Page components

(defn github-corner []
  [:a.github-corner
   {:aria-label "View source on GitHub"
    :target "_blank"
    :href "https://github.com/elias94/7guis-clojurescript"}
   [:svg
    {:aria-hidden "true"
     :style {:fill "#151513" :color "#fff" :position "absolute" :top 0 :border 0 :right 0}
     :view-box "0 0 250 250"
     :height "80"
     :width "80"}
    [:path {:d "M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"}]
    [:path.octo-arm
     {:style {:transform-origin "130px 106px"}
      :fill "currentColor"
      :d
      "M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2"}]
    [:path.octo-body
     {:fill "currentColor"
      :d
      "M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z"}]]])

(defn main-page []
  [:span.main
   [github-corner]
   [:h1.page-title "7GUIs in Clojurescript with Reagent"]
   [:p
    "This is a live version of 7GUIs with "
    [:a {:href "https://clojurescript.org"
         :target "_blank"}
     "Clojurescript"]
    " and "
    [:a {:href "https://reagent-project.github.io/"
         :target "_blank"}
     "Reagent"]
    "."
    [:br]
    [:small [:a {:href "https://github.com/elias94/7guis-clojurescript"
                 :target "_blank"}
             "(source)"]]]
   [components/ui-task "Counter"               counter]
   [components/ui-task "Temperature Converter" temp-converter]
   [components/ui-task "Flight Booker"         flight-booker]
   [components/ui-task "Timer"                 timer]
   [components/ui-task "CRUD"                  crud]
   [components/ui-task "Circle Drawer"         circle-drawer]
   [components/ui-task "Cells"                 cells]])


(defn start []
  (rdom/render [main-page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
