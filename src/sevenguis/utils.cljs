(ns sevenguis.utils
  (:require [clojure.string :as str]))


(def console-log
  "Log to js console"
  (.-log js/console))


(defn parse-int
  "Parse a string of integer into an int value"
  [integer]
  (js/parseInt integer 10))


(defn parse-float
  "Parse a string of integer into an int value"
  [integer]
  (js/parseFloat integer 10))


(defn escape-str
  "Escape a string replacing spaces with `-`"
  [string-with-spaces]
  (str/replace string-with-spaces #" " "-"))


(defn vec-remove
  "Remove elem in coll"
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))


(defn prevent-default
  "Prevent default and propagation of a DOM event"
  [e]
  (.stopPropagation e)
  (.preventDefault e))


(defn find-first
  "First item in a collection that satisfy the predicate"
  [f coll]
  (first (filter f coll)))


(defn char-range
  "Generate a range sequence of characters"
  [start end]
  (map char (range (int start) (inc (int end)))))


(defn in?
  "true if coll contains el"
  [coll el]
  (some #(= el %) coll))


(def date-now
  "Get current time in a js Date object"
  (js/Date. (.now js/Date)))


(defn format-date
  "Format date as dd-MM-yyyy"
  ([date]
   (format-date date "."))
  ([date sep]
   (let [year  (str (.getFullYear date))
         month (str (inc (.getMonth date)))
         day   (str (.getDate date))]
     (str (when (< (count day) 2) "0") day
          sep
          (when (< (count month) 2) "0") month
          sep
          year))))
