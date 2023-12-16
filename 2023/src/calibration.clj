(ns calibration
  (:require [shared])
  (:import (java.lang Character)
           (java.util Set)))

(defn digit? [^Character c]
  (Character/isDigit c))

(defn ->single-int [[head tail]]
  (Integer/parseInt (str head tail)))

(defn ->full-row [[head tail :as row]]
  (cond
    (empty? row) nil
    (nil? tail) [head head]
    :else row))

(def ^:const numbers #{"one" "two" "three" "four" "five" "six" "seven" "eight" "nine"})

(defn number->digit [word]
  (get
    {"one"   1
     "two"   2
     "three" 3
     "four"  4
     "five"  5
     "six"   6
     "seven" 7
     "eight" 8
     "nine"  9}
    word))

(defn ->number-finder
  "Returns the first occurrence of any of the available words on the specified line"
  [^Set words line]
  (let [[min-length max-length] (->> words (map count) (apply (juxt min max)))]
    (->> (range min-length (inc max-length))
         (map #(apply str (take % line)))
         (map words)
         (filter identity)
         (first))))

(defn words->digits [^Set available-words row]
  (let [find-number (partial ->number-finder available-words)]
    (loop [input row output []]
      (if (empty? input)
        (apply str output)
        (let [word (find-number input)
              to-skip (if (some? word) (dec (count word)) 1)]
          (recur
            (apply str (drop to-skip input))
            (if (some? word)
              (conj output (number->digit word))
              (apply conj output (take to-skip input)))))))))

(defn row->digits [^String row]
  (some->>
    row
    (words->digits numbers)
    (filter digit?)
    ((juxt first last))
    (filter identity)
    (->full-row)
    (->single-int)))

(defn file->calibration-score [file-path]
  (->>
    file-path
    (shared/file->rows)
    (map row->digits)
    (flatten)
    (reduce +)))
