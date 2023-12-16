(ns shared
  (:require [clojure.java.io :as io]))

(defn file->rows [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (line-seq reader))))
