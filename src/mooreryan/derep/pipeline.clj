(ns mooreryan.derep.pipeline
  (:require [mooreryan.derep.coords :as coords]
            [mooreryan.derep.nucmer :as nucmer]
            [clojure.java.io :as io]))

(defn check-file-type
  "Returns :fasta if input looks like fasta, :coords if it looks like output
  from nucmer's show-coords and :unknown otherwise."
  [fname]
  (with-open [rdr (io/reader fname)]
    (let [lines (line-seq rdr)]
      (cond
        (= \> (first (first lines)))
        :fasta
        (and (= "NUCMER" (second lines))
             (= coords/coords-header-line (nth lines 3)))
        :coords
        :else
        :unknown))))

(defn run-derep-pipeline
  [{:keys [in-file] :as opts}]
  (let [file-type (check-file-type in-file)]
    (cond
      (= :fasta file-type)
      (let [coords-fname (nucmer/make-coords-file opts)]
        (coords/parse-coords (assoc opts :in-file coords-fname)))
      (= :coords file-type)
      (coords/parse-coords opts)
      :else
      (throw (Exception. "Input file type was not fasta or coords!")))))