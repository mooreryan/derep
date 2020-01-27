(ns mooreryan.derep.const)

(def exit-codes {:success 0 :failure 1})

(def program "derep")
(def version "0.1.0")

(def version-info
  {:program program
   :version version
   :copyright "2020 Ryan Moore"
   :license "EPL v2"
   :contact "moorer@udel.edu"
   :website "https://github.com/mooreryan/derep"})

(def summary "Dereplicate genomes/contigs/sequences with nucmer.")

(def details "You must have nucmer installed and on the path!")