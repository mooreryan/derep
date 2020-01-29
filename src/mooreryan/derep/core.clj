(ns mooreryan.derep.core
  (:require [mooreryan.derep.cli :as cli])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cli/run-derep args))
