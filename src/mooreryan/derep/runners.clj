(ns mooreryan.derep.runners
  (:require [clojure.java.shell :as sh]
            [clojure.string :as s])
  (:import [java.io IOException]))

(defn command-on-path?
  [s]
  (let [{:keys [exit]} (sh/sh "which" s)]
    (zero? exit)))

(defn- sh-helper
  [& args]
  (try
    (apply sh/sh args)
    (catch IOException e
      (let [msg (format "Error while running '%s' -- %s"
                        (s/join " " args)
                        (.getMessage e))]
        (throw (Exception. msg))))))

(defn run-sh
  [& args]
  (let [{:keys [exit out err] :as result} (apply sh-helper args)]
    (if (zero? exit)
      result
      (let [msg (format "Non-zero exit status (%s) while running '%s' -- %s"
                        exit
                        (s/join " " args)
                        err)]
        (throw (Exception. msg))))))