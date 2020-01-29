(ns mooreryan.derep.cli
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clansi :refer [style]]
            [clj-wrap-indent.core :as wrap]
            [mooreryan.derep.coords :as coords]
            [mooreryan.derep.gui :as gui]
            [mooreryan.derep.const :as const]
            [mooreryan.derep.pipeline :as pipeline]))

(defn eprintln
  [& more]
  (binding [*out* *err*]
    (apply println more)))

(defn- validate-percentage
  [x]
  (and (< 0 x) (<= x 100)))

(defn- file-exists?
  [fname]
  (if fname
    (.exists (io/file fname))))

(defn- missing?
  [opt opts]
  (not (get opts opt)))

(defn- error-msg
  [errors]
  (str (style "ERROR" :bright) " -- "
       (s/join \newline errors)))

(defn exit
  [status msg]
  (eprintln msg)
  (System/exit status))

(defn- lines
  [& more]
  (->> more
       (map #(apply str %))
       (s/join \newline)))

(def cli-options
  [["-p" "--min-pident X" "Minimum percent identity"
    :default 95.0
    :parse-fn #(Double/parseDouble %)
    :validate [validate-percentage "Must be between 0 and 100"]]
   ["-c" "--min-cov X" "Minimum coverage"
    :default 80.0
    :parse-fn #(Double/parseDouble %)
    :validate [validate-percentage "Must be between 0 and 100"]]
   ["-i" "--in-file NAME" "Input coords file name"
    :validate [file-exists? "input file doesn't exist!"]]
   ["-o" "--out-dir NAME" "Output directory name"]
   ["-h" "--help"]])

(defn version-banner
  [{:keys [program version copyright contact website license]}]
  (lines
   (style program :underline)
   ""
   ["Version:   " version]
   ["Copyright: " copyright]
   ["License:   " license]
   ["Contact:   " contact]
   ["Website:   " website]))

(defn program-banner
  "This is the general banner for the program."
  []
  (lines
   ""
   (style "PROGRAM" :bright)
   ""
   (wrap/wrap-indent (version-banner const/version-info) 80 2)
   ""
   (style "SUMMARY" :bright)
   ""
   (wrap/wrap-indent const/summary 80 2)
   ""
   (style "USAGE" :bright)
   ""
   (wrap/wrap-indent (str const/program " [command]") 80 2)
   (if const/details
     (lines
      ""
      (style "DETAILS" :bright)
      ""
      (wrap/wrap-indent const/details 80 2)
      ""))
   (style "COMMANDS" :bright)
   ""
   (wrap/wrap-indent "cli -- Run the CLI interface" 80 2)
   (wrap/wrap-indent "gui -- Run the GUI interface" 80 2)
   ""))

(defn cli-banner
  "This is the banner specific to the CLI version of the program."
  [opts-info]
  (lines
   ""
   "Run the derep program's Command Line Interface (CLI)"
   ""
   (style "USAGE" :bright)
   ""
   (wrap/wrap-indent (str const/program " cli [options]") 80 2)
   (if const/details
     (lines
      ""
      (style "DETAILS" :bright)
      ""
      (wrap/wrap-indent const/details 80 2)
      ""))""
   (style "OPTIONS" :bright)
   ""
   (wrap/wrap-indent opts-info 80 2)
   ""))

(defn validate-cli-args
  "Validate args specific to the CLI program."
  [args]
  (let [{:keys [options arguments summary errors]}
        (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (cli-banner summary) :ok? true}
      errors
      {:exit-message (error-msg errors)}
      (missing? :in-file options)
      {:exit-message (str (style "ERROR:" :bright)
                          "  --in-file is required!"
                          "\n"
                          (cli-banner summary))}
      (missing? :out-dir options)
      {:exit-message (str (style "ERROR:" :bright)
                          "  --out-dir is required!"
                          "\n"
                          (cli-banner summary))}
      :else
      {:action pipeline/run-derep-pipeline :options options})))

(defn run-derep-cli
  "Run the CLI program."
  [args]
  (let [{:keys [action options exit-message ok?]}
        (validate-cli-args args)]
    (if exit-message
      (exit (if ok?
              (:success const/exit-codes)
              (:failure const/exit-codes))
            exit-message)
      (do
        (action options)
        (shutdown-agents)))))

(defn validate-args
  "Validate general program arguments."
  [args]
  (let [{:keys [options arguments summary errors]}
        (cli/parse-opts args cli-options :in-order true)]
    (cond
      (zero? (count arguments))
      {:exit-message (program-banner) :ok? true}
      (= (first arguments) "gui")
      (gui/run-derep-gui!)
      (= (first arguments) "cli")
      (run-derep-cli (rest arguments))
      ;; Want program general help
      (:help options)
      {:exit-message (program-banner) :ok? true}
      :else
      {:exit-message (str "ERROR: Command not available!\n\n"
                          (program-banner))})))

(defn run-derep
  "Run the derep program.  Depending on the args, may run either the CLI or the
  GUI."
  [args]
  (let [{:keys [action options exit-message ok?]}
        (validate-args args)]
    (if exit-message
      (exit (if ok?
              (:success const/exit-codes)
              (:failure const/exit-codes))
            exit-message))))
