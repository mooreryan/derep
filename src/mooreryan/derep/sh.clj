(ns mooreryan.derep.sh
  (:require [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:import [java.io IOException StringWriter]))

(defn- parse-args
  [args]
  ;; Use (complement keyword?) instead of string? in case you get data types
  ;; other than string in there.  Splits on first keyword.
  (let [[cmd opts] (split-with (complement keyword?) args)]
    [cmd (apply hash-map opts)]))

(defn- stream-to-file
  "Copies `in` to `out`.  Returns a file handle for `out`."
  [in out]
  (with-open [os (io/output-stream out)]
    (io/copy in os))
  (io/file out))

(defn- stream-to-string
  [in]
  (with-open [out (StringWriter.)]
    (io/copy in out)
    (.toString out)))

(defn- stream-it
  ([in] (stream-it in nil))
  ([in out]
   (if out
     (stream-to-file in out)
     (stream-to-string in))))

(defn- process
  "Create process by running cmd with default runtime.  Throws a nicer error
  message."
  [cmd]
  (try
    (.exec (Runtime/getRuntime) (s/join " " cmd))
    (catch IOException ex
      (let [msg (format "Error while running '%s' -- %s"
                        (s/join " " cmd)
                        (.getMessage ex))]
        (throw (IOException. msg))))))

(defn sh
  "A simplified version of clojure.java.shell/sh.  Can redirect stdout and
  stderr without reading it all into memory.  Useful if your shell script
  outputs tons of stuff to stdout and stderr and you don't want to read it all
  into memory."
  [& args]
  (let [[cmd {:keys [out-fname err-fname] :as opts}] (parse-args args)]
    (let [proc (process cmd)]
      (with-open [stdout (.getInputStream proc)
                  stderr (.getErrorStream proc)]
        ;; Streams to file if fname given, string otherwise.
        (let [out (future (stream-it stdout out-fname))
              err (future (stream-it stderr err-fname))
              exit-code (.waitFor proc)]
          {:exit exit-code :out @out :err @err})))))

(defn err-msg
  [cmd err]
  (format "Error when running cmd: '%s' -- %s"
          (s/join " " cmd)
          err))

(defn try-run
  [& args]
  (let [{:keys [exit err] :as result} (apply sh args)]
    (if (zero? exit)
      result
      (let [[cmd _] (parse-args args)]
        (throw (Exception. (err-msg cmd err)))))))

(def lots "/Users/moorer/Desktop/lots-of-data.rb" )

