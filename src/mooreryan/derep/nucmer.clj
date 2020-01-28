(ns mooreryan.derep.nucmer
  (:require [mooreryan.derep.sh :as sh]
            [me.raynes.fs :as fs])
  (:import [java.io File]))

(defn- nucmer-out-fname
  [prefix]
  (str prefix ".delta"))

(defn run-nucmer
  "Runs nucmer then return the name of the output file.  E.g., nucmer --prefix
  teehee vf141.contigs.fa vf141.contigs.fa.  Returns the nucmer output file,
  will end in .delta"
  [in-fname out-dir]
  (fs/mkdirs out-dir)
  (let [out-prefix (str out-dir
                        File/separator
                        (fs/name in-fname))]
    (sh/try-run "nucmer" "--prefix" out-prefix in-fname in-fname)
    (nucmer-out-fname out-prefix)))

(defn run-show-coords
  "Convert the nucmer output to something more suitable for parsing.

-c: include % cov
-r: sort output by reference sequence
-l: include seq len
-T: tab-delimeted output
-g: Only display alignments included in the Longest Ascending Subset, i.e.
    the global alignment
-I: Show only alignments >= this % identity (from 0 - 100)

Note that there will be some lines at the top that need to be
skipped when parsing the output.  1st line should be the input
files.  2nd line should be \"NUCMER\".  3rd line should be blank.
4th line should be the header.

Header line should be

[S1]\t[E1]\t[S2]\t[E2]\t[LEN 1]\t[LEN 2]\t[% IDY]\t[LEN R]\t[LEN Q]\t[COV R]\t[COV Q]\t[TAGS]

Also, the last 'column' ([TAGS]) actually has the reference name and
the query name separated by a tab char.  So it's really two columns."
  [in-fname pident]
  (let [in-dir (fs/parent in-fname)
        in-base (fs/name in-fname)
        out-fname (str in-dir
                       File/separator
                       in-base
                       ".coords.txt")]
    (sh/try-run "show-coords" "-crlT" "-I" pident in-fname
           :out-fname out-fname)
    out-fname))

(defn make-coords-file
  "Returns the coords file name.  `in-fname` should be a fasta file.
  `out-prefix` may contain a directory name.  `pident` is the min pident to
  include in the coords output."
  [{:keys [in-file out-dir min-pident]}]
  (-> in-file
      (run-nucmer out-dir)
      (run-show-coords min-pident)))