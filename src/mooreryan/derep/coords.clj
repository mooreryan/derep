(ns mooreryan.derep.coords
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs])
  (:import (java.io File)))

;; This should be the fourth line of the coords file if it was made properly.
(def coords-header-line "[S1]\t[E1]\t[S2]\t[E2]\t[LEN 1]\t[LEN 2]\t[% IDY]\t[LEN R]\t[LEN Q]\t[COV R]\t[COV Q]\t[TAGS]")

(defn try-parse-double
  [s]
  (try
    (Double/parseDouble s)
    (catch NumberFormatException e s)))

(defn parse-data-line

  "'r' means reference, 'q' means query.  Start and end are 1-based coordinates
  (I think...).  alnlen is the length of the alignment on the sequence. pident
  is % identity.  len is the length of the actual sequence. cov is the %
  coverage of the alignment on the sequence.  :ref and :query are the names of
  the reference and the query.

   Will throw if there are not enough data columns or if columns that should be
   numeric aren't."
  [line]
  (let [num-numeric-cols 11
        keys [:rstart :rend
              :qstart :qend
              :ralnlen :qalnlen
              :pident
              :rlen :qlen
              :rcov :qcov
              :ref :query]
        [numeric-data seq-names] (split-at num-numeric-cols (s/split line #"\t"))
        numeric-data (map try-parse-double numeric-data)]
    (cond
      (not (= (count keys) (+ (count numeric-data)
                              (count seq-names))))
      (throw (Exception. "Wrong number of data cols!"))
      (not-every? #(instance? Double %) numeric-data)
      (throw (Exception. "The first 11 columns should be numeric!"))
      :else
      (zipmap keys (flatten [numeric-data seq-names])))))

(defn different-ref-and-query?
  [aln]
  (not (= (:ref aln) (:query aln))))

(defn alignment-key
  [aln]
  (sort [(:ref aln) (:query aln)]))

(defn flip-alignment
  "Flip the reference and the query of an alignment.  Also flip all the
  starts and ends and stuff like that in the proper way so you can test for
  identity."
  [aln]
  {:rstart (:qend aln)
   :rend (:qstart aln)
   :qstart (:rend aln)
   :qend (:rstart aln)
   :ralnlen (:qalnlen aln)
   :qalnlen (:ralnlen aln)
   :pident (:pident aln)
   :rlen (:qlen aln)
   :qlen (:rlen aln)
   :rcov (:qcov aln)
   :qcov (:rcov aln)
   :ref (:query aln)
   :query (:ref aln)})

(defn canonical?
  "A canonical alignment has the lexographically lesser of the ref and query
  ID as the ref and the greater as the query, or has the same ref and query."
  [aln]
  (<= (compare (:ref aln) (:query aln)) 0))

(defn canonical-alignment
  "Returns canonical alignment.  Canonical alignments have the ref and query
  be sorted in lexographic order by key.  E.g., contig-A (ref) contig-B
  (query) would be canonical version of contig-B (ref) contig-A (query).  In
  other words, the ref should always be 'before' the query in string sort
  order."
  [aln]
  (if (canonical? aln)
    aln
    (flip-alignment aln)))

(defn update-alignments
  "Takes the `alns` hash map and adds a new alignment (`aln`) to it.  Keys
  the alignment on (alignment-key aln).  Return val is a hash-map from
  aligment-keys to a colls of aligments with that aln-key."
  [alns aln]
  (let [aln-key (alignment-key aln)]
    (if (contains? alns aln-key)
      (update alns aln-key conj aln)
      (assoc alns aln-key [aln]))))

(defn good-coverage?
  "At least one of the alignment coverages must be above the threshold."
  [min-cov aln]
  (or (>= (:rcov aln) min-cov)
      (>= (:qcov aln) min-cov)))

(defn good-pident?
  [min-pident aln]
  (>= (:pident aln) min-pident))

(defn update-total-cov
  "Expects `total-cov` and aln to have keys :rcov and :qcov."
  [min-pident total-cov aln]
  (if (good-pident? min-pident aln)
    (-> total-cov
        (update :rcov + (:rcov aln))
        (update :qcov + (:qcov aln)))
    total-cov))

(defn good-pident-and-coverage?
  [min-pident min-cov aln]
  (and (good-pident? min-pident aln)
       (good-coverage? min-cov aln)))







(defn try-flip-coordinates
  [coords]
  (if (<= (:start coords) (:end coords))
    coords
    {:start (:end coords) :end (:start coords)}))

(defn keywordize
  [& args]
  (keyword (apply str args)))

(defn start-end-coords
  [which aln]
  (let [start ((keywordize which "start") aln)
        end ((keywordize which "end") aln)]
    (if (<= start end)
      {:start start :end end}
      {:start end :end start})))

(defn merge-coords?
  "Check to see if you should merge coords."
  [previous current]
  (if (<= (:start current) (:end previous))
    {:start (:start previous)
     :end (:end current)}))

(defn tile-coords
  [all-coords]
  (if (empty? all-coords)
    (throw (Exception. ""))))

(defn last-idx [coll] (dec (count coll)))

(defn get-covered-regions
  "Note:  all-coords shouldn't have nil values....but I don't check for it."
  [all-coords]
  (reduce (fn [coords current]
            (if-let [merged (and (seq coords)
                                 (merge-coords? (last coords) current))]
              (assoc coords (last-idx coords) merged)
              (conj coords current)))
          []
          all-coords))

(defn get-covered-bases
  "Gets the total number of bases covered by aligments.  Assumes that
  `merged-coords` has something that responds to :end and :start, or is empty."
  [merged-coords]
  (reduce (fn [total-covered-bases current]
            (let [current-covered-bases
                  (inc (- (:end current) (:start current)))]
              (+ total-covered-bases current-covered-bases)))
          0
          merged-coords))

(defn total-cov
  "Only count aligments wich pass the `min-pident` threshold.  Uses the
  special tiling to get actual cov on a contig."
  [min-pident which alns]
  (let [len ((keywordize which "len") (first alns))
        covered-regions
        ;; Note...in each one of these lines, you could get an empty result...
        (->> alns
             (filter (partial good-pident? min-pident))
             (map (partial start-end-coords which))
             (sort-by :start)
             get-covered-regions
             get-covered-bases)]
    (* (/ covered-regions
          len)
       100)))

(defn pair-category
  "Given alignments for a ref->query pair, determine the category of that
  pair.

  :same means that the two contigs are duplicates according to pident
  and %cov (i.e., they have a single alignment between the ref and query that
  passes both the PID and %Cov thresholds).  The smaller of the two can be
  dropped.

  :unsure means the ref and query do NOT have a single alignment that passes
  the thresholds but may have multiple alignments that together pass the
  coverage threshold.  These should be hand checked.

  :different means that not :same and not :unsure.  Neither of these should
  be dropped because of this ref->query pair.  One or both may be dropped
  because of another ref->query pair though."
  [min-pident min-cov aln-key alns]
  (cond
    (some (partial good-pident-and-coverage? min-pident min-cov)
          alns)
    [aln-key :same]
    (let [ref-aln-cov (total-cov min-pident "r" alns)
          query-aln-cov (total-cov min-pident "q" alns)]
      (or (>= ref-aln-cov min-cov)
          (>= query-aln-cov min-cov)))
    [aln-key :unsure]
    :else
    [aln-key :different]))

(defn get-data-lines
  "Returns the data lines of the coords file.  Will throw on any format errors
  in the header."
  [lines]
  (let [[header-lines data-lines] (split-at 4 lines)]
    (if (not (= "NUCMER" (second header-lines)))
      (throw (Exception. "Second line of header should be: NUCMER")))
    (if (not (= coords-header-line
                (nth header-lines 3)))
      (throw (Exception. (str "Fourth line of header should be: " coords-header-line))))
    data-lines))

(defn get-category
  [pair-category]
  (last pair-category))

(defn different?
  "Input arg should be like [['apple' 'pie'] :different]"
  [pair-category]
  (= :different (get-category pair-category)))

(def not-different? (complement different?))

(defn get-ref-query-pairs
  "Returns the a hash mapping from aln-key => aligments.  Only keeps useful
  aligments."
  [min-pident lines]
  (->> lines
       get-data-lines
       (map parse-data-line)
       (filter different-ref-and-query?)
       (map canonical-alignment)
       (filter (partial good-pident? min-pident))
       (reduce update-alignments {})))

(defn ref-query-pair-categories
  "Returns categories for each pair of sequences represented in the
  `ref-query-pairs` map."
  [min-pident min-cov ref-query-pairs]
  (->> ref-query-pairs
       (map (fn [[aln-key alns]]
              (pair-category min-pident min-cov aln-key alns)))
       (filter not-different?)))

(defn write-pair-categories
  "Will create `out-dir` if it doesn't exist.  Uses in-file to set basename of
  outfile."
  [in-file out-dir pair-categories]
  (if-not (fs/exists? out-dir)
    (fs/mkdirs out-dir))
  (with-open [outf (io/writer (str out-dir
                                   File/separator
                                   (str (fs/name in-file)
                                        ".pair_categories.txt")))]
    (doseq [[category pairs] (group-by get-category pair-categories)]
      (doseq [pair pairs]
        (.write outf (s/join "\t" (flatten [(symbol category) (first pair)])))
        (.newLine outf)))))

(defn parse-coords
  "Parses the coords file.  Call this from the -main function."
  [{:keys [min-pident min-cov in-file out-dir]}]
  (with-open [rdr (io/reader in-file)]
    (->> rdr
         line-seq
         (get-ref-query-pairs min-pident)
         (ref-query-pair-categories min-pident min-cov)
         (write-pair-categories in-file out-dir))))
