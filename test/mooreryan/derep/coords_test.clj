(ns mooreryan.derep.coords-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [mooreryan.derep.coords :refer :all]))

(def test-dir "./test_files")
(def hi-coords (str test-dir "/hi.coords"))

(def example-canonical-aln {:rstart 1541.0
                            :rend 1661.0
                            :qstart 531.0
                            :qend 411.0
                            :ralnlen 121.0
                            :qalnlen 121.0
                            :pident 100.0
                            :rlen 1762.0
                            :qlen 1530.0
                            :rcov 6.87
                            :qcov 7.91
                            :ref "k141_8794"
                            :query "k141_900"})
;; This is the non-canonical version of the above.
(def example-non-canonical-aln {:rstart 411.0
                                :rend 531.0
                                :qstart 1661.0
                                :qend 1541.0
                                :ralnlen 121.0
                                :qalnlen 121.0
                                :pident 100.0
                                :rlen 1530.0
                                :qlen 1762.0
                                :rcov 7.91
                                :qcov 6.87
                                :ref "k141_900"
                                :query "k141_8794"})

(def coords-ary ["528"                     ; rstart
                 "914"                     ; rend
                 "1530"                    ; qstart
                 "1144"                    ; qend
                 "387"                     ; ralnlen
                 "387"                     ; qalnlen
                 "94.33"                   ; pident
                 "1762"                    ; rlen
                 "1530"                    ; qlen
                 "21.96"                   ; rcov
                 "25.29"                   ; qcov
                 "k141_8794"               ; ref
                 "k141_900"                ; query
                 ])

(def coords-line (s/join "\t" coords-ary))

(deftest test--parse-data-line
  (testing "It parses a data line from the coords file."
    (let [expected {:rstart 528.0
                    :rend 914.0
                    :qstart 1530.0
                    :qend 1144.0
                    :ralnlen 387.0
                    :qalnlen 387.0
                    :pident 94.33
                    :rlen 1762.0
                    :qlen 1530.0
                    :rcov 21.96
                    :qcov 25.29
                    :ref "k141_8794"
                    :query "k141_900"}]
      (is (= expected (parse-data-line coords-line)))))
  (testing "It throws if too few data columns"
    (is (thrown? Exception (parse-data-line "100\t200"))))
  (testing "It throws if a numeric col is not numeric"
    (is (thrown? Exception (parse-data-line
                            (s/join "\t" (assoc coords-ary 0 "apple")))))))

(deftest test--flip-alignment
  (testing "It 'flips' an aligment by swapping query and ref."
    (is (= example-canonical-aln (flip-alignment example-non-canonical-aln)))))

(deftest test--canonical?
  (testing "Same :ref and :query is canonical"
    (is (true? (canonical? {:ref "apple" :query "apple"}))))
  (testing "don't forget...it's lexographic compare!"
    (is (true? (canonical? {:ref "Apple" :query "apple"}))))
  (testing "Canonical aln has ref <= query"
    (is (true? (canonical? {:ref "apple" :query "pie"})))
    (is (false? (canonical? {:ref "pie" :query "apple"})))))

(deftest test--canonical-aligment
  (testing "if aln is canonical, return it"
    (is (= example-canonical-aln
           (canonical-alignment example-canonical-aln))))
  (testing "if aln is non-canonical, return canonical version"
    (is (= example-canonical-aln
           (canonical-alignment example-non-canonical-aln)))))

(deftest test--update-alignments
  (let [aln1 {:ref "pie" :query "apple" :pident 75.0}
        aln2 {:ref "apple" :query "pie" :pident 50.0}
        aln-key '("apple" "pie")]
    (testing "when aln-key is not present"
      (is (= {aln-key [aln1]}
             (update-alignments {} aln1))))
    (testing "when aln-key is present"
      (is (= {aln-key [aln1 aln2]}
             (update-alignments {aln-key [aln1]} aln2))))))

(deftest test--good-coverage?
  (testing "true if ref cov is greater than min-cov"
    (is (true? (good-coverage? 50.0 {:rcov 50.0 :qcov 10.0}))))
  (testing "true if qcov is >= min-cov"
    (is (true? (good-coverage? 50.0 {:rcov 10.0 :qcov 50.0}))))
  (testing "false if neither is >= min-cov"
    (is (false? (good-coverage? 50.0 {:rcov 10.0 :qcov 11.0})))))

(deftest test--update-total-cov
  (let [old-total-cov {:rcov 5.0 :qcov 10.0}
        min-pident 50.0]
    (testing "updates total-cov when has good-pident?"
      (is (= {:rcov 10.0 :qcov 17.0}
             (update-total-cov min-pident
                               old-total-cov
                               {:rcov 5.0 :qcov 7.0 :pident min-pident}))))
    (testing "returns original when doesn't meet pident threshold"
      (is (= old-total-cov
             (update-total-cov min-pident
                               old-total-cov
                               {:rcov 5.0
                                :qcov 7.0
                                :pident (- min-pident 1)}))))))

(deftest test--get-covered-regions
  (testing "if no regions overlap, return original thing"
    (let [coords [{:start 1 :end 5}
                  {:start 6 :end 10}]]
      (is (= coords (get-covered-regions coords)))))
  (testing "merges overlapping regions"
    (let [coords [{:start 1 :end 5}
                  {:start 3 :end 7}
                  {:start 6 :end 10}
                  {:start 100 :end 123}]
          expected [{:start 1 :end 10}
                    {:start 100 :end 123}]]
      (is (= expected (get-covered-regions coords))))))

(deftest test--pair-category
  (let [min-pident 50.0
        min-cov 60.0
        aln-key '("apple" "pie")
        pair-category-fn (partial pair-category min-pident min-cov aln-key)
        aln-base {:ref "apple" :query "pie"}]
    (testing "pairs that are the :same"
      (testing "a single :rcov and :pident passes threshold"
        (let [alns [(conj aln-base
                          {:rcov 60.0 :qcov 59.0 :pident 50.0})]]
          (is (= [aln-key :same]
                 (pair-category-fn alns)))))
      (testing "a single :qcov and :pident passes threshold"
        (let [alns [(conj aln-base
                          {:rcov 59.0 :qcov 60.0 :pident 50.0})]]
          (is (= [aln-key :same]
                 (pair-category-fn alns))))))
    (testing "pairs that are :unsure"
      (testing "non overlapping aligments"
        (testing ":pident and sum of :rcov good"
          (let [alns [(conj aln-base
                            {:rstart 1 :rend 300
                             :rlen 1000 :rcov 30.0
                             :qstart 1 :qend 300
                             :qlen 10000 :qcov 3.0
                             :pident 50.0})
                      (conj aln-base
                            {:rstart 700 :rend 1000
                             :rlen 1000 :rcov 30.0
                             :qstart 700 :qend 1000
                             :qlen 10000 :qcov 3.0
                             :pident 50.0})]]
            (is (= [aln-key :unsure]
                   (pair-category-fn alns)))))
        (testing ":pident and sum of :qcov good"
          (let [alns [(conj aln-base
                            {:qstart 1 :qend 300
                             :qlen 1000 :qcov 30.0
                             :rstart 1 :rend 300
                             :rlen 10000 :rcov 3.0
                             :pident 50.0})
                      (conj aln-base
                            {:qstart 700 :qend 1000
                             :qlen 1000 :qcov 30.0
                             :rstart 700 :rend 1000
                             :rlen 10000 :rcov 3.0
                             :pident 50.0})]]
            (is (= [aln-key :unsure]
                   (pair-category-fn alns)))))
        (testing "overlapping is handled properly"
          ;; These alignments when overlapped pass the threshold for coverage.
          (let [alns [(conj aln-base
                            {:qstart 1 :qend 500
                             :qlen 1000 :qcov 50.0
                             :rstart 1 :rend 300
                             :rlen 10000 :rcov 3.0
                             :pident 50.0})
                      (conj aln-base
                            {:qstart 101 :qend 600
                             :qlen 1000 :qcov 50.0
                             :rstart 700 :rend 1000
                             :rlen 10000 :rcov 3.0
                             :pident 50.0})]]
            (is (= [aln-key :unsure]
                   (pair-category-fn alns)))))))
    (testing "pairs that are :different"
      (testing "aligments combine to pass cov, but not pident"
        (let [alns [(conj aln-base
                          {:rstart 1 :rend 300
                           :rlen 1000 :rcov 30.0
                           :qstart 1 :qend 300
                           :qlen 10000 :qcov 3.0
                           :pident 20.0})
                    (conj aln-base
                          {:rstart 700 :rend 1000
                           :rlen 1000 :rcov 30.0
                           :qstart 700 :qend 1000
                           :qlen 10000 :qcov 3.0
                           :pident 50.0})]]
          (is (= [aln-key :different]
                 (pair-category-fn alns)))))
      (testing "long enough aligment but bad pident"
        (let [alns [(conj aln-base
                          {:rstart 1 :rend 1000
                           :rlen 1000 :rcov 100.0
                           :qstart 1 :qend 300
                           :qlen 10000 :qcov 3.0
                           :pident 49.0})]]
          (is (= [aln-key :different]
                 (pair-category-fn alns)))))
      (testing (str "before overlapping, sum of coverages passes threshold, "
                    "but after overlapping they don't")
        ;; The total query coverage if you consider overlaps is only 40% but if
        ;; you just look at the the :rcov and add them, you'd get >=60%.
        (let [alns [(conj aln-base
                          {:rstart 1 :rend 300
                           :rlen 1000 :rcov 30.0
                           :qstart 1 :qend 300
                           :qlen 10000 :qcov 3.0
                           :pident 50.0})
                    (conj aln-base
                          {:rstart 101 :rend 400
                           :rlen 1000 :rcov 30.0
                           :qstart 700 :qend 1000
                           :qlen 10000 :qcov 3.0
                           :pident 50.0})]]
          (is (= [aln-key :different]
                 (pair-category-fn alns))))))))

(deftest test--ref-query-pair-categories
  (let [lines (s/split-lines (slurp hi-coords))]
    (testing "it doesn't return pairs that are :different."
      (let [min-pident 99
            min-cov 50
            ref-query-pairs (get-ref-query-pairs min-pident lines)]
        (is (empty? (ref-query-pair-categories min-pident min-cov ref-query-pairs)))))
    (testing "it returns :unsure pairs"
      (let [min-pident 50
            min-cov 30
            ref-query-pairs (get-ref-query-pairs min-pident lines)]
        (is (= [[["k141_8794" "k141_900"] :unsure]]
               (ref-query-pair-categories min-pident min-cov ref-query-pairs)))))
    (testing "it returns :same pairs"
      (let [min-pident 50
            min-cov 20
            ref-query-pairs (get-ref-query-pairs min-pident lines)]
        (is (= [[["k141_8794" "k141_900"] :same]]
               (ref-query-pair-categories min-pident min-cov ref-query-pairs)))))))
