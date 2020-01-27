(ns mooreryan.derep.runners-test
  (:require [clojure.test :refer :all]
            [mooreryan.derep.runners :refer :all]))

(deftest test--command-on-path?
  (testing "returns truthy if arg is on PATH and executable"
    (is (true? (command-on-path? "ls")))
    (is (true? (command-on-path? "pwd"))))
  (testing "returns falsey if arg is not on PATH and executable"
    (is (false? (command-on-path? "arstoienarstoienarstoien")))))

(deftest test--run-sh
  (testing "throws an error if thing isn't executable"
    (is (thrown? Exception (run-sh "arsotien"))))
  (testing "throws an error if command has non-zero exit code"
    (is (thrown? Exception (run-sh "ls" "arositeaor"))))
  (testing "returns result of running command"
    (is (= {:exit 0, :out "hi\n", :err ""}
           (run-sh "echo" "hi")))))