(defproject mooreryan/derep "0.1.0"
  :description "Dereplicate contigs or genomes with nucmer"
  :url "https://github.com/mooreryan/derep"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [clansi "1.0.0"]
                 [clj-wrap-indent "1.0.0"]
                 [clj-commons/fs "1.5.0"]]
  :repl-options {:init-ns mooreryan.derep.core}
  :main ^:skip-aot mooreryan.derep.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}})
