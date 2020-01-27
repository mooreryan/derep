(defproject mooreryan/derep "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [clansi "1.0.0"]
                 [clj-wrap-indent "1.0.0"]
                 [seesaw "1.5.0"]]
  :repl-options {:init-ns mooreryan.derep.gui}
  :main ^:skip-aot mooreryan.derep.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}})