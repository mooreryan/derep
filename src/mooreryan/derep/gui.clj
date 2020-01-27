(ns mooreryan.derep.gui
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.stacktrace :refer [print-stack-trace]]
            [mooreryan.derep.coords :as coords]
            [seesaw.bind :as b]
            [seesaw.core :refer :all]
            [seesaw.chooser :refer [choose-file]]
            [seesaw.mig :refer [mig-panel]])
  (:import (javax.swing JOptionPane)))

;; TODO when exiting the GUI, exit the app.

(def app-config {:width 800 :height 300})

(def min-pident (atom 95))
(def min-cov (atom 80))

(def current-file (atom nil))
(def current-outfile (atom nil))

(def work-future (atom nil))

(def work-errors (atom nil))

(defn get-name
  [file]
  (if file
    (.getName file)))

(defn get-absolute-path
  [file]
  (if file
    (.getAbsolutePath file)))

(defn app [content]
  (frame :title "derep"
         :size [(:width app-config) :by (:height app-config)]
         :content content))

(declare set-current-file!)
(declare set-current-outfile!)
(declare my-app)

;; TODO make scrollable
(defn show-error-msg
  [msg]
  (JOptionPane/showMessageDialog my-app
                                 msg
                                 "Error!"
                                 JOptionPane/ERROR_MESSAGE))

(defn show-info-msg
  [msg]
  (JOptionPane/showMessageDialog my-app
                                 msg
                                 "Info",
                                 JOptionPane/PLAIN_MESSAGE))

(defn preview-file
  [file]
  (if file
    (with-open [rdr (io/reader file)]
      (s/join "\n" (take 10 (line-seq rdr))))))

(defn do-work
  [widget]
  (.setEnabled widget false)
  (config! widget :text "Working...")
  (reset! work-errors nil)
  (let [args {:min-pident (double @min-pident)
              :min-cov (double @min-cov)
              :coords-fname @current-file
              :out-fname @current-outfile}]
    (try
      (coords/parse-coords args)
      (show-info-msg (str "All done!"
                          (if @current-outfile
                            (str "\n\nOutfile:\n"
                                 (get-absolute-path @current-outfile)))))
      (catch Exception e
        (let [msg (str "ERROR: "
                       (.getMessage e)
                       "\n\n"
                       (with-out-str (print-stack-trace e)))]
          (reset! work-errors msg)
          (show-error-msg msg)))))
  ;; Fail or success, reset the button
  (config! widget :text "Run derep!")
  (.setEnabled widget true))

(def file-preview
  (text :multi-line? true :columns 35))
(def start-work
  (button :action
          (action :name "Run derep!"
                  :handler (fn [e]
                             (let [widget (.getSource e)]
                               (if (and @min-pident
                                        @min-cov
                                        @current-outfile
                                        @current-file)
                                 (reset! work-future (future (do-work widget)))
                                 (show-error-msg "Not ready!  Select an input file first!")))))))

(def main-panel
  (with-widgets [;; Labels
                 (label :id :min-pident-label
                        :text @min-pident)
                 (label :id :min-cov-label
                        :text @min-cov)
                 (label :id :fname-label
                        :text (if @current-file
                                (.getName @current-file)
                                "Pick an infile!"))
                 (label :id :out-fname-label
                        :text (if @current-outfile
                                (.getName @current-outfile)
                                "Pick an outfile!"))
                 ;; Sliders
                 (slider :id :min-pident-slider
                         :value @min-pident
                         :min 0
                         :max 100
                         :minor-tick-spacing 5
                         :snap-to-ticks? true)
                 (slider :id :min-cov-slider
                         :value @min-cov
                         :min 0
                         :max 100
                         :minor-tick-spacing 5
                         :snap-to-ticks? true)
                 ;; Buttons
                 (button :id :open-file-button
                         :action (action :handler set-current-file!
                                         :name "Select infile"
                                         :tip "Select an input coords file"))
                 (button :id :save-file-button
                         :action (action :handler set-current-outfile!
                                         :name "Select outfile"
                                         :tip "Select and output file"))]
    ;; Set up bindings
    (b/bind min-pident-slider min-pident)
    (b/bind min-cov-slider min-cov)
    (b/bind min-pident min-pident-label)
    (b/bind min-cov min-cov-label)
    (b/bind current-file (b/transform get-name) fname-label)
    (b/bind current-outfile (b/transform get-name) out-fname-label)
    (b/bind current-file (b/transform preview-file) file-preview)
    ;; Set up mig panel
    (mig-panel
     :items [[open-file-button "grow, span 1"] [fname-label "width 225!, left, wrap"]
             [save-file-button "grow, span 1"] [out-fname-label "width 225!, left, wrap"]
             ["% identity" "right"] [min-pident-slider "grow"] [min-pident-label "grow, wrap"]
             ["% coverage" "right"] [min-cov-slider "grow"] [min-cov-label "grow, wrap"]])))

(def my-app-border-panel
  (border-panel :border 5 :hgap 5 :vgap 5
                :north "Dereplicate your contigs!"
                :east (border-panel :border 2 :hgap 0 :vgap 2
                                    :north "Input file preview"
                                    :center (scrollable file-preview))
                :south start-work
                :center main-panel))

(def my-app (app my-app-border-panel))

(defn set-current-file!
  [_]
  (let [file (choose-file my-app :type :open)]
    (when file
      (reset! current-file file))))

(defn set-current-outfile!
  [_]
  (let [file (choose-file my-app :type :save)]
    (when file
      (reset! current-outfile file))))

(defn run-derep-gui! []
  (-> my-app pack! show!))

(run-derep-gui!)