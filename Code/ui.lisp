(in-package :regrind-ee)

(setf (cl-who:html-mode) :html5)

(defun start-ui ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/tufte.min.js"
       (merge-pathnames "Static/tufte.min.js"
                        (asdf:system-source-directory :regrind-ee)))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/style.css"
       (merge-pathnames "Static/style.css"
                        (asdf:system-source-directory :regrind-ee)))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/logo.svg"
       (merge-pathnames "Static/logo.svg"
                        (asdf:system-source-directory :regrind-ee)))
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (page :uri "/") ()
  (who:with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:title "Regrind EE")
      (:script :src "https://d3js.org/d3.v4.min.js")
      (:script :src "/tufte.min.js")
      (:meta :http-equiv "refresh" :content "5")
      (:link :rel "stylesheet" :href "/style.css"))
     (:body
      (:span :style "float: right" "Running on " (:i (who:esc (machine-instance))))
      (:h1 (:img :src "/logo.svg") "Regrind " (:span :class "small" "Enterprise Edition"))
      (:h2 "Performance")
      (:h3 "Throughput")
      (:div :id "regex-counter")
      (:script
       (who:str
        (format nil "new tufte.LinePlot('#regex-counter', [~{{x: ~d, y: ~d}~^, ~}], {'label': {'x': 'Seconds ago', 'y': 'Regex/second'}});"
                (loop for ((count nil) (count-1 nil)) on *log*
                      for n = 0 then (1- n)
                      unless (null count-1)
                        collect (* *log-interval* n)
                        and collect (/ (- count count-1)
                                       *log-interval*)))))
      (:h3 "Heap used")
      (:div :id "heap")
      (:script
       (who:str
        (format nil "new tufte.LinePlot('#heap', [~{{x: ~d, y: ~d}~^, ~}], {'label': {'y': 'MB used'}});"
                (loop for (nil bytes) in *log*
                      for n = 0 then (1- n)
                      collect (* *log-interval* n)
                      collect (round bytes 1000000)))))
      
      (:h2 "Crashes")
      (:ul
       (loop for (reason re haystack) in (state-losers *state*)
             do (who:htm
                 (:li (who:esc reason) " for "
                      (:pre (who:esc re)) " with haystack "
                      (:pre (who:esc haystack))))))))))
