(in-package :regrind-ee)

(defvar *start*)
(defvar *end*)

(defun checked-string-ref (string index)
  (assert (and (<= *start* index) (< index *end*)))
  (aref string index))

(defvar *layout*
  (one-more-re-nightmare::make-layout
   :ref 'checked-string-ref))

(defvar *remaining-depth* 4)
(defun random-re ()
  (macrolet ((terminal ()
               ;; A random element of [A-Z].
               '(string (code-char (+ 65 (random 26)))))
             (recurse (control n)
               `(format nil ,control ,@(loop repeat n collect '(random-re)))))
    (if (zerop *remaining-depth*)
        (terminal)
        (let ((*remaining-depth* (1- *remaining-depth*)))
          (case (random 8)
            (0 (terminal))
            (1 (recurse "~a~a" 2))
            (2 (recurse "(~a)" 1))
            (3 (recurse "«~a»" 1))
            (4 (recurse "(~a)|(~a)" 2))
            (5 (recurse "(~a)&(~a)" 2))
            (6 (recurse "(¬~a)" 1))
            (7 (recurse "(~a)*" 1)))))))

(defun random-haystack ()
  (let* ((n (random 80))
         (haystack (make-string n)))
    (dotimes (i n)
      (setf (char haystack i) (code-char (+ 65 (random 26)))))
    haystack))

(defstruct state
  threads
  (loser-lock (bt:make-lock))
  (losers '()))
(defstruct thread-state
  (done 0)
  current-re
  current-haystack)

(defun report-loser (state reason re haystack)
  (bt:with-lock-held ((state-loser-lock state))
    (push (list reason re haystack) (state-losers state))))

(defun regrind-worker (state thread-state depth)
  (lambda ()
    (loop
      (let* ((*remaining-depth* depth)
             #+sbcl (one-more-re-nightmare::*code-type* :interpreted)
             (re (random-re))
             (haystack (random-haystack)))
        (setf (thread-state-current-re thread-state) re
              (thread-state-current-haystack thread-state) haystack)
        (handler-case
            (one-more-re-nightmare::%compile-regular-expression
             re
             :layout *layout*)
          (one-more-re-nightmare:exceeded-state-limit ())
          (error () (report-loser state "Compilation failed" re haystack))
          (:no-error (code registers)
            (let ((result (make-array registers))
                  (*start* 0)
                  (*end* (length haystack)))
              (handler-case
                  (funcall code haystack 0 (length haystack) result
                           (lambda ()
                             (loop for p across result
                                   do (unless (or (null p) (<= 0 p *end*))
                                        (report-loser state "Bogus registers" re haystack)
                                        (return)))))
                (error () (report-loser state "Execution failed" re haystack))))))
        (incf (thread-state-done thread-state))))))

(defvar *state*)

(defun start-regrind (threads &key (depth 4))
  (setf *state* (make-state :threads (make-array threads)))
  (dotimes (i threads)
    (let ((thread-state (make-thread-state)))
      (setf (aref (state-threads *state*) i) thread-state)
      (bt:make-thread (regrind-worker *state* thread-state depth))))
  (bt:make-thread #'logger))

(defvar *log-lock* (bt:make-lock))
(defvar *log* '())
(defvar *log-length* 36000)
(defvar *log-interval* 0.1)

(defun logger ()
  (loop
    (bt:with-lock-held (*log-lock*)
      (push (list (reduce #'+ (state-threads *state*)
                          :key #'thread-state-done)
                  (sb-kernel:dynamic-usage))
            *log*)
      (when (> (length *log*) *log-length*)
        (setf *log* (subseq *log* 0 *log-length*))))
    (sleep *log-interval*)))
