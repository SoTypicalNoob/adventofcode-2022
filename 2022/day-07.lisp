(defun input-to-list ()
  (with-open-file (stream "test")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (uiop:split-string line :separator " "))))

(defun structured-list (input)
  (let ((output (list))
        (buffer (list))
        (a "")
        (entry '())
        (folder "/")
        (deepness 0))
    (loop :for item :in input
          :do (progn
                (cond (()))
                ))
    output))

(defparameter *test* '(((intern "a") . B)))

(defun test (input)
  (let ((a "")
        (b "")
        (output '()))
    (setq *test* '())
    (loop :for item :in input
          :do (progn
                (setf a (car item))
                (setf b (cdr item))
                (push `(,(intern a) . ,b) output)) ;; or intern??? make-symbol???
          )
    output))

;; CL-USER> (substitute '("c" "d") "b" '("a" "b" "c") :test #'equal)
;; ("a" ("c" "d") "c")

(defun input-to-list ()
  (with-open-file (stream "test")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (uiop:split-string line :separator " "))))

(defparameter *output* (list))

(defun command (input)
  (let ((buffer (list))
        (deepness 0))
    (loop :for item :in input
          :do (progn
                (cond
                  ((and (equal "$" (first item))
                        (equal "cd" (second item))
                        (equal "/" (third item)))
                   (push (third item) buffer)))
                ))))

(defun test (input)
  (let ((a "")
        (b "")
        (output '()))
    (setq *test* '())
    (loop :for item :in input
          :do (progn
                (setf a (car item))
                (setf b (cdr item))
                (push `(,(intern a) . ,b) output)) ;; or intern??? make-symbol???
          )
    output))

(defparameter *test* '((foo . "foo") (bar . "bar")))

(defparameter *test* (list (cons ' "foo") (cons 'bar "bar")))

(defmacro var-to-sym (a b)
  `(,a ,b))

(defun input-to-list ()
  (with-open-file (stream "test")
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (progn
                (if ())))))
