(defun input-to-array ()
  (let ((array-output (make-array 1 :adjustable t)))
    (with-open-file (stream "test")
      (loop :for line = (read-line stream nil nil)
            :while line
            :collect (coerce (mapcar #'char-int (coerce line 'list)) 'vector)))))

(defun input ()
  (uiop:read-file-lines "test"))

(defparameter *input-file* (input))

(defun day-12-1 (input-file)
  (let* ((num-lines (length input-file))
         (num-line (length (car input-file)))
         (topo-map (make-array (list num-lines num-line))))
    (mapcar #lambda (x)
            )
    ))
