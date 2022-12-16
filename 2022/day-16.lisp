(defun input (input-file)
  (uiop:read-file-lines input-file))

(defparameter *input-file* (input "test"))
