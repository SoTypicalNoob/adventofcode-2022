(defun input ()
  (with-open-file (stream "input-06")
    (car (loop :for line = (read-line stream nil nil)
               :while line
               :collect (concatenate 'list  line)))))

(defun day-06-1 (input &optional (counter 4))
  (if (= 4 (length (remove-duplicates `(
                                        ,(nth 0 input)
                                        ,(nth 1 input)
                                        ,(nth 2 input)
                                        ,(nth 3 input)))))
      counter
      (day-06-1 (cdr input) (+ counter 1))))

(defun day-06-2 (input &optional (counter 14))
  (if (= 14 (length (remove-duplicates (loop :for i :from 0 :to 13 :collect (nth i input)))))
      counter
      (day-06-2 (cdr input) (+ counter 1))))
