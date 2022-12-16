(defun input ()
  (uiop:read-file-lines "test"))

(defparameter *input-file* (input))

(member T (loop :for i :from 2 :to (isqrt n)
                :collect (integerp (/ n i))))

(defun pkg-comp (1st-pkg 2nd-pkg)
  (format t "~&~{~A~%~}~%" (list 1st-pkg 2nd-pkg))
  (loop :for 1st :in 1st-pkg
        :for 2nd :in 2nd-pkg
        :collect (progn
                   (cond ((and (integerp 1st) (integerp 2nd) (>= 1st 2nd))
                          T))
                   (cond ((and (listp 1st) (listp 2nd))
                          ())))))

(defun day-13-1 (input &optional (packages (list)))
  (if input
      (progn
        (if (not (equal (car input) ""))
            (progn
              (push (read-from-string (substitute #\space #\, (substitute #\) #\] (substitute #\( #\[ (car input))))) packages)
              (day-13-1 (cdr input) packages))
            (progn
              (pkg-comp (first packages) (second packages))
              (day-13-1 (cdr input) (list)))))
      packages))

