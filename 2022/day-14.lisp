(defun input (file-name)
  (uiop:read-file-lines file-name))

(defun cave-gen (input)
  (let ((all-coors '()))
    (setq all-coors
          (loop :for line :in input
                :collect (loop :for item :in (uiop:split-string line :separator ":")
                               :collect (list
                                          (read-from-string (first (uiop:split-string item :separator ",")))
                                          (read-from-string (second (uiop:split-string item :separator ",")))))))
    all-coors))

(defun min-max (cave &optional (x-min 498) (y-min 4) (x-max 503) (y-max 6))
  (if cave
      (progn
        (setq x-min (loop :for item :in (car cave) :minimize (first item)))
        (setq y-min (loop :for item :in (car cave) :minimize (second item)))
        (setq x-max (loop :for item :in (car cave) :maximize (first item)))
        (setq y-min (loop :for item :in (car cave) :maximize (second item)))
        (min-max (cdr cave) x-min y-min x-max y-max))
      (list x-min y-min x-max y-max)))

(defun cave-array (cave, minmax)
  (let ((cave-matrix (make-array )))))
