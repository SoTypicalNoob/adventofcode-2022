(ql:quickload "str")

(defun input (input-file)
  (uiop:read-file-lines input-file))

(defun convert-line (input)
  (loop :for item :in (str:split "x" input)
        :collect (parse-integer item)))

(defun dimension-calculator (dimensions)
  (+ (* 2 (nth 0 dimensions) (nth 1 dimensions))
     (* 2 (nth 1 dimensions) (nth 2 dimensions))
     (* 2 (nth 0 dimensions) (nth 2 dimensions))))

(defun smallest-side (dimensions)
  (let* ((a (* (nth 0 dimensions) (nth 1 dimensions)))
         (b (* (nth 1 dimensions) (nth 2 dimensions)))
         (c (* (nth 0 dimensions) (nth 2 dimensions)))
         (sides (list a b c)))
    (apply 'min sides)))

(defun day-02-1 (input &optional (sum 0))
  (if input (let* ((dimensions (convert-line (car input))))
              (format t "~A~%" dimensions)
              (day-02-1 (cdr input) (+ sum
                                       (dimension-calculator dimensions)
                                       (smallest-side dimensions))))
      sum))

(defun ribbon-wrap (dimensions)
  (setq dimensions (sort dimensions #'<))
  (+ (* 2 (first dimensions))
     (* 2 (second dimensions))))

(defun day-02-2 (input &optional (sum 0))
  (if input (let* ((dimensions (convert-line (car input))))
              (day-02-2 (cdr input) (+ sum
                                       (ribbon-wrap dimensions)
                                       (apply '* dimensions))))
      sum))
