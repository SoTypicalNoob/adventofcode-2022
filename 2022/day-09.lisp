(defun input-to-list ()
  (with-open-file (stream "test")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (list (first (uiop:split-string line :separator " "))
                         (read-from-string (second (uiop:split-string line :separator " ")))))))

(defun update-head (head way)
  (cond ((equal "L" way)
         (format t "L ~%")
         (push (list (1- (first (car head))) (second (car head))) head))
        ((equal "R" way)
         (push (list (1+ (first (car head))) (second (car head))) head))
        ((equal "U" way)
         (push (list (first (car head)) (1- (second (car head)))) head))
        ((equal "D" way)
         (push (list (first (car head)) (1+ (second (car head)))) head)))
    head)
(defun update-head (head way)
  (let ((new-item (list)))
    (format t "~A~%" way)
    (cond ((equal "L" (first way))
           (setq new-item (list (- (first (car head)) (second way)) (second (car head)))))
          ((equal "R" (first way))
           (setq new-item (list (+ (first (car head)) (second way)) (second (car head)))))
          ((equal "U" (first way))
           (setq new-item (list (first (car head)) (+ (second (car head)) (second way)))))
          ((equal "D" (first way))
           (setq new-item (list (first (car head)) (- (second (car head)) (second way))))))
    new-item))

(defun update-tail (tail head)
  (let ((h-x (first (car head)))
        (h-y (second (car head)))
        (t-x (first (car tail)))
        (t-y (second (car tail))))
    (cond
      ((and (= h-x t-x) (> h-y t-y))    ; done
       (loop :for i :from t-y :to (1- h-y)
             :do (progn
                   (push (list t-x i) tail))))
      ((and (= h-x t-x) (< h-y t-y))    ; done
       (loop :for i :from t-y :downto (1+ h-y)
             :do (progn
                   (push (list t-x i) tail))))
      ((and (= h-x t-x) (> h-y t-y))
       (loop :for i :from t-y :to (1- h-y)
             :do (progn
                   (push (list t-x i) tail))))
      ((and (= h-x t-x) (> h-y t-y))
       (loop :for i :from t-y :to (1- h-y)
             :do (progn
                   (push (list t-x i) tail))))
      )
    tail))

(defun day-9-1 (moves &optional (head (list (list 0 0))) (tail (list (list 0 0))))
  (let ((way (car moves)))
    (if moves
        (progn
          (push (update-head head way) head)
          (day-9-1 (cdr moves) head))
        head)))
