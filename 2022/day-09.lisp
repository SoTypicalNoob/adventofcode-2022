(defun input-to-list ()
  (with-open-file (stream "input-09")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (list (first (uiop:split-string line :separator " "))
                         (read-from-string (second (uiop:split-string line :separator " ")))))))

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
    (format t "New item: ~A~%" new-item)
    new-item))

(defun update-tail (head tail)
  (let ((h-x (first head))
        (h-y (second head))
        (t-x (first (car tail)))
        (t-y (second (car tail))))
    (format t "Tail init status: ~A~%" (car tail))
    (cond
      ((and (= h-x t-x) (> h-y t-y))    ; h and t are on x, h moves up on x
       (loop :for i :from t-y :to (1- h-y)
             :do (progn
                   (format t "h and t are on x, h moves up on x~%~A, ~A~%" t-x i)
                   (push (list t-x i) tail))))
      ((and (= h-x t-x) (< h-y t-y))    ; h and t are on x, h moves down on x
       (loop :for i :from t-y :downto (1+ h-y)
             :do (progn
                   (format t "h and t are on x, h moves down on x~%~A, ~A~%" t-x i)
                   (push (list t-x i) tail))))
      ((and (= h-y t-y) (> h-x t-x)) ; h and t are on y, h moves to the right on x
       (loop :for i :from t-x :to (1- h-x)
             :do (progn
                   (format t "h and t are on y, h moves to the right on x~%~A, ~A~%" i t-y)
                   (push (list i t-y) tail))))
      ((and (= h-y t-y) (< h-x t-x)) ; h and t are on y, h moves to the left on x
       (loop :for i :from t-x :to (1+ h-x)
             :do (progn
                   (format t "h and t are on y, h moves to the left on x~%~A, ~A~%" i t-y)
                   (push (list i t-y) tail))))
      ((and (or (= (abs (- h-x t-x)) 1) ; h moves up, h and t are not on the same y
                (= (abs (- t-x h-x)) 1))
            (> (- h-y t-y) 1))
       (loop :for i :from (1+ t-y) :to (1- h-y)
             :do (progn
                   (format t "h moves up, h and t are not on the same y~%~A, ~A~%" h-x i)
                   (push (list h-x i) tail))))
      ((and (or (= (abs (- h-x t-x)) 1) ; h moves down, h and t are not on the same y
                (= (abs (- t-x h-x)) 1))
            (> (- t-y h-y) 1))
       (loop :for i :from (1- t-y) :downto (1+ h-y)
             :do (progn
                   (format t "h moves down, h and t are not on the same y~%~A, ~A~%" h-x i)
                   (push (list h-x i) tail))))
      ((and (or (= (abs (- h-y t-y)) 1) ; h moves to the right, h and t are not on the same x
                (= (abs (- t-y h-y)) 1))
            (> (- h-x t-x) 1))
       (loop :for i :from (1+ t-x) :to (1- h-x)
             :do (progn
                   (format t "h moves to the right, h and t are not on the same x~%~A, ~A~%" i h-y)
                   (push (list i h-y) tail))))
      ((and (or (= (abs (- h-y t-y)) 1) ; h moves to the left, h and t are not on the same x
                (= (abs (- t-y h-y)) 1))
            (> (abs (- t-x h-x)) 1))
       (loop :for i :from (1- t-x) :downto (1+ h-x)
             :do (progn
                   (format t "h moves to the left, h and t are not on the same x~%~A, ~A~%" i h-y)
                   (push (list i h-y) tail))))
      )
    (format t "Tail new status: ~A~%" (car tail))
    tail))

(defun day-09-1 (moves &optional (head (list (list 0 0))) (tail (list (list 0 0))))
  (let ((way (car moves)))
    (if moves
        (progn
          (push (update-head head way) head)
          (setq tail (update-tail (car head) tail))
          (day-09-1 (cdr moves) head tail))
        (progn
          (format t "Head: ~A~%Tail: ~A~%" head (remove-duplicates tail :test #'(lambda (x y)
                                                    (and (= (car x) (car y))
                                                         (= (second x) (second y))))))
          (length (remove-duplicates tail :test #'(lambda (x y)
                                                    (and (= (car x) (car y))
                                                         (= (second x) (second y))))))))))

;; ..##..
;; ...##.
;; .####.
;; ....#.
;; s###..
;;
;; ..##..
;; ...##.
;; .####.
;; ....#.
;; s###..
