(defun space-check (input)
  (if (not (equal " " input))
      input
      " "))

(defun stacks ()
  (let ((stack (list (list) (list) (list) (list) (list) (list) (list) (list) (list))))
    (with-open-file (stream "input-05-crates")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (progn
                  (push (space-check (subseq line 1 2)) (nth 0 stack))
                  (push (space-check (subseq line 5 6)) (nth 1 stack))
                  (push (space-check (subseq line 9 10)) (nth 2 stack))
                  (push (space-check (subseq line 13 14)) (nth 3 stack))
                  (push (space-check (subseq line 17 18)) (nth 4 stack))
                  (push (space-check (subseq line 21 22)) (nth 5 stack))
                  (push (space-check (subseq line 25 26)) (nth 6 stack))
                  (push (space-check (subseq line 29 30)) (nth 7 stack))
                  (push (space-check (subseq line 33 34)) (nth 8 stack))
                  )))
    (mapcar #'(lambda (x)
                (remove " " x :test 'equal)) stack)))

(defun day-05-1 ()
  (let ((line-list (list))
        (move 0)
        (from 0)
        (to 0))
    (with-open-file (stream "input-05-moves")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (progn
                  (setq line-list (uiop:split-string line :separator " "))
                  (setq move (read-from-string (nth 1 line-list)))
                  (setq from (- (read-from-string (nth 3 line-list)) 1))
                  (setq to (- (read-from-string (nth 5 line-list)) 1))
                  (loop :for i :from 1 :to move :do
                    (push (pop (nth from *stacks*)) (nth to *stacks*))))))
    (mapcar #'car  *stacks*)))

(defparameter *stacks* (mapcar #'reverse (stacks)))

(defun day-05-2 ()
  (let ((line-list (list))
        (move 0)
        (from 0)
        (to 0))
    (with-open-file (stream "input-05-moves")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (progn
                  (format t "~A~%" *stacks*)
                  (setq line-list (uiop:split-string line :separator " "))
                  (setq move (read-from-string (nth 1 line-list)))
                  (setq from (- (read-from-string (nth 3 line-list)) 1))
                  (setq to (- (read-from-string (nth 5 line-list)) 1))
                  (mapcar #'(lambda (x)
                              (push x (nth to *stacks*))) (reverse (loop :for i :from 1 :to move :collect
                                                                                                 (pop (nth from *stacks*))))))))
    (format t "~A~%" *stacks*)
    (mapcar #'car  *stacks*)))


;; CL-USER> (nth (- (read-from-string (nth 3 *test*)) 1) *stacks*)
;; ("W" "R" "V" "Q" "F" "N" "J" "C")
