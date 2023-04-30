(ql:quickload "str")

(defun input (input-file)
  (uiop:read-file-lines input-file))

(defun input-to-list (input-file)
  (let* ((input (input input-file)))
    (remove "" (loop :for line :in input
                     :collect (str:replace-all "," " "
                                               (str:replace-all "[" "("
                                                                (str:replace-all "]" ")"
                                                                                 line)))) :test 'equal)))

(defun item-comp (left right &optional (winner nil)) ; winner: left or right
  (if (and left right)
      (let* ((lft-item (car left))
             (rght-item (car right)))
        (format t "lft-item: ~A~%rght-item: ~A~%" lft-item rght-item)
        (item-comp (cdr left) (cdr right)))
      winner))

(defun side-comp (input &optional (cnt 0))
  (if input
      (let* ((left (read-from-string (car input)))
             (right (read-from-string (car (cdr input)))))
        (format t "~A~%~A~%" left right)
        (setq cnt (item-comp left right cnt))
        (side-comp (cdr (cdr input)))))
  cnt)

(defun day-13-1 (input-file)
  (let* ((input (input-to-list input-file)))
    (parse-lists input)))
