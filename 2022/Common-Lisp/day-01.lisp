(defpackage #:day-01
  (:use :cl))

(in-package #:day-01)

(defun all-calories (input-file)
  (let ((calorie 0)
        (calories (list)))
    (with-open-file (stream input-file)
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (if (not (equal "" line))
                    (setq calorie (+ calorie (read-from-string line)))
                    (progn
                      (push calorie calories)
                      (setf calorie 0)))))
    (push calorie calories)
    calories))

(defun day-01-1 (calories)
  (reduce #'max calories))

(defun day-01-2 ()
  (let ((calorie 0)
        (calories (list))
        (rich-elves (list)))
    (with-open-file (stream "input")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (if (not (equal "" line))
                    (setq calorie (+ calorie (read-from-string line)))
                    (progn
                      (push calorie calories)
                      (setf calorie 0)))))
    (push calorie calories)
    (setq rich-elves (push (reduce #'max calories) rich-elves))
    (format t "~A~%" rich-elves)
    (setq calories (remove (car rich-elves) calories))
    (setq rich-elves (push (reduce #'max calories) rich-elves))
    (format t "~A~%" rich-elves)
    (setq calories (remove (car rich-elves) calories))
    (setq rich-elves (push (reduce #'max calories) rich-elves))
    (format t "~A~%" rich-elves)
    (apply #'+  rich-elves)))

(main (input-file)
      (day-01-1 (all-calories input-file)))
