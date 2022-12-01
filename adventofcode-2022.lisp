(defpackage #:adventofcode-2022
  (:use :cl))

(in-package #:adventofcode-2022)

(defun day-01-1 ()
  (let ((calorie 0)
        (calories (list)))
    (with-open-file (stream "input")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (if (not (equal "" line))
                    (setq calorie (+ calorie (read-from-string line)))
                    (progn
                      (push calorie calories)
                      (setf calorie 0)))))
    (push calorie calories)
    (reduce #'max calories)))

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
