(defpackage #:day-02
  (:use :cl))

(in-package #:day-02)


(defun day-02-1 ()
  (let ((score 0))
    (with-open-file (stream "input-02")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (setq score (+ score (cond
                                       ((equal line "A X") 4)
                                       ((equal line "A Y") 8)
                                       ((equal line "A Z") 3)

                                       ((equal line "B X") 1)
                                       ((equal line "B Y") 5)
                                       ((equal line "B Z") 9)

                                       ((equal line "C X") 7)
                                       ((equal line "C Y") 2)
                                       ((equal line "C Z") 6)
                                       (t 0)))))
      (format t "~A~%" score))))

(defun day-02-2 ()
  (let ((score 0))
    (with-open-file (stream "input-02")
      (loop :for line = (read-line stream nil nil)
            :while line
            :do (setq score (+ score (cond
                                       ((equal line "A X") 3)
                                       ((equal line "A Y") 4)
                                       ((equal line "A Z") 8)

                                       ((equal line "B X") 1)
                                       ((equal line "B Y") 5)
                                       ((equal line "B Z") 9)

                                       ((equal line "C X") 2)
                                       ((equal line "C Y") 6)
                                       ((equal line "C Z") 7)
                                       (t 0)))))
      (format t "~A~%" score))))
