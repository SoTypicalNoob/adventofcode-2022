(defun input (input-file)
  (uiop:read-file-lines input-file))

(defparameter *input-file* (input "example-15"))

(defun cave-gen (input)
  (let ((all-coors '()))
    (setq all-coors
          (loop :for line :in input
                :collect (list
                          (read-from-string (subseq (first (uiop:split-string
                                                            (first (uiop:split-string line :separator ":")) :separator ",")) 12))
                          (read-from-string (subseq (second (uiop:split-string
                                                             (first (uiop:split-string line :separator ":")) :separator ",")) 3))
                          (read-from-string (subseq (first (uiop:split-string
                                                            (second (uiop:split-string line :separator ":")) :separator ",")) 24))
                          (read-from-string (subseq (second (uiop:split-string
                                                             (second (uiop:split-string line :separator ":")) :separator ",")) 3))
                          )))
    all-coors))

(defun min-max (cave &optional (x-min -2) (y-min 15) (x-max 2) (y-max 18))
  (if cave
      (progn
        (when (> x-min (nth 0 (car cave))) (setq x-min (nth 0 (car cave))))
        (when (> x-min (nth 2 (car cave))) (setq x-min (nth 2 (car cave))))
        (when (< x-max (nth 0 (car cave))) (setq x-max (nth 0 (car cave))))
        (when (< x-max (nth 2 (car cave))) (setq x-max (nth 2 (car cave))))
        (when (> y-min (nth 1 (car cave))) (setq y-min (nth 1 (car cave))))
        (when (> y-min (nth 3 (car cave))) (setq y-min (nth 3 (car cave))))
        (when (< y-max (nth 1 (car cave))) (setq y-max (nth 1 (car cave))))
        (when (< y-max (nth 3 (car cave))) (setq y-max (nth 3 (car cave))))
        (min-max (cdr cave) x-min y-min x-max y-max))
      (list x-min y-min x-max y-max)))

(defun cave-array (cave)
  (let* ((min-max (min-max cave))
         (x-min (nth 0 min-max))
         (x-max (nth 2 min-max))
         (y-min (nth 1 min-max))
         (y-max (nth 3 min-max))
         (cave-array (make-array (list (1+ (- y-max y-min))
                                        (1+ (- x-max x-min)))
                                  :initial-element "." :adjustable t)))
    (loop :for line :in cave
          :do (progn
                (setf (aref cave-array
                            (- (nth 1 line) y-min)
                            (- (nth 0 line) x-min)) "S")
                (setf (aref cave-array
                            (- (nth 3 line) y-min)
                            (- (nth 2 line) x-min)) "B")))
    cave-array))

(defun manhattan-calc (coors)
  (let ((x1 (nth 0 coors))
        (y1 (nth 1 coors))
        (x2 (nth 2 coors))
        (y2 (nth 3 coors)))
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defun coverage (cave)
  (let* ((min-max (min-max cave))
         (x-min (nth 0 min-max))
         (y-min (nth 1 min-max))
         (cave-array (cave-array cave))
         (x-max (1- (second (array-dimensions cave-array))))
         (y-max (1- (first (array-dimensions cave-array))))
         (sx1 0) (sy1 0) (bx1 0) (by1 0)
         (manhattan 0)
         (not-beacon 0))
    (loop :for beacon :in cave
          :do (progn
                (setq sx1 (- (nth 0 beacon) x-min))
                (setq sy1 (- (nth 1 beacon) y-min))
                (setq bx1 (- (nth 2 beacon) x-min))
                (setq by1 (- (nth 3 beacon) y-min))
                (setq manhattan (manhattan-calc (list sx1 sy1 bx1 by1)))
                (loop :for y :from 0 :to y-max
                      :do (progn
                            ;; (format t "~A" beacon)
                            (loop :for x :from 0 :to x-max
                                  :do (progn
                                        (if (and (<= (manhattan-calc (list sx1 sy1 x y)) manhattan)
                                                 (not (equal (aref cave-array y x) "S"))
                                                 (not (equal (aref cave-array y x) "B")))
                                            (setf (aref cave-array y x) "#"))))))))
    (loop :for x :from 0 :to x-max
          :do (progn
                (if (equal (aref cave-array 10 x) "#")
                    (setq not-beacon (1+ not-beacon))))
          )
    (format t "Not Beacon: ~A~%" not-beacon)
    cave-array))

(defun print-array (input-array)
  (let ((x-max (1- (second (array-dimensions input-array))))
        (y-max (1- (first (array-dimensions input-array)))))
    (loop :for y :from 0 :to y-max
          :do (progn (loop :for x :from 0 :to x-max
                           :do (format t "~A" (aref input-array y x)))
                     (format t "~%")))))

(defun day-15-1-slow (cave Y)
  (let* ((known (list))
         (occupied (list))
         (sx 0) (sy 0) (bx 0) (by 0)
         (manhattan 0)
         (offset 0)
         (low-x 0)
         (high-x 0))
    (loop :for line :in cave
          :do (progn
                (setq sx (nth 0 line)) (setq sy (nth 1 line))
                (setq bx (nth 2 line)) (setq by (nth 3 line))
                (setq manhattan (manhattan-calc line))
                (setq offset (- manhattan (abs (- sy Y))))
                (when (not (< offset 0))
                  (setq low-x (- sx offset))
                  (setq high-x (+ sx offset))
                  (loop :for x :from low-x :to high-x
                        :do (push x known)))
                (when (= by Y)
                  (push by occupied))))
    (- (length (remove-duplicates known))
       (length (remove-duplicates occupied)))))

;; (defun combine-intervals (intervals &optional (combined (pop intervals)))
;;   (if (cdr intervals)
;;       (let* (( (nth 0 intervals))
;;              (second-line (nth 1 intervals)))
;;         (format t "~A~%" combined)
;;         (format t "~A~%" intervals)
;;         (cond ((<= (second first-line) (first second-line))
;;                (format t "true")))
;;         intervals)))

;; (defun day-15-1-optimized (cave Y)
;;   (let* ((sx 0) (sy 0) (bx 0) (by 0)
;;          (manhattan 0)
;;          (offset 0)
;;          (low-x 0)
;;          (high-x 0)
;;          (intervals (list)))
;;     (loop :for line :in cave
;;           :do (progn
;;                 (setq sx (nth 0 line)) (setq sy (nth 1 line))
;;                 (setq bx (nth 2 line)) (setq by (nth 3 line))
;;                 (setq manhattan (manhattan-calc line))
;;                 (setq offset (- manhattan (abs (- sy Y))))
;;                 (when (not (< offset 0))
;;                   (setq low-x (- sx offset))
;;                   (setq high-x (+ sx offset))
;;                   (push (list low-x high-x) intervals))))
;;     (setq intervals (sort intervals #'< :key #'first))
;;     intervals))

(defun day-15-2-slow (cave y-max x-max)
  (let* ((known (list))
         (occupied (list))
         (sx 0) (sy 0) (bx 0) (by 0)
         (manhattan 0)
         (offset 0)
         (low-x 0)
         (high-x 0))
    (loop :for line :in cave
          :do (progn
                (setq sx (nth 0 line)) (setq sy (nth 1 line))
                (setq bx (nth 2 line)) (setq by (nth 3 line))
                (setq manhattan (manhattan-calc line))
                (setq offset (- manhattan (abs (- sy Y))))
                (when (not (< offset 0))
                  (setq low-x (- sx offset))
                  (setq high-x (+ sx offset))
                  (loop :for x :from low-x :to high-x
                        :do (push x known)))
                (when (= by Y)
                  (push by occupied))))
    (- (length (remove-duplicates known))
       (length (remove-duplicates occupied)))))
