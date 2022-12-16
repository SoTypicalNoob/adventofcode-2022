(defun input (input-file)
  (uiop:read-file-lines input-file))

(defparameter *input-file* (input "test"))

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
         (bx-min (nth 0 min-max))
         (by-min (nth 1 min-max))
         (cave-array (cave-array cave))
         (x-max (1- (second (array-dimensions cave-array))))
         (y-max (1- (first (array-dimensions cave-array))))
         (bx1 0)
         (sx1 0)
         (by1 0)
         (sy1 0)
         (manhattan 0))

    (loop :for beacon :in cave
          :do (progn
                (setq bx1 (- (nth 0 beacon) bx-min))
                (setq by1 (- (nth 1 beacon) by-min))
                (setq sx1 (- (nth 2 beacon) bx-min))
                (setq sy1 (- (nth 3 beacon) by-min))
                (setq manhattan (manhattan-calc (list bx1 by1 sx1 sy1)))
                (loop :for y :from 0 :to y-max
                      :do (progn
                            (format t "~A" beacon)
                            (loop :for x :from 0 :to x-max
                                  :do (progn
                                        (if (and (<= (manhattan-calc (list bx1 by1 x y)) manhattan)
                                                 (not (equal (aref cave-array y x) "S"))
                                                 (not (equal (aref cave-array y x) "B")))
                                            (format t "#")
                                            (format t "~A" (aref cave-array y x)))))
                            (format t "~%")))))
    ))

(defun print-array (input-array)
  (let ((x-max (1- (second (array-dimensions input-array))))
        (y-max (1- (first (array-dimensions input-array)))))
    (loop :for y :from 0 :to y-max
          :do (progn (loop :for x :from 0 :to x-max
                           :do (format t "~A" (aref input-array y x)))
                     (format t "~%")))))

;;;;
    (loop for i from 0 below n do
      (loop for j from 0 below m do
        (format t "a[~a ~a] = ~a~%" i j (aref a i j))))

CL-USER> (uiop:split-string "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" :separator ":")
("Sensor at x=2, y=18" " closest beacon is at x=-2, y=15")
CL-USER> (uiop:split-string "Sensor at x=2, y=18" :separator ",")
("Sensor at x=2" " y=18")
CL-USER> (subseq "Sensor at x=2" 13)
""
CL-USER> (subseq "Sensor at x=2" 12)
"2"
CL-USER> (read-from-string (subseq "Sensor at x=2" 12))
2 (2 bits, #x2, #o2, #b10)
1 (1 bit, #x1, #o1, #b1)
CL-USER> (subseq "Sensor at x=2" 12)
"2"
CL-USER> (read-from-string (subseq "Sensor at x=2" 12))
2 (2 bits, #x2, #o2, #b10)
1 (1 bit, #x1, #o1, #b1)
