(defun input (file-name)
  (uiop:read-file-lines file-name))

(defun rocks (input)
  (let ((rocks '())
        (temp-coor-1 '())
        (temp-coor-2 '()))
    (loop :for line :in input
          :do (loop :for coor :in (uiop:split-string (remove #\space (remove #\> line)) :separator "-")
                    :do
                       (setq temp-coor-1 (list (read-from-string (first (uiop:split-string coor :separator ",")))
                                               (read-from-string (second (uiop:split-string coor :separator ",")))))
                       (when (not (= (length temp-coor-2) 0))
                         (when (= (first temp-coor-2) (first temp-coor-1))
                           (if (> (second temp-coor-1) (second temp-coor-2))
                               (loop :for rock :from (second temp-coor-2) :to (second temp-coor-1)
                                     :do (push (list (first temp-coor-1) rock) rocks))
                               (loop :for rock :from (second temp-coor-1) :to (second temp-coor-2)
                                     :do (push (list (first temp-coor-1) rock) rocks))))
                         (when (and (= (second temp-coor-2) (second temp-coor-1))
                                    (> (first temp-coor-2) (first temp-coor-1)))
                           (loop :for rock :from (first temp-coor-1) :to (first temp-coor-2)
                                 :do (push (list rock (second temp-coor-1)) rocks)))
                         (when (and (= (second temp-coor-2) (second temp-coor-1))
                                    (> (first temp-coor-1) (first temp-coor-2)))
                           (loop :for rock :from (first temp-coor-2) :to (first temp-coor-1)
                                 :do (push (list rock (second temp-coor-1)) rocks)))
                         )
                       (setq temp-coor-2 temp-coor-1)
                    )
              (setq temp-coor-1 '())
              (setq temp-coor-2 '())
          )
    ;; (remove-duplicates rocks :test 'equal)
    rocks))

(defun min-max-xy (input)
  (loop :for item :in input
        :minimize (first item) :into x-min
        :maximize (first item) :into x-max
        :minimize (second item) :into y-min
        :maximize (second item) :into y-max
        :finally (return (list x-min x-max y-min y-max))))

(defun min-500 (rocks sands)
  (loop :for min-500 :in (list (loop :for rock-y :in (remove nil (mapcar #'(lambda (x)
                                                                   (when (equal (first x) 500)
                                                                     (second x))) rocks))
                                     :minimize rock-y)
                               (loop :for sand-y :in (remove nil (mapcar #'(lambda (x)
                                                                             (when (equal (first x) 500)
                                                                               (second x))) sands))
                                     :minimize sand-y))
        :minimize min-500))

(defun place-sand (rocks sands y-max &optional (x 500) (y 0) (cnt 0))
  (let* ((coors (append rocks sands)))
    ;; (format t "~A~%" coors)
    (cond ((> y y-max)
           (format t "end: ~A, ~A~%" y cnt)
           cnt)
          ((and (find (list x y) coors :test 'equal)
                (not (find (list (1- x) y) coors :test 'equal)))
           (format t "left main~%")
           (if (and (find (list (1- x) (1+ y)) coors :test 'equal)
                    (find (list (- x 2) (1+ y)) coors :test 'equal))
               (progn
                 (format t "left, rest~%")
                 (push (list (1- x) y) sands)
                 (place-sand rocks sands y-max 500 0 (1+ cnt)))
               (progn
                 (format t "left, free fall~%")
                 (place-sand rocks sands y-max (1- x) (1+ y) cnt))))
          ((and (find (list x y) coors :test 'equal)
                (find (list (1- x) y) coors :test 'equal)
                (not (find (list (1+ x) y) coors :test 'equal)))
           (format t "right main~%")
           (if (and (find (list (1+ x) (1+ y)) coors :test 'equal)
                    (find (list (+ x 2) (1+ y)) coors :test 'equal))
               (progn
                 (format t "right, rest~%")
                 (push (list (1+ x) y) sands)
                 (place-sand rocks sands y-max 500 0 (1+ cnt)))
               (progn
                 (format t "right, free fall~%")
                 (place-sand rocks sands y-max (1+ x) (1+ y) cnt))))
          ((and (find (list x y) coors :test 'equal)
                (find (list (1- x) y) coors :test 'equal)
                (find (list (1+ x) y) coors :test 'equal))
           (format t "rest main~%")
           (push (list x (1- y)) sands)
           (place-sand rocks sands y-max 500 0 (1+ cnt)))
          (t (progn
               (format t "y increment: ~A~%" y)
               (place-sand rocks sands y-max x (1+ y) cnt))))))

(defun day-14-1 (input-file)
  (let* ((rocks (rocks (input input-file)))
         (sands '())
         (min-max-xy (min-max-xy rocks))
         (x-min (nth 0 min-max-xy))
         (x-max (nth 1 min-max-xy))
         (y-min (nth 2 min-max-xy))
         (y-max (nth 3 min-max-xy))
         (highest-y 0))
    (place-sand rocks sands y-max)))

0 ......+...
1 ..........
2 ..........
3 ......s...
4 .....sss##
5 ....#sss#.
6 ...##sss#.
7 ..s.ssss#.
8 .sssssss#.
9 #########.
