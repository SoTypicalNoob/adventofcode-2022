(ql:quickload "str")

(defun input (input-file)
  (uiop:read-file-lines input-file))

(defun day-01-1 (input-file)
  (let* ((input (input input-file))
         (flr 0)
         (cnt 0))
    (loop :for line :in input
          :do (loop :for item :across line
                    :do (if (string= item "(")
                            (incf flr)
                            (decf flr))
                        (incf cnt)
                        (if (= flr -1)
                            (format t "~A" cnt)
                            )))
    flr))

(day-01-1 "inputs/input-01")

(defun day-01-2 (input-file)
  (let* ((input (input input-file))
         (flr 0)
         (cnt 0))
    (loop :for line :in input
          :do (loop :for item :across line
                    :do (if (string= item "(")
                            (incf flr)
                            (decf flr))
                        (incf cnt)
                    :when (= flr -1)
                      :return cnt))
    cnt))

(day-01-2 "inputs/inpt-01")
