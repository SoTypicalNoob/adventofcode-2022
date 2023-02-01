(defun input (input-file)
  (uiop:read-file-lines input-file))

(defun droplets (droplets)
  (loop :for droplet :in droplets
        :collect (mapcar #'(lambda (x)
                             (read-from-string x))
                         (uiop:split-string droplet :separator ","))))

(defun day-18-1 (droplets)
  (let* ((cntr 0))
    (setq droplets (remove-duplicates droplets :test 'equal))
    (loop :for droplet :in droplets
          :do (progn
                  (when (not (find (list (1+ (first droplet)) (second droplet) (third droplet)) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  (when (not (find (list (1- (first droplet)) (second droplet) (third droplet)) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  (when (not (find (list (first droplet) (1+ (second droplet)) (third droplet)) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  (when (not (find (list (first droplet) (1- (second droplet)) (third droplet)) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  (when (not (find (list (first droplet) (second droplet) (1+ (third droplet))) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  (when (not (find (list (first droplet) (second droplet) (1- (third droplet))) droplets :test 'equal))
                   (setq cntr (1+ cntr)))
                  ))
    cntr))

(defun min-max-xyz (input)
  (loop :for item :in input
        :minimize (first item) :into x-min
        :maximize (first item) :into x-max
        :minimize (second item) :into y-min
        :maximize (second item) :into y-max
        :minimize (third item) :into z-min
        :maximize (third item) :into z-max
        :finally (return (list x-min x-max y-min y-max z-min z-max)))
  )

(defun day-18-2 (droplets)
  (let* ((min-max (min-max-xyz droplets))
         (x-min (nth 0 min-max))
         (x-max (nth 1 min-max))
         (y-min (nth 2 min-max))
         (y-max (nth 3 min-max))
         (z-min (nth 4 min-max))
         (z-max (nth 5 min-max))
         (air (list))
         (cntr 0))
    (loop :for x :from (1+ x-min) :to (1- x-max)
          :do (loop :for y :from (1+ y-min) :to (1- y-max)
                    :do (loop :for z :from (1+ z-min) :to (1- z-max)
                              :do (progn
                                    (if (and (not (find (list x y z) droplets :test 'equal))
                                             (or
                                              (find (list (1+ x) y z) droplets :test 'equal)
                                              (find (list (1- x) y z) droplets :test 'equal)
                                              (find (list x (1+ y) z) droplets :test 'equal)
                                              (find (list x (1- y) z) droplets :test 'equal)
                                              (find (list x y (1+ z)) droplets :test 'equal)
                                              (find (list x y (1- z)) droplets :test 'equal))
                                             )
                                        (progn (setq cntr (1+ cntr))
                                               (push (list x y z) air)))))))
    ;; (- (day-18-1 droplets) (* 6 cntr))
    (- (day-18-1 droplets) (day-18-1 air))))
