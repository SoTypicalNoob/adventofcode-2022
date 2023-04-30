(defun input-to-list ()
  (with-open-file (stream "input-07")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (coerce line 'list))))

(defun before (forest tree i j)
  (let ((eval "True"))
    (loop :for cnt :from (1- j) :downto 0
          :do (if (<= (digit-char-p tree) (digit-char-p (nth cnt (nth i forest))))
                  (setq eval nil)))
    eval))

(defun after (forest tree i j)
  (let ((eval "True"))
    (loop :for cnt :from (1+ j) :to (1- (length (car forest)))
          :do (if (<= (digit-char-p tree) (digit-char-p (nth cnt (nth i forest))))
                  (setq eval nil)))
    eval))

(defun above (forest tree i j)
  (let ((eval "True"))
    (loop :for cnt :from (1- i) :downto 0
          :do (if (<= (digit-char-p tree) (digit-char-p (nth j (nth cnt forest))))
                  (setq eval nil)))
    eval))

(defun below (forest tree i j)
  (let ((eval "True"))
    (loop :for cnt :from (1+ i) :to (1- (length forest))
          :do (if (<= (digit-char-p tree) (digit-char-p (nth j (nth cnt forest))))
                  (setq eval nil)))
    eval))


(defun day-08-1 (input)
  (let ((visible-trees 0))
    (setq visible-trees (+ visible-trees (* (- (length input) 2) 2)))
    (setq visible-trees (+ visible-trees (* (length (car input)) 2)))
    (loop :for line :in input
          :for i :from 0
          :do (loop :for item :in line
                    :for j :from 0
                    :do (progn
                          (cond ((or (= i 0) (= i (1- (length input)))
                                     (= j 0) (= j (1- (length line))))
                                 nil)
                                ((or (before input item i j) (after input item i j)
                                     (above input item i j) (below input item i j))
                                 (setq visible-trees  (1+ visible-trees))))))
          )
    visible-trees))

;; 30373
;; 25512
;; 65332
;; 33549
;; 35390
