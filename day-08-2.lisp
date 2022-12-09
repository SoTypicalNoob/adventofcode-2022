(defun input-to-list ()
  (with-open-file (stream "input-08")
    (loop :for line = (read-line stream nil nil)
           :while line
          :collect (coerce line 'list))))

(defun before (forest tree i j)
  (let ((eval 0)
        (blocked nil))
    (loop :for cnt :from (1- j) :downto 0
          :do (cond ((and  (> (digit-char-p tree) (digit-char-p (nth cnt (nth i forest)))) (not blocked))
                     (setq eval (1+ eval)))
                    ((and  (<= (digit-char-p tree) (digit-char-p (nth cnt (nth i forest)))) (not blocked))
                     (setq eval (1+ eval))
                     (setq blocked "True"))
                    (t (setq blocked "True"))))
    eval))

(defun after (forest tree i j)
  (let ((eval 0)
        (blocked nil))
    (loop :for cnt :from (1+ j) :to (1- (length (car forest)))
          :do (cond ((and  (> (digit-char-p tree) (digit-char-p (nth cnt (nth i forest)))) (not blocked))
                     (setq eval (1+ eval)))
                    ((and  (<= (digit-char-p tree) (digit-char-p (nth cnt (nth i forest)))) (not blocked))
                     (setq eval (1+ eval))
                     (setq blocked "True"))
                    (t (setq blocked "True"))))
    eval))

(defun above (forest tree i j)
  (let ((eval 0)
        (blocked nil))
    (loop :for cnt :from (1- i) :downto 0
          :do (cond ((and  (> (digit-char-p tree) (digit-char-p (nth j (nth cnt forest)))) (not blocked))
                     (setq eval (1+ eval)))
                    ((and  (<= (digit-char-p tree) (digit-char-p (nth j (nth cnt forest)))) (not blocked))
                     (setq eval (1+ eval))
                     (setq blocked "True"))
                    (t (setq blocked "True"))))
    eval))

(defun below (forest tree i j)
  (let ((eval 0)
        (blocked nil))
    (loop :for cnt :from (1+ i) :to (1- (length forest))
          :do (cond ((and  (> (digit-char-p tree) (digit-char-p (nth j (nth cnt forest)))) (not blocked))
                     (setq eval (1+ eval)))
                    ((and  (<= (digit-char-p tree) (digit-char-p (nth j (nth cnt forest)))) (not blocked))
                     (setq eval (1+ eval))
                     (setq blocked "True"))
                    (t (setq blocked "True"))))
    eval))

(defun day-8-2 (input)
  (let ((sceneric-score 0)
        (tree-score 0))
    (loop :for line :in input
          :for i :from 0
          :do (loop :for item :in line
                    :for j :from 0
                    :do (progn
                          (setq tree-score (* (before input item i j)
                                              (after input item i j)
                                              (above input item i j)
                                              (below input item i j)))
                          (if (< sceneric-score tree-score)
                              (setq sceneric-score tree-score))))
          )
    sceneric-score))

;; 30373
;; 25512
;; 65332
;; 33549
;; 35390
