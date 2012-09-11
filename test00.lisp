;;;;
;;;;
;;;; lisp test
;;;;|
;;;;

(defun threat (i j a b)
  (or (= i a)
      (= j b)
      (= (- i j) (- a b))
      (= (+ i j) (+ a b))))


(defun conflict (n m board)
  (cond 
   ((endp board) nil)
   ((threat n m (first (first board)) (second (first board))) t)
   (t (conflict n m (rest board)))))


(defun print-hz-bd (board)
  (format t "~%*~A*"
    (make-string (1+ (* 2 (length board))) :initial-element #\-)))


(defun print-board (board)
  (print-hz-bd board)
  (dolist (queen-coordinates board)
    (format t "~%|")
    (dotimes (column (length board))
      (if (= column (second queen-coordinates))
        (format t " Q")
        (format t " .")))
      (format t " |"))
    (print-hz-bd board))


(defun queen (size &optional (board nil) (n 0) (m 0))
  (unless (= m size)
    (unless (conflict n m board)
      (if (= (+ 1 n) size)
        (print-board (reverse (cons (list n m) board)))
        (queen size (cons (list n m) board) (+ 1 n) 0)))
  (queen size board n (+ 1 m))))


(queen 8)

