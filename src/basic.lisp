(in-package :basic.linear-space)

(export
  (defun lst+ (&rest lsts)
    (if lsts
      (reduce #'(mapcar-sbs #'+ _ _) lsts)
      nil)))

(export
  (defun c*lst (c lst)
    (mapcar #'(* c _) lst)))

(export
  (defun lst- (x y)
    (lst+ x (c*lst -1 y))))

(export
  (defun lst/c (lst c)
    (c*lst (/ 1 c) lst)))

(export
  (defun inner-product (x y)
    (apply #'+ (mapcar-sbs #'* x y))))

(export
  (defun transpose (matrix)
    (apply #'mapcar #'list matrix)))

(defun sweep (target subtractor idx)
  (lst- target
        (c*lst (* (nth idx target)
                  (/ 1 (nth idx subtractor)))
               subtractor)))

(defun make1 (target idx)
    (let ((c (/ 1 (nth idx target))))
      (mapcar #'(* _ c) target)))

(defun sweep-out (matrix row-idx col-idx)
  (let ((subtractor (make1 (nth row-idx matrix) col-idx)))
    (imapcar #'(if (= a0 row-idx)
                 subtractor
                 (sweep a1 subtractor col-idx))
             matrix)))

;;; It seems that this printer has already reached the limit of easy
;;; implementation. The default value of ROW-HEADLINE and
;;; COLUMN-HEADLINE consume too much characters, the format string might
;;; be too complex. What if an element of MATRIX requires more than 4
;;; characters? There are many features a pretty printer might support.
;;; But to offer such sophisticated features, complete reconstruction
;;; should be desireable.
(export
  (defun print-matrix (matrix &key (row-headline (iota (length matrix) :start 1))
                              (column-headline (iota (length (car matrix)) :start 1))
                              (strm *standard-output*))
    (let-it-be matrix
      (let ((row-headline-control
              (format nil "~~~D<~~A~~>"
                      (apply #'max (mapcar #'(length (format nil "~A" _)) row-headline)))))
        (format strm "~&~%~@[~? ~{~4<~A~>~}~]" row-headline-control '("") column-headline)
        (format strm "~&~@[~?|~{----~*~}|~]" row-headline-control '("") column-headline)
        (format strm "~&~:{~?|~{~4<~A~>~}|~&~}"
                (mapcar #'(list row-headline-control (list _) _) row-headline it))))))

(defun element (matrix row column)
  (nth column (nth row matrix)))

(export
  (defun swap-row (matrix m n)
    "Swap `m`th row and `n`th row of the given matrix MATRIX. The given
     MATRIX is NOT modified."
    (let-it-be (copy-tree matrix)
      (setf (nth m it) (nth n matrix)
            (nth n it) (nth m matrix)))))

(defun get-subtractor-row (matrix row-idx col-idx)
  (labels ((rec (n)
             (cond ((<= (length matrix) n) (values nil nil))
                   ((zerop (element matrix n col-idx)) (rec (1+ n)))
                   (t (values (nth n matrix) n)))))
    (rec row-idx)))

;; Matrix is a list of some horizontal vectors. A horizontal vector is
;; represented as a list.
(export
  (defun solve (matrix)
    (do* ((col-idx 0 (1+ col-idx))
          (row-idx 0 (- col-idx (length skipped-columns)))
          (matrix matrix)
          (skipped-columns '()))
      ((or (= row-idx (length matrix))
           (= col-idx (length (car matrix))))
       matrix)
      (mbind (_ num) (get-subtractor-row matrix row-idx col-idx)
        (if num
          (setf matrix (sweep-out (if (= num row-idx)
                                    matrix
                                    (swap-row matrix row-idx num))
                                  row-idx
                                  col-idx))
          (setf skipped-columns (cons col-idx skipped-columns)))))))

(defun zero-vector-p (lst)
  (every #'zerop lst))

(defun zero-vector (len)
  (make-list len :initial-element 0))

;;; The zero-vector appending process is time consuming if the given
;;; canonicalized-matrix is large.
(defun list-free-variables (canonicalized-matrix)
  (labels ((search-boundary (row start-from)
             (or (position 1 row :start start-from)
                 (length row)))
           (calc-free-indices (cur next)
             (iota (- next (1+ cur)) :start (1+ cur))))
    (do* ((matrix canonicalized-matrix (cdr matrix))
           (boundary -1 next)
           row next result)
      ((null matrix)
        (nreverse
          (nreconc (calc-free-indices
                     next (length (car canonicalized-matrix)))
                   result)))
      (setf row (car matrix))
      (setf next (search-boundary row (1+ boundary)))
      (setf result (nreconc (calc-free-indices boundary next) result)))))
