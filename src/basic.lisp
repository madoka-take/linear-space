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

(defun sweep-out (matrix idx)
  (let ((subtractor (make1 (nth idx matrix) idx)))
    (imapcar #'(if (= a0 idx)
                 subtractor
                 (sweep a1 subtractor idx))
             matrix)))

(export
  (defun print-matrix (matrix &optional column-headline
                              (strm *standard-output*))
    (let-it-be matrix
      (format strm "~&~@[|~{~4A~}|~]" column-headline)
      (format strm "~&~{|~{~4A~}|~&~}" it))))

(defun element (matrix row column)
  (nth column (nth row matrix)))

(export
  (defun swap-row (matrix m n)
    "Swap `m`th row and `n`th row of the given matrix MATRIX. The given
     MATRIX is NOT modified."
    (let-it-be (copy-tree matrix)
      (setf (nth m it) (nth n matrix)
            (nth n it) (nth m matrix)))))

(defun get-subtractor-row (matrix i)
  (do ((n i (1+ n)))
    ((not (zerop (element matrix n i)))
     (values (nth n matrix) n))))

(defun prepare-pivot (matrix i)
  (mbind (_ row-number) (get-subtractor-row matrix i)
    (if (= row-number i)
      (values matrix nil)
      (values (swap-row matrix i row-number)
              (cons i row-number)))))

;; Matrix is a list of some horizontal vectors. A horizontal vector is
;; represented as a list. For each `i` from 0 to `n`, the `i`th
;; element of `i` the row must be non-zero. This solver does not tweak
;; the input matrix.
(export
  (defun solve (matrix)
    (do* ((i 0 (1+ i))
          (matrix
            (sweep-out (prepare-pivot matrix 0) 0)
            (sweep-out (prepare-pivot matrix i) i)))
      ((= i (1- (length matrix))) matrix))))
