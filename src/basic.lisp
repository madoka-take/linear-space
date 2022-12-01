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
  (labels ((rec (n)
             (cond ((<= (length matrix) n) (values nil nil))
                   ((zerop (element matrix n i)) (rec (1+ n)))
                   (t (values (nth n matrix) n)))))
    (rec i)))

;; Matrix is a list of some horizontal vectors. A horizontal vector is
;; represented as a list. For each `i` from 0 to `n`, the `i`th
;; element of `i` the row must be non-zero. This solver does not tweak
;; the input matrix.
(export
  (defun solve (matrix)
    (mvdo (((i) 0 (1+ i))
           ((_ num) (get-subtractor-row matrix 0)
                    (get-subtractor-row matrix (1+ i))))
          ((or (= i (1- (length matrix)))
               (null num))
           (if num
             (sweep-out matrix i)
             matrix))
      (setf matrix (sweep-out (if num
                                (swap-row matrix i num)
                                matrix)
                              i)))))
