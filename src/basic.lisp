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
  (defun row-canonicalize (matrix)
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

(export
  (defun zero-vector-p (lst)
    (every #'zerop lst)))

(export
  (defun zero-vector (len)
    (make-list len :initial-element 0)))

(defun search-boundary (row start-from)
  (or (position 1 row :start start-from)
      (length row)))
(defun list-free-variables (canonicalized-matrix)
  (let-it-be nil
    (do-tuples/o (end start)
                 (cons (length (car canonicalized-matrix))
                       (reduce (lambda (acc item)
                                 (cons (search-boundary item (1+ (car acc)))
                                       acc))
                               canonicalized-matrix :initial-value '(-1)))
                 (setf it
                       (nconc (iota (- end (1+ start)) :start (1+ start))
                              it)))))

(defun list-free-variables-2 (canonicalized-matrix)
  (sort (nset-difference
          (iota (length (car canonicalized-matrix)))
          (reduce (lambda (acc row)
                    (cons (search-boundary row (1+ (car acc))) acc))
                  (remove-if #'zero-vector-p canonicalized-matrix)
                  :initial-value '(-1)))
        #'<))

;;; TODO: In the usage of the structure it returns, arrays might be a
;;; suitable choice because the randam access could be convenient.
(defun variable-type-list (canonicalized-matrix)
  (let ((solution-dimension (length (car canonicalized-matrix))))
    (labels ((rec (n acc dependent-idx equations)
               (cond ((= n solution-dimension) (nreverse acc))
                     ((null dependent-idx)
                      (nreconc acc (make-list (- solution-dimension n))))
                     ((= n dependent-idx)
                      (rec (1+ n) (cons t acc)
                           (search-boundary (car equations) (1+ n))
                           (cdr equations)))
                     (t (rec (1+ n) (cons nil acc) dependent-idx equations)))))

      (cdr (rec -1 nil -1 (remove-if #'zero-vector-p canonicalized-matrix))))))

(defun negative-unit-vector (dim i)
  (let-it-be (make-list dim :initial-element 0)
    (setf (elt it i) -1)))

(export
  (defun solution-matrix (canonicalized-matrix)
    (let ((solution-dimension (length (car canonicalized-matrix)))
          (free-indices (list-free-variables canonicalized-matrix)))
      (labels ((rec (n acc equations free-indices)
                 (cond ((null equations)
                        (nreconc acc
                                 (mapcar #'(negative-unit-vector solution-dimension _)
                                         free-indices)))
                       ((null free-indices) (nreconc acc equations))
                       ((= n (car free-indices))
                        (rec (1+ n)
                             (cons (negative-unit-vector solution-dimension n)
                                   acc)
                             equations
                             (cdr free-indices)))
                       (t (rec (1+ n) (cons (car equations) acc)
                               (cdr equations) free-indices)))))
        (values
          (rec 0 ()
            (remove-if #'zero-vector-p canonicalized-matrix)
            free-indices)
          free-indices)))))

(export
  (defun solution-basis (canonicalized-matrix)
    (mbind (matrix free-indices) (solution-matrix canonicalized-matrix)
      (mapcar (lambda (free-idx)
                (mapcar #'(nth free-idx _) matrix))
              free-indices))))

(export
  (defun identity-matrix (n)
    (loop :for i := 1 :then (1+ i)
          :until (< n i)
          :collect (nconc (make-list (1- i) :initial-element 0)
                          (list 1)
                          (make-list (- n i) :initial-element 0)))))

(export
  (defun c*matrix (c mat)
    "Scalar multiplation of the given matrix MAT is returned. C must be
     a scalar."
    (MAPCAR #'(c*lst c _) mat)))

(defun mat+mat (A B)
  (mapcar #'(mapcar #'+ _ _) A B))

(export
  (defun add-matrices (A &rest matrices)
    (reduce #'mat+mat matrices :initial-value A)))

(defun subtract-matrix (A B)
  "A - B is returned. A is the minuend and B is the subtrahend."
  (mat+mat A (c*matrix -1 B)))

(defun mat*mat (A B)
  (let ((trans-B (transpose B)))
    (mapcar (lambda (a-)
              (mapcar #'(inner-product a- _) trans-B))
            A)))

;;; We cannot offer zero-argument version because we cannot decide the
;;; size of identity matrix it returns.
(export
  (defun multiply-matrices (A &rest matrices)
    (reduce #'mat*mat matrices :initial-value A)))

(export
  (defun exponentiate-matrix (base exponent)
    "It returns BASE power of EXPONENT. BASE must be a square matrix,
     and EXPONENT must be a natural number."
    (let-it-be (identity-matrix (length base))
      (dotimes (i exponent)
        (setf it (mat*mat it base))))))
