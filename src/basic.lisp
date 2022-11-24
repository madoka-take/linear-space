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

(defun sweep-out (matrix idx)
  (let ((subtractor (make1 (nth idx matrix) idx)))
    (imapcar #'(if (= a0 idx)
                 subtractor
                 (sweep a1 subtractor idx))
             matrix)))

(defun sweep (target subtractor idx)
  (let ((c (* (nth idx target)
              (/ -1 (nth idx subtractor)))))
    (mapcar #'(+ (car a0) (* c (cdr a0)))
            (zip target subtractor))))

(defun make1 (target idx)
    (let ((c (/ 1 (nth idx target))))
      (mapcar #'(* _ c) target)))

(export
  (defun print-matrix (matrix &optional column-headline
                              (strm *standard-output*))
    (let-it-be matrix
      (format strm "~&~@[|~{~4A~}|~]" column-headline)
      (format strm "~&~{|~{~4A~}|~&~}" it))))

;; Matrix is a list of some horizontal vectors. A horizontal vector is
;; represented as a list. For each `i` from 0 to `n`, the `i`th
;; element of `i` the row must be non-zero. This solver does not tweak
;; the input matrix.
(export
  (defun solve (matrix)
    (do* ((i 0 (1+ i))
          (matrix (sweep-out matrix i) (sweep-out matrix i)))
      ((= i (1- (length matrix))) matrix)
      (terpri)
      (print-matrix matrix))))
