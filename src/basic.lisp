(in-package :basic.linear-space)

(defun sweep (target subtractor idx)
  (let ((c (* (nth idx target)
              (/ -1 (nth idx subtractor)))))
    (mapcar #'(+ (car a0) (* c (cdr a0)))
            (zip target subtractor))))

(defun make1 (target idx)
  (let ((c (/ 1 (nth idx target))))
    (mapcar #'(* _ c) target)))

;; SBS stands for Side by Side. I cannot tell which of "SBS-MAPCAR" and
;; "MAPCAR-SBS" is better. The name may be better to say that it
;; requires 2 lists arguments.
(defun mapcar-sbs (fn lst1 lst2)
  "MAPCAR Side By Side"
  (with-oneish ((fn x y))
    (labels ((rec (lst1 lst2)
               (unless (or (null lst1) (null lst2))
                 (cons (fn (car lst1) (car lst2))
                       (rec (cdr lst1) (cdr lst2))))))
      (rec lst1 lst2))))

(defun lst+ (&rest lsts)
  (if lsts
    (reduce #'(mapcar-sbs #'+ _ _) lsts
            )
    nil))

(defun c*lst (c lst)
  (mapcar #'(* c _) lst))

(defun lst- (x y)
  (lst+ x (c*lst -1 y)))

(defun lst/c (lst c)
  (c*lst (/ 1 c) lst))

(defun inner-product (x y)
  (apply #'+ (mapcar-sbs #'* x y)))

(defun sweep-out (matrix idx)
  (let ((subtractor (make1 (nth idx matrix) idx)))
    (imapcar #'(if (= a0 idx)
                 subtractor
                 (sweep a1 subtractor idx))
             matrix)))


(defun print-matrix (matrix &optional (strm *standard-output*))
  (let-it-be matrix
    (format strm "~{|~{~4A~}|~&~}" it)))

;; Matrix is a list of some horizontal vectors. A horizontal vector is
;; represented as a list. For each `i` from 0 to `n`, the `i`th
;; element of `i` the row must be non-zero. This solver does not tweak
;; the input matrix.
(defun solve (matrix)
  (do* ((i 0 (1+ i))
        (matrix (sweep-out matrix i) (sweep-out matrix i)))
    ((= i (1- (length matrix))) matrix)
    (terpri)
    (print-matrix matrix)))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))
