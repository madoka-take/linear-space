(in-package :utility.linear-space)

;; SBS stands for Side by Side. I cannot tell which of "SBS-MAPCAR" and
;; "MAPCAR-SBS" is better. The name may be better to say that it
;; requires 2 lists arguments.
(export
  (defun mapcar-sbs (fn lst1 lst2)
    "MAPCAR Side By Side"
    (with-oneish ((fn x y))
      (labels ((rec (lst1 lst2)
                 (unless (or (null lst1) (null lst2))
                   (cons (fn (car lst1) (car lst2))
                         (rec (cdr lst1) (cdr lst2))))))
        (rec lst1 lst2)))))

