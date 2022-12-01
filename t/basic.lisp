(in-package :basic.linear-space)

(let ((matrix (list (iota 5)
                    '(0 0 0 0 0)
                    '(0 0 0 0 0)
                    '(0 0 0 0 0)
                    (iota 5 :start 5)
                    (iota 5 :start 10)
                    (iota 5 :start 15)
                    )))
  (print-matrix (solve '((0 0 3)
                         (2 1 4)
                         (3 4 67))))
  (print-matrix (solve '((3 4 67)
                         (2 1 4)
                         (0 0 3))))
  (print-matrix (solve '((0 0 3)
                         (2 1 4)
                         (0 0 3))))
  (print-matrix (solve matrix)))

(let ((p1 '(1 1 1 1 0 0 0))
      (p2 '(1 1 -1 -1 0 0 0))
      (p3 '(0 0 1 2 2 1 3)))
  (print-matrix (solve (list p1 p2 p3))))