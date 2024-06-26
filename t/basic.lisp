(in-package :basic.linear-space)

(let ((matrix (list (iota 5)
                    '(0 0 0 0 0)
                    '(0 0 0 0 0)
                    '(0 0 0 0 0)
                    (iota 5 :start 5)
                    (iota 5 :start 10)
                    (iota 5 :start 15)
                    )))
  (and
    (equal (print-matrix (row-canonicalize '((0 0 3)
                                             (2 1 4)
                                             (3 4 67))))
           '((1 0 0) (0 1 0) (0 0 1)))
    (equal (print-matrix (row-canonicalize '((3 4 67)
                                             (2 1 4)
                                             (0 0 3))))
           '((1 0 0) (0 1 0) (0 0 1)))
    (equal (print-matrix (row-canonicalize '((0 0 3)
                                             (2 1 4)
                                             (0 0 3))))
           '((1 1/2 0) (0 0 1) (0 0 0)))
    (equal (print-matrix (row-canonicalize matrix))
           '((1 0 -1 -2 -3)
             (0 1 2 3 4)
             (0 0 0 0 0)
             (0 0 0 0 0)
             (0 0 0 0 0)
             (0 0 0 0 0)
             (0 0 0 0 0)))))

(let ((p1 '(1 1 1 1 0 0 0))
      (p2 '(1 1 -1 -1 0 0 0))
      (p3 '(0 0 1 2 2 1 3))
      (q1 '(1 1 0 0 0 0 1))
      (q2 '(0 1 1 0 0 0 1))
      (q3 '(0 0 1 1 0 0 1))
      (q4 '(0 0 0 1 1 0 1))
      (q5 '(0 0 0 0 1 1 1)))
  (and (equal (print-matrix (row-canonicalize (list p1 p2 p3)))
         '((1 1 0 0 0 0 0)
           (0 0 1 0 -2 -1 -3)
           (0 0 0 1 2 1 3)))
       (equal (print-matrix (row-canonicalize (list q1 q2 q3 q4 q5)))
              '((1 0 0 0 0 1 1)
                (0 1 0 0 0 -1 0)
                (0 0 1 0 0 1 1)
                (0 0 0 1 0 -1 0)
                (0 0 0 0 1 1 1)))))

(print-matrix
  `((-1 0 1 0 0 0)
    (0 4 0 1 0 0)
    (2 0 0 0 1 0)
    (0 3 0 0 0 1)
    (-2 0 1 1 0 0)
    (2 1 0 0 1 0)
    (-2 0 -1 0 0 1))
  :row-headline '(p1 p2 p3 q1 q2 q3 q4))

(equal (list-free-variables '((0 1 2 0 0 0 0 -3 0 0 0)
                              (0 0 0 0 1 0 0 1 0 0 0)
                              (0 0 0 0 0 1 0 0 0 0 0)
                              (0 0 0 0 0 0 1 2/7 0 0 0)
                              (0 0 0 0 0 0 0 0 1 0 4)
                              (0 0 0 0 0 0 0 0 0 1 0)
                              (0 0 0 0 0 0 0 0 0 0 0)))
       '(0 2 3 7 10))

(equal (list-free-variables '((0 1 0 0 0)))
       '(0 2 3 4))

(equal (list-free-variables '((1 2 0 0)
                              (0 0 1 0)
                              (0 0 0 1)))
       (1))

(equal (variable-type-list '((1 2 0 0)
                             (0 0 1 0)
                             (0 0 0 1)))
       '(t nil t t))

(solution-basis  '((0 1 2 0 0 0 0 -3 0 0 0 0)
                   (0 0 0 0 1 0 0 1 0 0 0 0)
                   (0 0 0 0 0 1 0 0 0 0 0 0)
                   (0 0 0 0 0 0 1 2/7 0 0 0 0)
                   (0 0 0 0 0 0 0 0 1 0 0 4)
                   (0 0 0 0 0 0 0 0 0 0 1 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0)))

(and (matrix-equal '((1 2 3) (4 5 6) (7 8 9))
                   `(,(iota 3 :start 1) ,(iota 3 :start 4) ,(iota 3 :start 7)))
     (matrix-equal '((0)) '((0))))
