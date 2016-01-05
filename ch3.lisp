
;; 3.1
;; nil '() is list
(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))

;; 3.2
(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

;; 3.4
(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

;; append: concatenation lists
(append '(a b) '(c d) 'e) ;; (A B C D . E)
(append '(a b) '(c d) '(e)) ;; (A B C D E)

;; 3.5 游程编码 run-length encoding
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun compress (lst)
  (if (consp lst)
      (compr (car lst) 1 (cdr lst))
      lst))

;; uncompress
(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

;; 3.6 Access: nth and nthcdr (zero-indexed)
(nth 0 '(a b c))     ;; A
(nthcdr 1 '(a b c))  ;; (B C)

(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

;; function last： Get the last cons, not the last element
(last '(a b c))      ;; (C)

;; 3.7 Mapping Functions

;; The most frequently used is mapcar, which takes a function and
;; one or more lists, and returns the result of applying the function
;; to elements taken from each list, until some list runs out:
(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))   ;; => (11 12 13)

(mapcar #'list
        '(a b c)
        '(1 2 3 4)) ;; => ((A 1) (B 2) (C 3))

;; The related maplist takes the same arguments, but calls the function
;; on successive cdrs of the lists:
(maplist #'(lambda (x) x)
         '(a b c))  ;; => ((A B C) (B C) (C))
;; Othe mapping functions include mapc, which is discussed on page 88,
;; and mapcan, which is discussed on page 202.

;; 3.8 Trees

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))
