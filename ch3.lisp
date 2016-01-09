
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

;; For example, suppose we have a list like
;; (and (integerp x) (zerop (mod x 2)))
;; and we want to substitute y for x throughout.It won't do to call substitute,
;; which replaces elements in a sequence:
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; => (AND (INTEGERP X) (ZEROP (MOD X 2)))
;; This call has no effect because the list has three elements,
;; and none of them are x. What we need here is subst,
;; which replaces elements in a tree:
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; =>  (AND (INTEGERP Y) (ZEROP (MOD Y 2)))
;; If we define a version of subst, it comes out looking a lot like copy-tree:
(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if ((atom tree)
	   tree
	   (cons (our-subst new old (car tree))
		 (our-subst new old (cdr tree)))))))
;; Functions the operate on trees ususlly have this form, recursing down
;; both the car and cdr. Such dunctions are said to be doubly recursive.

;; 3.9 Understanding Recursion
;; For example, here is a recursive function for finding the length of a list:
(defun our-len (lst)
  (if (null lst)
      0  ;; 1. That is works for lists of length 0
      (+ (len (cdr lst)) 1))) ;; 2. Given that is works for lists of length n,
                              ;; that it also works for lists of length n+1

;; 3.10 Sets

(member 'b '(a b c))                   ;; => (B C)
(member '(a) '((a) (z)))               ;; => nil
;; a keyword argument :test, that function will be used to test for eqality
;; instead of eql.So if we want to find a member of a list that is equal to
;; a given object, we might say:
(member '(a) '((a) (z)) :test #'equal) ;; => ((A) (Z))
;; The other keyword argument accepted by member is a :key argument.
;; By providing this argument you can specify a function to be applied to each
;; element before comparison:
(member 'a '((a b) (c d)) :key #'car)  ;; => ((A B) (C D))
;; If we wanted to give both keyword arguments, we could give them in
;; either order. The following two calls ar equivalent:
(member 2 '((1) (2)) :key #'car :test #'equal) ;; => ((2))
(member 2 '((1) (2)) :test #'equal :key #'car) ;; => ((2))

;; If we want to find an element satisfying an arbitrary predicate -- like oddp,
;; which returns true for odd integers -- we can use the related member-if:
(member-if #'oddp '(2 3 4)) ;; => (3 4)
;; We could imagine a limited version of member-if being written:
(defun our-member-if (fn lst)
  (and (consp lst)
	(if (funcall fn (car lst))
	    lst
	    (our-member-if fn (cdr lst)))))

;; The function adjoin is like a conditional cons.
;; It takes an object and a list, and consed the object onto the list
;; only if it is not already a member:
(adjoin 'b (a b c))        ;; => (A B C)
(adjoin 'z '(a b c))       ;; => (Z A B C)
;; In the general case it takes the same keyword arguments as member.

;; The operations of set union, intersection, and complement are implemented
;; by the functions union, intersection, and set-difference.These function
;; expect exactly two lists (but also take the same keyword arguments as member)
(union '(a b c) '(c b s))   ;; => (A C B S)
(intersection '(a b c) '(b b c))   ;; => (B C)
(set-difference '(a b c d e) '(b e))   ;; => (A C D)

;; 3.11 Sequences

;; The function length returns the number of elements in a sequence:
(length '(a b c))    ;; => 3

;; To copy part of a sequence, we use subseq.
;; The second argument (required) is the position of the first element to
;; be included, and the third argument(optional) is the position of
;; the first elment no to be included.
(subseq '(a b c d) 1 2)      ;; (B)

;; I the third argument is omitted, the subsequence goes all the way to
;; the end of the original sequence.
(subseq '(a b c d) 1 2)      ;; (B C D)

;; The function reverse returns a sequence with the same elments
;; as its argument, but in the reverse order:
(reverse '(a b c))            ;; (C B A)

;; A palindrome is a sequence that reads the same in either drection.
;; Using length, subseq, and reverse, we can dfine a funtion:
(defun mirror? (s)
  (let ((len (length s)))
    (or (and (evenp len)
	     (let ((mid (/ len 2)))
	       (equal (subseq s 0 mid)
		      (reverse (subseq s mid)))))
	(and (oddp len)
	     (let ((mid (/ (+ len 1) 2)))
	       (equal (subseq s 0 (- mid 1))
		      (reverse (subseq s mid))))))))

;; Common lisp has a built-in sort function called sort.
;; It taks a sequence and a comparison function of two argments,
;; and returns a sequence with the same elements, sored according
;; to the function:
(sort '(0 2 1 3 8) #'>)
;; We have to be careful when using sort, bacause it's destructive.

;; Using sort and nth, we can write a function that takes an integer n,
;; and returns the nth greatest element of a list:
(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

;; The functions every and some take a predicate and on or more sequence.
;; When given just on sequence, they test whether the elements satisfy
;; the predicate:
(every #'oddp '(1 3 5))      ;; => T
(some #'evenp '(1 2 3))      ;; => T

;; If they are given more than on sequence, the predicate must take as
;; many arguments as there are sequences, and arguments are drawn on at
;; a time from all the sequences:
(every #'> '(1 3 5) '(0 2 4))  ;; => T
;; If the sequences are of different lengths, the shortst on determines the
;; number of tests performed.

;; 3.12 Stacks
;; (push obj lst) =is equivalent to=> (setf lst (cons obj lst))
;; (pop lst) =is equivalent to=> (let ((x (car lst))) (setf lst (cdr lst)) x)
(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

