;; chapter 2

(defun our-member (obj lst)
  (if (null lst)
      nil)
      (if (eql (car lst) obj)
	  lst
	  (if (listp (our-member obj (cdr lst)))
	      lst)))

(defun ask-number ()
  (format t "Please enter a number:")
  (let ((num (read)))  ;; force on brakets, used two 
    (if (numberp num)
	num
	(ask-number))))

(defparameter *GLOB* 99)
(defconstant limit (+ *GLOB* 1))

;; if you want to check whether symbol is the name of
;; a global variable or constant, use boundp
(boundp '*glob*)

;; 2.13 iteration
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

;; a recursive version of show-squares
(defun show-squares (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

;; a recursive version of our-length
(defun our-lenght (lst)
  (if (null lst)
      0
      (+ (our-lenght (cdr lst)) 1)))

;; a tail-recursive version of our-lenght
(defun our-lenght (lst n)
  (if (null lst)
      n
      (our-lenght (cdr lst) (+ n 1))))

;; 2.14 functions as objects

;; the function apply can be given any number of arguments,
;; so long as the last is a list
(apply #'+ '(1 2 3))
(apply #'+ 1 2 '(3 4 5))

;; The function funcall dose the same thing but does not need
;; the arguments to be packaged in a list
(funcall #'+ 1 2 3)

;; lambda just a symbal, not an operator
(lambda (x y)
  (+ x y))

;; A lambda expression can be considered as the name of a function.
;; an ordinary function name, a lambda expression can be the first element
;; of a function call,
((lambda (x) (+ x 100)) 1)
(funcall #'(lambda (x) (+ x 100)) 1)
(apply #'(lambda (x) (+ x 100)) '(1))

;; chapter2 exercises

;; 7. Using only operator introduced in the chapter,
;; define a function that takes a list as an argumnet
;; and returns true if on of its elements is a list
(defun element-has-list (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  t
	  (element-has-list (cdr lst)))))

;; 8. Give iterative and recursive definitions of a function that
;;  a. takes a positive integer and prints that many dots.
;;  b. takes a list and returns the number of times the symbol a occurs in it.

;; a. A iterative version
(defun prints-dots (n)
  (let ((dot "."))
    (do ((i 1 (+ i 1)))
	((> i n) 'done)
      (format t "~A" dot))))

;; a. A recursive version
(defun prints-dots (n)
  (if (<= n 0)
      'done
      (progn
	(format t ".")
	(prints-dots (- n 1)))))

;; b. a iterative version
(defun count-first-symbol (lst)
  (let ((obj (car lst)) (count 0))
    (dolist (item lst)
      (if (equal obj item)
          (setf count (+ count 1))))
    count))

(defun symbol-occur-times (lst)
  (let ((target-lst nil) (find-lst lst))
    (do ()
        ((null find-lst) target-lst)
      (progn
        (setf target-lst (cons (cons (car find-lst) (count-first-symbol find-lst)) target-lst))
        (setf find-lst (remove (car find-lst) find-lst))))))

;; a recursive version
(defun symbol-occur-times (lst)
  (and
   (consp lst)
   (cons (cons (car lst) (count-first-symbol lst)) (symbol-occur-times (remove (car lst) lst)))))

;; 9. A friend is trying to write a function that returns the sum of
;; all the non-nil elements in a list. He has written two versions of
;; this function, and neither of them work. Explain what's wrong with
;; each,and give a correct version:
;; 9. (a) it can work.
(defun summit (lst)
  (remove nil lst)
  (apply #'+ lst))

;; 9. (b)
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (and
         x ;; (numberp x)
         (+ x (summit (cdr lst)))))))
