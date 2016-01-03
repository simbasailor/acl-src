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

