;; 4 Specialized Data Structures
(setf arr (make-array '(3 3) :initial-element nil))
(aref arr 1 1)
(setf (aref arr 1) 20)

;; If you want just a one-dimensional array, you can give an
;; integer instead of a list as the first argument to make-array
(setf vec (make-array 4 :initial-element nil))
;; A one-dimensional array is also called a vector. You can create
;; and fill one in a single step by calling vector, which will return
;; a vector of whatever arguments you give it:
(vector "a" 'b 3)
(aref vec 0)
;; You can use aref for vector access, but there is a faster function
;; called svref(sv, simple vector) for use with vectors.
(svref vec 0)

;; 4.2 Binary Search

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start))
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (aref vec mid)))
	    (if (< obj obj2)
		(finder obj vec start (- mid 1))
		(if (> obj obj2)
		    (finder obj vec (+ mid 1) end)
		    obj)))))))

(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))

;; 4.3 Strings and Characters
;; Strings are vectors of characters. We denote a constant string as
;; a series of characters surrounded by double-quotes, and a individual
;; character c as #\c.
;; Each character has an associated integer -- usually, but not necessarily,
;; the ASCII number. In most implementations, the function char-code returns
;; the number associated with a character, and code-char returns the character
;; associated with a number.
;; The function char< (less than), char<= (less than or equal),
;; char= (equal), char>= (greater than or equal) char> (greater then),
;; and char/= (different) compare characters.
(sort "elbow" #'char<)
(aref "abc" 1)
;; but with a string you can use the faster char:
(char "abc" 1)
;; You can use setf with char (or aref) to replace elements:
(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

;; If you want to compare two strings, you can use the general equal,
;; but there is also a function string-equal that ignores case:
(equal "fred" "fred")
(equal "fred" "Fred")
(string-equal "fred" "Fred")
;; Common lisp provides a large number of functions for comparing and
;; manipulating strings.
;; There are several ways of building strings.
;; The most general is to use format. Calling format with nil as the first
;; argument makes it return as a string what it would have printed:
(format nil "~A or ~A" "truth" "dare")

;; 4.4 Sequences
;; But if you just want to join several strings together, you can use
;; concatenat, which takes a symbol indicting the type of the result,
;; plus on or more sequences:
(concatenate 'string "not " "to worry")


