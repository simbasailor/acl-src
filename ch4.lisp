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

(defun mirrors? (s)
  (let ((len (length s)))
    (or (and (evenp len)
	     (do ((forward 0 (+ forward 1))
		  (back (- len 1) (- back 1)))
		 ((or (> forward back)
		      (not (eql (elt s forward)
				(elt s back))))
		  (> forward back))))
	(and (oddp len)
	     (do ((forward 0 (+ forward 1))
		  (back (- len 1) (- back 1)))
		 ((or (> (+ forward 1) back)
		      (not (eql (elt s forward)
				(elt s back))))
		  (> (+ forward 1) back)))))))
;; This version would work with lists too, but its implementation is better
;; suited to vectors. The frequent calls to elt would be expensive with lists,
;; because lists only allow sequential access. In vectors, which allow random
;; access, it is as cheap to reach on element as any other.
;; Many sequence functions take on or more keyword arguments form the standard
;; set listed in this table:
;; | FARAMETER  | PURPOSE                              | DEFAULT
;; | :key       | a function to apply to each element  | identity
;; | :test      | the test function for comparison     | eql
;; | :from-end  | if true, work backwards              | nil
;; | :start     | position at which to start           | 0
;; | :end       | position, if any, at which to stop   | nil

;; One function that takes the full set is position, which returns the position
;; of an element in a sequence, or nil if it is not found. We'll use position to
;; illustrate the roles of the keyword arguments.
(position #\a "funtasia")
(position #\a "funtasia" :start 5)
(position #\a "funtasia" :from-end t)
(position 'a '((c d) (b a) (a b)) :key #'car)
(position '(a b) '((a b) (c d)) :test #'equal)

;; Using sebseq and position, we can write functions that take sequences apart.
;; For example, this function:
(defun second-word (str)
  (let ((pl (+ (position #\  str) 1)))
    (subseq str pl (position #\  str :start pl))))
;; returns the second workd in a string of words seqarated by spaces
(second-word "This is a work.")

;; To find an element satisfying a predicate of on argument, we use
;; position-if.It takes a function and a sequence, and returns the
;; position of the firest element satisfying the function:
(position-if #'oddp '(2 3 4 5))
;; It takes all the keyword arguments except :test.

;; There are functions similar to member and member-if for sequences.
;; They are, resepectively, find (which takes all the keyword arguments)
;; and find-if (which takes all except :test)
(find #\a "cat")
(find-if #'characterp "ham" :start 1)
;; Unlike member and member-if, they return only the object they were looking for.
;; Often a call to find-if will be clearer if it is translated into a find
;; with a :key argument. for example, the expression
(find-if #'(lambda (x)
	     (eql (car x) 'complete))
	 lst)
;; would be better rendered as
(find 'complete lst :key #'car)

;; The functions remove and remove-if both work on sequence generally.
;; They bear the the same relation to one another as find and find-if.
;; A related function is remove-duplicates, which preserves only the
;; last of each occurrence of any element of a sequence.
(remove-duplicates "abracadabra")

;; The function reduce is for boiling down a sequence into a single value.
;; It takes at least two arguments, a function and a sequence. The function
;; must be a function of two arguments. In the simplest case, it will be
;; called initially with the first two elements, and thereafter with successive
;; elements as the second argument, and the value it returned last time as
;; the first. The value returned by the last call is returned as the value of
;; the reduce. Which means that an expression like
;; (reduce #'fn '(a b c d))
;; is equivalent to
;; (fn (fn (fn 'a 'b) 'c) 'd)
;; We can use reduce to extend functions that only take two arguments. For
;; example, to get the intersection of three or more lists, we could write
;; something like
(reduce #'intersection '((b r a d 's) (b a d) (c a t)))

;; 4.5 Example: Parsing Dates
;; As an example of operations on sequences, theis section shows how to wirte
;; a program to parse dates. We will write a program that can take a string
;; like "16 Aug 1980" and return a list of integers representing the day, month,
;; and year.
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(tokens "ab12 3cde.f" #'alpha-char-p 0)
(tokens "ab12 3cde.f gh" #'constituent 0)

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
	  (parse-month (second toks))
	  (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
	   :test #'string-equal)))
    (if p
	(+ p 1)
	nil)))

(defun read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
	(dotimes (pos (length str))
	  (setf accum (+ (* accum 10)
			 (digit-char-p (char str pos)))))
	accum)
      nil))

;; 4.6 Structures
;; A structure can be considered as a deluxe kind of vector. Suppose you
;; had to write a program that kept track of a number of rectangular solids.
;; You might consider representing them as vectors of three elment: heiht,
;; width, and depth. Your program would be easier to read if, instead of
;; using raw sevefs, you defined function like:
;; (defun block-height (b) (svref b 0))
;; and so on. You can think of a structure as a vector in which all these
;; kinds of functions get defined for you.
;; To define a structure, we use defstruct. In the simplest case we just
;; give the name of the structure and the names of the fields:
(defstruct point
  x
  y)
;; This defines a point to be a structure with two fields, x and y. It also
;; implicitly defines the functions make-point, point-p copy-point, point-x
;; and point-y.
;; Section 2.3 mentioned that Lisp program could write Lisp program.This is
;; one of the most conspicuous examples we have seen so far. When you call
;; defstruct, it automatically writes code defining several other functions.
;; With macros you will be able to do the same thing yourself. (You could
;; even write defstruct if you had to.)
;; Each call to make-point will return a new point. We can specify the values
;; of individual fields by giving the corresponding keyword arguments:
(setf p (make-point :x 3 :y 4))
(point-x p)
(point-y p)
(setf (point-x p) 2)
(point-p p)
(typep p 'point)
;; We can also use general-purpose functions like typep.

;; We can specify default values for structure fields by enclosing the field
;; name and a default expression in a list in the original definition.
(defstruct polemic
  (type (progn
	  (format t "What kind of polemic was it?")
	  (read)))
  (effect nil))
(make-polemic)

(defstruct (point (:conc-name p)
		  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))
;; The :conc-name argument specifies what should be concatenated to the front
;; of the field names to make access functions for them. By default it was
;; point-; now it will be simply p. Not using the default makes your code a
;; little less readable, so you would only want to do this kind of thing if
;; you're going to be using the access functions constantly.
;; The :print-functions is the name of the function that should be used to
;; print a point when it has to be displayed --e.g. by the toplevel. This
;; function must take three arguments: the structure to be printed, the
;; place where it is to be printed, and a third argument that can usually
;; be ignored. We will deal with streams in Section 7.1.
(make-point)
;; The function print-point will display points in an abbreviated form.

