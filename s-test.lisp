;-*- Syntax:COMMON-LISP -*-

;This is the November, 26 1991 version of
;the testing file for Richard C. Waters' Series macro package.

;------------------------------------------------------------------------
;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.
;------------------------------------------------------------------------

;To test Series just load this file and run the function (DO-TESTS).
;It prompts you for the name of a scratch file to use when testing.
;You must type a string "..."  containing the path name.  It then
;prints out identifying numbers of tests as it performs one test after
;another.  When all of the tests have been run a summary line is
;printed saying how many tests failed.  This file includes a copy of
;the RT regression tester.  (See the Documentation for RT in Lisp
;Pointers Volume 4 number 2 1991 for a full description of the
;features of RT.)  The file previously used an older tester.  For easy
;comparison of results, all of the old tests are given numerical names
;that match the numbers printed out when running the old tester.

;;;; $Id: s-test.lisp,v 1.9 1999/07/01 17:18:05 toy Exp $
;;;;
;;;; $Log: s-test.lisp,v $
;;;; Revision 1.9  1999/07/01 17:18:05  toy
;;;; Use the package COMMON-LISP-USER instead of USER.
;;;;
;;;; Revision 1.8  1999/04/29 22:07:43  toy
;;;; Don't break on signals for Allegro either.
;;;;
;;;; Revision 1.7  1999/04/13 15:56:39  toy
;;;; Remove an unused NIL tag from test 418.
;;;;
;;;; Revision 1.6  1999/04/06 21:40:52  toy
;;;; Some extra tests from Arthur Lemmens <lemmens@simplex.nl>.  They make
;;;; sure series does the right things with strings and bit vectors.
;;;;
;;;; Revision 1.5  1998/06/10 18:55:08  toy
;;;; Merged in the T -> TO -> TON macro renaming from Rev 1.1.1.2.
;;;;
;;;; Some of the symbolics-only tests appear to work with CMUCL.
;;;;
;;;; For Allegro, compiler-let is not exported from CLTL1 package.
;;;;
;;;;

(in-package "COMMON-LISP-USER")
(eval-when (load compile)
  (series::install))

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")

(defstruct (entry (:conc-name nil)
		  (:type list))
  pend name form)

(defmacro vals (entry) `(cdddr ,entry))
(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l)) 
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~@:(~S~)"
        (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun do-entry (entry &optional
		 (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   ;; Are these right?  I (RLT) think 'warning is ok for
	   ;; Harlequin, but for CMUCL, it causes some tests to fail
	   ;; that otherwise would pass because the compiler generates
	   ;; a warning.
	   #-(or cmu allegro) (*break-on-warnings* t)
	   #+(or cmu allegro harlequin) (*break-on-signals* #+(or cmu allegro) nil
							    #-(or cmu allegro) 'warning)
	   ;; Don't print out "Compiling..." messages
	   #+cmu
	   (*compile-print* nil)
	   (r (multiple-value-list
		(eval (form entry)))))
      (setf (pend entry)
	    (not (equal r (vals entry))))
      (when (pend entry)
	(format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~
                   ~%Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
		*test* (form entry)
		(length (vals entry))
		(vals entry)
		(length r) r))))
      (when (not (pend entry)) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
		 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file 
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (cdr *entries*)
		 :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (pend entry)
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests)))
    (if (null pending)
	(format s "~&No tests failed.")
	(format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		(length pending)
		(length (cdr *entries*))
		pending))
    (null pending)))

;convenient shorthand
(defun more () (continue-testing))

;This is useful for experimentation
(defvar form nil)
(defmacro r (&optional (f nil))
  (if f (setq form f))
  (setq f (series::iterative-copy-tree form))
  (gensym 1)
  (setq f (macroexpand f))
  (let ((*print-length* nil) (*print-level* nil) #+symbolics(*print-lines* nil))
    (pprint f))
  (cond ((Y-or-N-p "continue") f)))

(defun o ()
  (setq series::*optimize-series-expressions*
	(not series::*optimize-series-expressions*)))

;This is the standard tester.
(defvar test-file nil)
(defvar *compile-tests* T)

(defmacro ton (form) `(test-opt-&-non-opt ',form))

(defun test-opt-&-non-opt (form)
  (loop (if test-file (return nil))
    (format T "~%Type a string representing a pathname of a scratch disk file: ")
    (setq test-file (read))
    (if (not (stringp test-file)) (setq test-file nil)))
  (let* ((non-opt (test-non-opt form))
	 (opt (test-opt form)))
   (if (equal non-opt opt) opt
       (list "opt and non-opt disagree" opt non-opt))))

(defmacro to (form) `(test-opt ',form))

(defun test-opt (form)
  (setq series:*last-series-loop* nil)
  (let ((series::*series-implicit-map* nil)
	(series::*optimize-series-expressions* T))
  (setq form (series::iterative-copy-tree form))
  (if *compile-tests*
      (funcall (compile nil `(lambda () ,form)))
      (eval form))))

(defmacro tn (form) `(test-non-opt ',form))

(defun test-non-opt (form)
  (setq series:*last-series-loop* nil)
  (let ((series::*series-implicit-map* nil)
	(series::*optimize-series-expressions* nil))
    (setq form (series::iterative-copy-tree form))
    (eval form)))

(defmacro td (defun form)
  (let ((series::*series-implicit-map* nil))
    (eval (series::iterative-copy-tree defun))
  #+symbolics(compile (cadr defun)) ;does not work in all lisps
    `(test-opt-&-non-opt ',form)))

(defmacro tw (form) `(test-wrs ',form))

(defun test-wrs (form)
  (let ((v nil)
	(series::*series-implicit-map* nil)
	(series::*optimize-series-expressions* T)
	(series::*testing-errors* T))
    (setq series:*last-series-error* nil)
    (setq series:*last-series-loop* nil)
    (with-output-to-string (*error-output*)
      (setq v (eval (series::iterative-copy-tree form))))
    (values v (cadr series:*last-series-error*))))

(defmacro tr (form) `(test-rrs ',form))

(defun test-rrs (form)
  (let ((v nil)
	(series::*series-implicit-map* nil)
	(series::*testing-errors* T)
	(series::*optimize-series-expressions* T)
	(*suppress-series-warnings* nil))
    (setq series:*last-series-error* nil)
    (setq series:*last-series-loop* nil)
    (with-output-to-string (*error-output*)
      (setq v (eval (series::iterative-copy-tree form))))
    (values v (cadr series:*last-series-error*))))

(defmacro te (form) `(test-ers ',form))

(defun test-ers (form)
  (setq series:*last-series-loop* nil)
  (let* ((series::*series-implicit-map* nil)
	 (series::*testing-errors* T)
	 (opt (catch :testing-errors (test-opt form)))
	 (non-opt (catch :testing-errors (test-non-opt form))))
   (if (equal non-opt opt) opt
       (list "opt and non-opt disagree" opt non-opt))))

(defmacro teo (form) `(test-ers-opt ',form))

(defun test-ers-opt (form)
  (setq series:*last-series-loop* nil)
  (let* ((series::*series-implicit-map* nil)
	 (series::*testing-errors* T))
    (catch :testing-errors (test-opt form))))

(defmacro tm (form) `(test-mapping ',form))

(defun test-mapping (form)
  (setq series:*last-series-loop* nil)
  (let ((series::*series-implicit-map* T)
        (*suppress-series-warnings* nil))
    (setq form (series::iterative-copy-tree form))
    (if *compile-tests*
	(funcall (compile nil `(lambda () ,form)))
	(eval form))))

(defun decls (arg) (declare (ignore arg)) (decls0 series:*last-series-loop*))
(defun decls0 (tree)
  (cond ((not (consp tree)) nil)
	((eq (car tree) 'declare) tree)
	(T (do ((l tree (cdr l))) ((not (consp l)) nil)
	     (let ((x (decls0 (car l))))
	       (if x (return x)))))))

(defun phys-scan-list (l)
  (scan l))

(declaim (special *x*))

(defun incx (&optional (val *x*))
  (incf *x*) val)

(defvar *c1* 1)
(defvar *c2* 2)

(defmacro c1-c2-macro (value)
  `(list ,*c1* ,*c2* ,value))

;The first few pages of tests attempt to test each of the different
;series operations in the series function library.

(rem-all-tests)

;first are individual tests of all the exported series functions
(deftest 0 (ton (collect #Z(a b c))) (a b c))
(deftest 1 (ton (collect #Z())) ())

(deftest 2 (ton (collect (#Mlist (series 'a0) #Z(a b)))) ((a0 a) (a0 b)))
(deftest 3 (ton (collect (#1Mlist (series 'a0 'a1) #Z(a b c)))) ((a0 a) (a1 b) (a0 c)))

(deftest 4 (ton (collect (make-series 'b 'c))) (b c))
(deftest 5 (ton (collect (make-series 'b))) (b))

(deftest 6 (ton (collect (#Mcar (scan-fn T #'(lambda () '(a b c)) #'cdr #'null)))) (a b c))
(deftest 7 (ton (collect (#Mcar (scan-fn '(values T) #'(lambda () '(a b c)) #'cdr #'null))))
  (a b c))
(deftest 8 (ton (collect (#Mcar (scan-fn T #'(lambda () '(a b c))
					 'cdr #'(lambda (x) (null x))))))
  (a b c))
(deftest 9 (ton (collect (#M(lambda (x y) (list x (car y)))
			    #Z(a b c)
			    (scan-fn T #'(lambda () '(1 2)) #'cdr))))
  ((a 1) (b 2) (c nil)))
(deftest 10 (ton (let* ((lst (list 'a 'b 'c)))
		   (multiple-value-bind (e l)
		       (scan-fn '(values T T) #'(lambda () (values (car lst) lst))
				#'(lambda (element parent)
				    (declare (ignore element))
				    (values (cadr parent) (cdr parent)))
				#'(lambda (element parent)
				    (declare (ignore element))
				    (null parent)))
		     (list (collect e) (collect l)))))
  ((a b c) ((a b c) (b c) (c))))

(deftest 11 (ton (collect
		  (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				(scan-fn T #'(lambda () 0)
					 #'(lambda (sum)
					     (incf xx)
					     (+ sum xx))
					 #'(lambda (x) (> x 10)))))) (0 1 3 6 10))
(deftest 12 (ton (multiple-value-bind (a b)
		     (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				   (scan-fn '(values T T)
					    #'(lambda () (values 0 1))
					    #'(lambda (sum prod)
						(incf xx)
						(values (+ sum xx) (* prod xx)))
					    #'(lambda (x y) (> (min x y) 10))))
		   (list (collect a) (collect b))))
  ((0 1 3 6 10) (1 1 2 6 24)))

(deftest 13 (ton (collect (#Mcar (scan-fn-inclusive T #'(lambda () '(a b c)) #'cdr #'null))))
  (a b c nil))
(deftest 14 (ton (collect (#Mcar (scan-fn-inclusive T #'(lambda () ()) #'cdr #'null)))) (nil))
(deftest 15 (ton (let* ((lst (list 1 2 3 -4 5)))
		   (multiple-value-bind (e l)
		       (scan-fn-inclusive '(values T T) #'(lambda () (values (car lst) lst))
					  #'(lambda (element parent)
					      (declare (ignore element))
					      (values (cadr parent) (cdr parent)))
					  #'(lambda (element parent)
					      (declare (ignore parent))
					      (minusp element)))
		     (list (collect e) (collect l)))))
  ((1 2 3 -4) ((1 2 3 -4 5) (2 3 -4 5) (3 -4 5) (-4 5))))

(deftest 16 (ton (collect
		  (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				(scan-fn-inclusive T #'(lambda () 0)
						   #'(lambda (sum)
						       (incf xx)
						       (+ sum xx))
						   #'(lambda (x) (> x 10))))))
  (0 1 3 6 10 15))
(deftest 17 (ton (multiple-value-bind (a b)
		     (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				   (scan-fn-inclusive '(values T T)
						      #'(lambda () (values 0 1))
						      #'(lambda (sum prod)
							  (incf xx)
							  (values (+ sum xx) (* prod xx)))
						      #'(lambda (x y) (> (min x y) 10))))
		   (list (collect a) (collect b))))
  ((0 1 3 6 10 15) (1 1 2 6 24 120)))
(deftest 18 (ton (collect (scan ()))) ())
(deftest 19 (ton (let* ((x (list 'a 'b 'c)) (e (scan 'list x)))
		   (list (collect e) (alter e (scan-range)) (collect e) x)))
  ((a b c) nil (a b c) (0 1 2)))
(deftest 20 (ton (collect (scan 'vector '#()))) ())
(deftest 21 (ton (let* ((v (copy-seq "FOO")) (e (scan 'vector v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(deftest 22 (ton (let* ((v (copy-seq "FOO")) (e (scan 'sequence v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(deftest 23 (ton (let* ((v (copy-seq '(1 2 3))) (e (scan 'sequence v)))
		   (list (collect e) (alter e (series 0)) v)))
  ((1 2 3) nil (0 0 0)))
(deftest 24 (ton (let* ((type 'string) (v (copy-seq "FOO")) (e (scan type v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(deftest 25 (ton (let* ((v (copy-seq "FOOBAR")) (e (scan '(simple-vector 3) v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---BAR"))

(deftest 26 (ton (multiple-value-bind (a b) (scan-multiple 'list '(1 2 3) '(3 4 5 6))
		   (list (collect a) (collect b))))
  ((1 2 3) (3 4 5)))
(deftest 27 (ton (multiple-value-bind (a b)
		     (scan-multiple '(values vector list) '#(1 2 3) '(3 4))
		   (list (collect a) (collect b))))
  ((1 2 3) (3 4 nil)))
(deftest 28 (ton (collect (until-if #'null (scan-multiple (cadr '(values list)) '(1 2 nil 3)))))
  (1 2))
(deftest 29 (ton (let* ((x (list 'a 'b nil 'c))
			(e (scan-multiple 'list x)))
		   (list (collect e) (alter e (scan-range)) x)))
  ((a b nil c) nil (0 1 2 3)))
(deftest 30 (ton (let* ((x (list 'a 'b nil 'c))
			(e (until-if #'null (scan-multiple 'list x))))
		   (list (collect e) (alter e (scan-range)) (collect e) x)))
  ((a b) nil (a b) (0 1 nil c)))

(deftest 31 (ton (collect (scan-sublists '(a b c)))) ((a b c) (b c) (c)))
(deftest 32 (ton (collect (scan-sublists ()))) ())

(deftest 33 (ton (collect (#Mlist (scan-range) #Z(a b c)))) ((0 a) (1 b) (2 c)))
(deftest 34 (ton (collect (#Mlist (scan-range :from 4 :by 3) #Z(a b c))))
  ((4 a) (7 b) (10 c)))
(deftest 35 (ton (collect (scan-range :upto 3))) (0 1 2 3))
(deftest 36 (ton (collect (scan-range :below 3))) (0 1 2))
(deftest 37 (ton (collect (scan-range :length 3))) (0 1 2))
(deftest 38 (ton (collect (scan-range :from 2 :upto 3))) (2 3))
(deftest 39 (ton (collect (scan-range :from 2 :below 3))) (2))
(deftest 40 (ton (collect (scan-range :from 2 :length 3))) (2 3 4))
(deftest 41 (ton (collect (scan-range :from 4 :upto 3))) ())
(deftest 42 (ton (collect (scan-range :from 4 :below 3))) ())
(deftest 43 (ton (collect (scan-range :from 4 :length 3))) (4 5 6))
(deftest 44 (ton (collect (scan-range :upto 3 :by 2))) (0 2))
(deftest 45 (ton (collect (scan-range :upto 4 :by 2))) (0 2 4))
(deftest 46 (ton (collect (scan-range :below 3 :by 2))) (0 2))
(deftest 47 (ton (collect (scan-range :below 4 :by 2))) (0 2))
(deftest 48 (ton (collect (scan-range :length 3 :by 2))) (0 2 4))
(deftest 49 (ton (collect (#M(lambda (x) (round (* 10. x)))
			     (scan-range :from 1.5 :by .2 :below 2.0))))
  (15 17 19))
(deftest 50 (ton (collect (#Mlist (scan-range :from 4 :by -3) #Z(a b c))))
  ((4 a) (1 b) (-2 c)))
(deftest 51 (ton (collect (scan-range :by -1 :downto -3))) (0 -1 -2 -3))
(deftest 52 (ton (collect (scan-range :by -1 :above -3))) (0 -1 -2))
(deftest 53 (ton (collect (scan-range :by -1 :length 3))) (0 -1 -2))
(deftest 54 (ton (collect (scan-range :from 4 :by -1 :downto 3))) (4 3))
(deftest 55 (ton (collect (scan-range :from 4 :by -1 :above 3))) (4))
(deftest 56 (ton (collect (scan-range :from 4 :by -1 :length 3))) (4 3 2))
(deftest 57 (ton (collect (scan-range :downto -3 :by -2))) (0 -2))
(deftest 58 (ton (collect (scan-range :downto -4 :by -2))) (0 -2 -4))
(deftest 59 (ton (collect (scan-range :above -3 :by -2))) (0 -2))
(deftest 60 (ton (collect (scan-range :above -4 :by -2))) (0 -2))
(deftest 61 (ton (collect (scan-range :length 3 :by -2))) (0 -2 -4))

(deftest 62 (ton (collect (scan-lists-of-lists '(1 (2 3) 4)))) ((1 (2 3) 4) 1 (2 3) 2 3 4))
(deftest 63 (ton (collect (scan-lists-of-lists '(1 (2 3) 4) #'atom)))
  ((1 (2 3) 4) 1 (2 3) 2 3 4))
(deftest 64 (ton (collect (scan-lists-of-lists '(1 (2 3) 4)
					       #'(lambda (n) (not (and (consp n) (cddr n)))))))
  ((1 (2 3) 4) 1 (2 3) 4))
(deftest 65 (ton (collect (scan-lists-of-lists nil))) (nil))

(deftest 66 (ton (collect (scan-lists-of-lists-fringe '((1 2 ((3 . 4) 4) (5) () (((6))))))))
  (1 2 3 4 5 nil 6))
(deftest 67 (ton (collect (scan-lists-of-lists-fringe '(1 2 ((3 . 4) 4) (5) () (((6))))
						      #'(lambda (n) (not (and (consp n) (cdr n)))))))
  (1 2 3 4 (5) nil (((6)))))
(deftest 68 (ton (collect (scan-lists-of-lists-fringe '((2) (nil))
						      #'(lambda (e) (numberp (car e))))))
  ((2) nil))

(deftest 69 (ton (collect (scan-lists-of-lists-fringe ()))) (nil))
(deftest 70 (ton (let ((tree (list (list 3) 4)))
		   (let ((leaf (choose-if #'evenp (scan-lists-of-lists-fringe tree))))
		     (alter leaf (#M- leaf)))
		   tree)) ((3) -4))
(deftest 71 (ton (let ((z (list 'a 'b (cons 3 'e) 'd)))
		   (let* ((x (scan-lists-of-lists-fringe z)))
		     (alter x (#Mlist x)))
		   z)) ((a) (b) ((3) . e) (d)))

(deftest 72 (ton (collect (scan-alist '((1 . a) () (2) (1 . c))))) (1 2))
(deftest 73 (ton (collect (scan-alist ()))) ())
(deftest 74 (ton (multiple-value-bind (key value)
		     (scan-alist '((1 . a) () (2) (1 . c)))
		   (collect (#Mlist key value)))) ((1 a) (2 nil)))
(deftest 75 (ton (let ((alist (list (cons 'a  1) (cons 'b 2))))
		   (multiple-value-bind (key val) (scan-alist alist)
		     (alter key (#Mlist key))
		     (alter val (#Mlist val)))
		   alist)) (((a) . (1)) ((b) . (2))))

(deftest 76 (ton (collect (scan-plist '(P1 1 P2 2 P1 3 P3 4)))) (P1 P2 P3))
(deftest 77 (ton (collect (scan-plist ()))) ())
(deftest 78 (ton (multiple-value-bind (key value) (scan-plist '(P1 1 P2 2 P1 3))
		   (collect (#Mlist key value)))) ((P1 1) (P2 2)))
(deftest 79 (ton (let ((plist (list 'a 1 'b 2)))
		   (multiple-value-bind (key val) (scan-plist plist)
		     (alter key (#Mlist key))
		     (alter val (#Mlist val)))
		   plist)) ((a) (1) (b) (2)))

(deftest 80 (ton (multiple-value-bind (key val)
		     (scan-hash (let ((x (make-hash-table)))
				  (setf (gethash 'color x) 'brown)
				  (setf (gethash 'name x) 'fred)
				  x))
		   (sort (collect (#Mcons key val))
			 #'(lambda (x y) (string-lessp (string (car x)) (string (car y)))))))
  ((color . brown) (name . fred)))

(deftest 81 (ton (progn (collect-first (scan-symbols)) nil)) nil) ;grotesquely weak tests
(deftest 82 (ton (progn (collect-first (scan-symbols (find-package "SERIES"))) nil)) nil)

;scan-file tested in conjunction with collect-file.

(deftest 83 (ton (collect (previous #Z(a b c)))) (nil a b))
(deftest 84 (ton (collect (previous #Z(a b c) 'fill 2))) (fill fill a))
(deftest 85 (ton (collect (previous #Z(a b c) 0))) (0 a b))

(deftest 86 (ton (collect (latch #Z(nil 3 nil 4 5)))) (nil 3 nil nil nil))
(deftest 87 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2))) (nil 3 nil 4 nil))
(deftest 88 (ton (collect (latch #Z(nil 3 nil 4 5) :after 0))) (nil nil nil nil nil))
(deftest 89 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :pre 'a))) (A A A A 5))
(deftest 90 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :pre 'a :post 'b)))
  (A A A A B))
(deftest 91 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :post 'b))) (nil 3 nil 4 B))
(deftest 92 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2))) (nil 3 nil nil nil))
(deftest 93 (ton (collect (latch #Z(nil 3 nil 4 5) :before 0))) (nil nil nil nil nil))
(deftest 94 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :pre 'a))) (A A A 4 5))
(deftest 95 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :pre 'a :post 'b)))
  (A A A B B))
(deftest 96 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :post 'b))) (nil 3 nil B B))

(deftest 97 (ton (collect (until #Z(nil nil T nil T) #Z(1 2 3)))) (1 2))
(deftest 98 (ton (multiple-value-bind (x y)
		     (until #Z(nil nil T nil T) #Z(1 2 3) #Z(a b c d))
		   (list (collect x) (collect y)))) ((1 2) (a b)))
(deftest 99 (ton (multiple-value-bind (x y)
		     (until #Z(nil nil) #Z(1 2 3) #Z(a b c d))
		   (list (collect x) (collect y)))) ((1 2) (a b)))
(deftest 100 (ton (multiple-value-bind (x y)
		      (until #Z(nil nil nil nil T) #Z(1 2 3) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2 3) (a b c)))
(deftest 101 (ton (multiple-value-bind (x y)
		      (until #Z(nil nil nil nil T) #Z(a b c d) #Z(1 2 3))
		    (list (collect x) (collect y)))) ((a b c) (1 2 3)))
(deftest 102 (ton (multiple-value-bind (x y z)
		      (until #Z(nil nil T nil T) #Z(a b c d) #Z(1 2 3) #Z(5 6 7))
		    (list (collect x) (collect y) (collect z)))) ((a b) (1 2) (5 6)))
(deftest 103 (ton (collect (until #Z() #Z(1 2 3)))) ())
(deftest 104 (ton (let ((x #Z(1 2 3 nil nil)))
		    (collect (until (previous (#Mnull x)) x))))
  (1 2 3 nil))

(deftest 105 (ton (collect (until-if #'null #Z(1 2 3 nil nil)))) (1 2 3))
(deftest 106 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(1 2 (3)) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2) (a b)))
(deftest 107 (ton (let ((z #'listp))
		    (multiple-value-bind (x y)
			(until-if z #Z(1 2 (3)) #Z(a b c d))
		      (list (collect x) (collect y))))) ((1 2) (a b)))
(deftest 108 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(1 2) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2) (a b)))
(deftest 109 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(a b c d) #Z(1 2))
		    (list (collect x) (collect y)))) ((a b) (1 2)))
(deftest 110 (ton (multiple-value-bind (x y z)
		      (until-if #'listp #Z(a b (c) d) #Z(1 2 3) #Z(5 6 7))
		    (list (collect x) (collect y) (collect z)))) ((a b) (1 2) (5 6)))
(deftest 111 (ton (let ((fn #'null))
		    (collect (until-if fn #Z(1 2 3 nil nil))))) (1 2 3))
(deftest 112 (ton (let ((v (list 1 -2 3)))
		    (let ((x (until-if #'minusp (scan v))))
		      (collect-sum x)
		      (alter x (#M- x)))
		    v)) (-1 -2 3))

(deftest 113 (ton (collect (map-fn T #'list #Z(1 2 3)))) ((1) (2) (3)))
(deftest 114 (ton (collect (map-fn 'list #'list #Z(1 2 3)))) ((1) (2) (3)))
(deftest 115 (ton (collect (map-fn '(values list) #'list #Z(1 2 3)))) ((1) (2) (3)))
(deftest 116 (ton (collect (map-fn '(values *) #'list #Z(1 2 3)))) ((1) (2) (3)))
(deftest 117 (ton (collect (map-fn T 'list #Z(1 2 3)))) ((1) (2) (3)))
(deftest 118 (ton (collect (map-fn T #'(lambda (z) (list z)) #Z(1 2 3)))) ((1) (2) (3)))
(deftest 119 (ton (multiple-value-bind (a b)
		      (map-fn '(values integer integer) #'(lambda (x) (values x (1+ x)))
			      #Z(1 2 3))
		    (collect (#Mlist a b)))) ((1 2) (2 3) (3 4)))
(deftest 120 (ton (let ((z 2))
		    (collect (map-fn T #'(lambda (x) (+ x z)) #Z(1 2 3))))) (3 4 5))
(deftest 121 (ton (let ((z 2))
		    (collect (map-fn T #'(lambda (x) (+ x z)) #Z(1 2 3))))) (3 4 5))

(deftest 122 (ton (collect (mapping ((e #Z(1 2 3))) (1+ e)))) (2 3 4))
(deftest 123 (ton (collect (mapping (((e f) (scan-plist '(a 1 b 2))))
			     (cons e f)))) ((a . 1) (b . 2)))
(deftest 124 (ton (collect (mapping ((d #Z(10 20 30 40))
				     ((e f) (scan-plist '(a 1 b 2))))
			     (list* d e f)))) ((10 a . 1) (20 b . 2)))

(deftest 125 (ton (let ((c 1))
		    (collect (#Mcons #Z(a b c) (#M(lambda () (incf c)))))))
  ((a . 2) (b . 3) (c . 4)))
(deftest 126 (ton (let* ((tt '((1 2) (3 4)))
			 (e (scan tt)))
		    (collect (#M(lambda (x y) (list (collect 'bag (scan x)) y)) e e))))
  (((2 1) (1 2)) ((4 3) (3 4))))
(deftest 127 (ton (let ((e #Z((1 2) (3 4))))
		    (collect (#M(lambda (x) (collect-sum (scan x))) e)))) (3 7))

(deftest 128 (ton (collect (collecting-fn T #'(lambda () 0) #'+ #Z(1 2 3)))) (1 3 6))
(deftest 129 (ton (collect (collecting-fn 'integer #'(lambda () 0) #'+ #Z(1 2 3)))) (1 3 6))
(deftest 130 (ton (collect (collecting-fn T #'(lambda () 0) '+ #Z(1 2 3)))) (1 3 6))
(deftest 131 (ton (collect (collecting-fn T #'(lambda () 0) #'(lambda (s z) (+ s z)) #Z(1 2 3))))
  (1 3 6))
(deftest 132 (ton (collect (collecting-fn '(values T T) #'(lambda () (values nil T))
					  #'(lambda (max flag n)
					      (values (if flag n (max max n)) nil))
					  #Z(1 4 2)))) (1 4 4))
(deftest 133 (ton (collect 'list
			   (collecting-fn '(values list integer) #'(lambda () (values nil 0))
					  #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
					  #Z(A B C) #Z(1 2 3))))
  (((a 1 0)) ((b 2 1) (a 1 0)) ((c 3 2) (b 2 1) (a 1 0))))
(deftest 134 (ton (collect (collecting-fn T #'(lambda () 0) #'- #Z(1 2 3)))) (-1 -3 -6))

(deftest 135 (ton (multiple-value-bind (x y) (cotruncate #Z(1 2 3) #Z(4 5))
		    (list (collect-sum x) (collect-sum y)))) (3 9))
(deftest 136 (ton (multiple-value-bind (x y z) (cotruncate #Z(1 2 3) #Z(4 5) #Z(9 8 7))
		    (list (collect-sum x) (collect-sum y) (collect-sum z)))) (3 9 17))
(deftest 137 (ton (multiple-value-bind (x y) (cotruncate #Z() #Z(4 5))
		    (list (collect-sum x) (collect-sum y)))) (0 0))
(deftest 138 (ton (multiple-value-bind (x y) (cotruncate #Z(1 2 3) #Z(4 5))
		    (list (collect-sum (#M+ x y)) (collect-sum y)))) (12 9))
(deftest 139 (ton (let ((ll (list 1 2 3 4)))
		    (multiple-value-bind (x y) (cotruncate (scan ll) #Z(4 5))
		      (list (collect x) (alter x y) ll (collect-sum y)))))
  ((1 2) nil (4 5 3 4) 9))

(deftest 140 (ton (let ((x '(b c)))
		    (collect (catenate (scan (cons 'a x)) #Z(1 2 3)))))
  (a b c 1 2 3))
(deftest 141 (ton (collect (catenate #Z() #Z(a b c) #Z() #Z(a b c))))
  (a b c a b c))
(deftest 142 (ton (let ((x #Z(1 2)) (y #Z(3 4)))
		    (collect (catenate x y)))) (1 2 3 4))

(deftest 143 (ton (let ((x '(b c)))
		    (collect (subseries (scan (cons 'a x)) 1 2)))) (b))
(deftest 144 (ton (collect (subseries #Z(a b c) 1))) (b c))
(deftest 145 (ton (let ((v (list 1 -2 3)))
		    (let ((x (subseries (scan v) 1)))
		      (alter x (#M- x)))
		    v)) (1 2 -3))

(deftest 146 (ton (collect (positions #Z(a nil 3 nil T nil)))) (0 2 4))
(deftest 147 (ton (let ((x '(3 T nil)))
		    (collect (positions (scan  (cons nil x)))))) (1 2))
(deftest 148 (ton (collect (positions #Z(nil nil)))) ())

(deftest 149 (ton (collect (subseries (mask #Z()) 0 6))) (nil nil nil nil nil nil))
(deftest 150 (ton (collect (subseries (mask #Z(0 2 4)) 0 6))) (T nil T nil T nil))

(deftest 151 (ton (collect (mingle #Z(1 3 7 9) #Z(4 5 8) #'<))) (1 3 4 5 7 8 9))
(deftest 152 (ton (collect (mingle #Z(4 5 8) #Z(1 3 7 9) #'<))) (1 3 4 5 7 8 9))
(deftest 153 (ton (collect (mingle #Z((1 a) (2 b)) #Z((1 c) (3 d))
				   #'(lambda (x y) (< (car x) (car y))))))
  ((1 a) (1 c) (2 b) (3 d)))

(deftest 154 (ton (collect (choose #Z(t t nil nil t) #Z(1 2 nil nil -4))))
  (1 2 -4))
(deftest 155 (ton (collect (choose #Z(1 2 nil nil -4)))) (1 2 -4))
(deftest 156 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (choose (#Mplusp x) x)))) (1 2))
(deftest 157 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (#M(lambda (x) (if (plusp x) x)) x)))) (1 nil 2 nil))
(deftest 158 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (#M(lambda (x) (if (plusp x) x (- x))) x)))) (1 1 2 2))
(deftest 159 (ton (let ((x #Z(0 1 -1 2 -2)))
		    (collect (#Mlist (choose (#Mplusp x) x) (scan-range))))) ((1 0) (2 1)))
(deftest 160 (ton (let ((x #Z(0 1 -1 2 -2))
			(tag (scan-range)))
		    (collect (#Mlist (choose (#Mplusp x) x) tag)))) ((1 0) (2 1)))
(deftest 161 (ton (let* ((l (list 1 2 nil nil -4))
			 (e (choose #Z(t t nil nil t) (scan l))))
		    (list (collect e) (alter e (#Mlist e)) l)))
  ((1 2 -4) nil ((1) (2) nil nil (-4))))

(deftest 162 (ton (collect (choose-if #'minusp #Z(1 2 -2 3 -4)))) (-2 -4))
(deftest 163 (ton (let ((fn #'minusp))
		    (collect (choose-if fn #Z(1 2 -2 3 -4))))) (-2 -4))
(deftest 164 (ton (let ((v (list 1 -2 3)))
		    (let ((x (choose-if #'minusp (scan v))))
		      (alter x (#M- x)))
		    v)) (1 2 3))

(deftest 165 (ton (collect (expand #Z(nil T nil T nil) #Z(a b c))))
  (nil a nil b nil))
(deftest 166 (ton (collect (expand #Z(nil T nil T) #Z(a b c) T))) (T a T b))

(deftest 167 (ton (collect (spread #Z(1 1) #Z(2 4) -1))) (-1 2 -1 4))
(deftest 168 (ton (collect (spread #Z(0 2 4) #Z(a b)))) (a nil nil b))
(deftest 169 (ton (collect (spread #Z(1) #Z(a b)))) (nil a))

(deftest 170 (ton (let* ((x #Z(1 -1 2 -2)))
		    (multiple-value-bind (y+ y-) (split x (series t nil t nil))
		      (list (collect x) (collect y+) (collect y-)))))
  ((1 -1 2 -2) (1 2) (-1 -2)))
(deftest 171 (ton (let* ((x #Z(1 0 -1 2 0 -2)))
		    (multiple-value-bind (y+ y- y0) (split x (series t nil nil t nil nil)
							   (series nil nil t nil nil t))
		      (list (collect y+) (collect y-) (collect y0) (collect x)))))
  ((1 2) (-1 -2) (0 0) (1 0 -1 2 0 -2)))
(deftest 172 (ton (let* ((l (list 1 -1 2 -2))
			 (x (scan l)))
		    (multiple-value-bind (y+ y-) (split x (series t nil t nil))
		      (list (collect x) (alter y+ (#Mlist y+)) (collect y+) (collect y-) l))))
  ((1 -1 2 -2) nil (1 2) (-1 -2) ((1) -1 (2) -2)))

(deftest 173 (ton (let* ((x #Z(1 -1 2 -2)))
		    (multiple-value-bind (y+ y-) (split-if x #'plusp)
		      (list (collect x) (collect y+) (collect y-)))))
  ((1 -1 2 -2) (1 2) (-1 -2)))
(deftest 174 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (collect (#M+ y+ y+))))
  (2 4))
(deftest 175 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (list (collect y+) (collect-sum y+))))
  ((1 2) 3))
(deftest 176 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (collect (catenate y+ #Z(5 6)))))
  (1 2 5 6))
(deftest 177 (ton (let* ((x #Z(1 0 -1 2 0 -2)))
		    (multiple-value-bind (y+ y- y0) (split-if x #'plusp #'minusp)
		      (list (collect y+) (collect y-) (collect y0) (collect x)))))
  ((1 2) (-1 -2) (0 0) (1 0 -1 2 0 -2)))
(deftest 178 (ton (let* ((x #Z(1 (nil) (3))))
		    (multiple-value-bind (y+ y- y0) (split-if x #'numberp #'car)
		      (list (collect y+) (collect y-) (collect y0)))))
  ((1) ((3)) ((nil))))

(deftest 179 (ton (multiple-value-bind (x y) (chunk 2 #Z(1 2 3 4))
		    (list (collect x) (collect y))))
  ((1 2 3) (2 3 4)))
(deftest 180 (ton (multiple-value-bind (x y) (chunk 2 2 #Z(1 2 3 4))
		    (list (collect x) (collect y))))
  ((1 3) (2 4)))
(deftest 181 (ton (multiple-value-bind (x y) (chunk 2 3 #Z(1 2 3 4 5))
		    (list (collect x) (collect y))))
  ((1 4) (2 5)))
(deftest 182 (ton (multiple-value-bind (x y) (chunk 2 #Z(1 2))
		    (list (collect x) (collect y))))
  ((1) (2)))
(deftest 183 (ton (multiple-value-bind (x y) (chunk 2 #Z(1))
		    (list (collect x) (collect y))))
  (() ()))
(deftest 184 (ton (collect (chunk 1 2 #Z(1 2 3)))) (1 3))
(deftest 185 (ton (multiple-value-bind (x y z) (chunk 3 2 #Z(1 2 3 4 5))
		    (list (collect x) (collect y) (collect z))))
  ((1 3) (2 4) (3 5)))

(deftest 186 (ton (collect #Z(a b c))) (a b c))

(deftest 187 (ton (collect 'bag #Z(a b c))) (c b a))
(deftest 188 (ton (collect-append 'list #Z((a b c) (a b c)))) (a b c a b c))
(deftest 189 (ton (collect-append (car '(list)) #Z((a b c) (a b c)))) (a b c a b c))
(deftest 190 (ton (collect-append 'list #Z())) ())
(deftest 191 (ton (let ((a (list 1 2)) (b '(3 4)))
		    (collect-append (scan (list a b)))
		    a)) (1 2))

(deftest 192 (ton (collect-nconc (scan (list nil (list 'a 'b) nil
					     (list 'c 'd) (list 'e) nil))))
  (a b c d e))
(deftest 193 (ton (collect-nconc #Z())) ())
(deftest 194 (ton (let ((a (list 1 2)) (b '(3 4)))
		    (collect-nconc (scan (list a b)))
		    a)) (1 2 3 4))

(deftest 195 (ton (collect-alist #Z(d e d) #Z(a b c))) ((d . c) (e . b) (d . a)))
(deftest 196 (ton (collect-alist #Z(d e d) #Z())) ())

(deftest 197 (ton (collect-plist #Z(d e d) #Z(a b c))) (d c e b d a))
(deftest 198 (ton (collect-plist #Z(d e d) #Z())) ())

(deftest 199 (ton (let ((h (collect-hash #Z(color name) #Z(brown fred))))
		    (multiple-value-bind (key val) (scan-hash h)
		      (sort (collect (#Mcons key val))
			    #'(lambda (x y)
				(string-lessp (string (car x)) (string (car y))))))))
  ((color . brown) (name . fred)))

(deftest 200 (ton (coerce (collect 'vector #Z(a b c)) 'list)) (a b c))
(deftest 201 (ton (coerce (collect 'vector #Z()) 'list)) ())
(deftest 202 (ton (collect '(simple-string 3) #Z(#\B #\A #\R))) "BAR")
(deftest 203 (ton (coerce (collect '(vector t 3) #Z(a b c)) 'list)) (a b c))

(deftest 204 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (collect-file test-file #Z(a b c))
			 (collect (scan-file test-file)))) (a b c))

(deftest 205 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (collect-file test-file #Z(#\a #\space #\newline #\c) #'write-char)
			 (collect (scan-file test-file #'read-line))))
  ("a " "c"))

(deftest 206 (ton (collect-last #Z(a b c))) c)
(deftest 207 (ton (collect-last #Z())) nil)
(deftest 208 (ton (collect-last #Z() 'fill)) fill)

(deftest 209 (ton (collect-length #Z(a b c))) 3)
(deftest 210 (ton (collect-length #Z())) 0)
(deftest 211 (ton (collect-length (choose (#Mplusp #Z(1 -1 2 -2))))) 2)

(deftest 212 (ton (collect-sum #Z(1 2 3))) 6)
(deftest 213 (ton (collect-sum #Z(1 2 3) 'float)) 6.0)
(deftest 214 (ton (collect-sum #Z() 'float)) 0.0)

(deftest 215 (ton (collect-min #Z(1 2 3))) 1)
(deftest 216 (ton (collect-min #Z(2 1 3) #Z(a b c) 4)) b)
(deftest 217 (ton (collect-min #Z())) nil)
(deftest 218 (ton (collect-min #Z() #Z(a b c) 4)) 4)
(deftest 219 (ton (collect-min #Z(a b c) #Z() 4)) 4)

(deftest 220 (ton (collect-max #Z(1 2 3))) 3)
(deftest 221 (ton (collect-max #Z(1 3 2) #Z(a b c))) b)
(deftest 222 (ton (collect-max #Z())) nil)
(deftest 223 (ton (collect-max #Z() #Z(a b c) 4)) 4)

(deftest 224 (ton (collect-fn T #'(lambda () 0) #'+ #Z(1 2 3))) 6)
(deftest 225 (ton (collect-fn 'integer #'(lambda () 0) #'+ #Z(1 2 3))) 6)
(deftest 226 (ton (collect-fn 'integer #'(lambda () 0) #'(lambda (x y) (+ x y)) #Z(1 2 3))) 6)
(deftest 227 (ton (collect-fn T #'(lambda () 0) #'(lambda (&rest args) (apply #'+ args))
			      #Z(1 2 3))) 6)
(deftest 228 (ton (collect-fn T #'(lambda () 0) #'- #Z(1 2 3))) -6)
(deftest 229 (ton (collect-fn T #'(lambda () 0) #'+ #Z())) 0)
(deftest 230 (ton (collect-fn T #'(lambda () T) #'+ #Z())) T)
(deftest 231 (ton (multiple-value-list
		      (collect-fn ' (values list integer) #'(lambda () (values nil 0))
				    #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
				    #Z(A B C) #Z(1 2 3))))
  (((c 3 2) (b 2 1) (a 1 0)) 3))
(deftest 232 (ton (multiple-value-list
		      (collect-fn '(values list integer) #'(lambda () (values nil 0))
				  #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
				  #Z(A B C) #Z(1 2 3))))
  (((c 3 2) (b 2 1) (a 1 0)) 3))

(deftest 233 (ton (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				(collect-fn T #'(lambda () 0)
					    #'(lambda (sum x)
						(incf xx)
						(+ sum x xx))
					    #Z(10 20 30)))) 66)
(deftest 234 (ton (multiple-value-list
		      (encapsulated #'(lambda (b) `(lisp:let ((xx 0)) ,b))
				    (collect-fn '(values t t)
						#'(lambda () (values 0 1))
						#'(lambda (sum prod x)
						    (incf xx)
						    (values (+ sum x xx) (* prod x xx)))
						#Z(10 20 30))))) (66 36000))

(deftest 235 (ton (collect-first #Z(a b c))) a)
(deftest 236 (ton (collect-first #Z())) nil)
(deftest 237 (ton (collect-first #Z() 'T)) T)
(deftest 238 (ton (collect-first (#Mcar #Z((T) (nil) 4)))) T)
(deftest 239 (ton (collect-first (positions (#Mplusp #Z(-3 1 -1 3 -2))))) 1)
(deftest 240 (ton (collect-first (choose #Z(nil t nil) #Z(0 1 -1 3 -2)))) 1)

(deftest 241 (ton (collect-nth 1 #Z(a b c))) b)
(deftest 242 (ton (collect-nth 1 #Z())) nil)
(deftest 243 (ton (collect-nth 1 #Z() 'T)) T)
(deftest 244 (ton (collect-nth 1 (#Mcar #Z((T) (nil) 4)))) nil)

(deftest 245 (ton (collect-and #Z(1 2))) 2)
(deftest 246 (ton (collect-and (#Mcar #Z((T) (nil) 4)))) nil)
(deftest 247 (ton (collect-and #Z())) T)

(deftest 248 (ton (collect-or #Z(nil))) nil)
(deftest 249 (ton (collect-or (#Mcar #Z((T) (nil) 4)))) T)
(deftest 250 (ton (collect-or #Z())) nil)

;this contains tests of the various special forms supported.
(deftest 251 (ton (let* ((lst (list 'a 'b 'c))
			 (l (scan-sublists lst))
			 (e (to-alter (#Mcar l)
				      #'(lambda (new parent)
					  (rplaca parent new))
				      l)))
		    (list (collect e)
			  (alter e (#Mlist e))
			  (collect e)
			  lst)))
  ((a b c) nil (a b c) ((a) (b) (c))))
(deftest 252 (ton (let* ((lst (list 1 2 3))
			 (l (scan-sublists lst))
			 (e (to-alter (#Mcar l)
				      #'(lambda (new parent num)
					  (rplaca parent (+ new num)))
				      l (series 10))))
		    (alter e (#M1+ e))
		    lst))
  (12 13 14))

(deftest 253 (ton (let* ((x #Z(a b c))
			 (xx (#Mlist x)))
		    (collect (#Mlist x xx)))) ((a (a)) (b (b)) (c (c))))
(deftest 254 (ton (let* ((x #Z(a b c))
			 (x (#Mlist x)))
		    (collect x))) ((a) (b) (c)))
(deftest 255 (ton (let ((x 9))
		    (let ((x #Z(a b c))
			  (xx (series (list x))))
		      (collect (#Mlist x xx))))) ((a (9)) (b (9)) (c (9))))
(deftest 256 (ton (let () (collect #Z(a b c)))) (a b c))
(deftest 257 (ton (let* ((e 3)
			 (f #Z(a b c))
			 (g (collect f))
			 (h (collect #Z(a b c))))
		    (list e g h))) (3 (a b c) (a b c)))
(deftest 258 (ton (let ((x (collect #Z(1 2 3))))
		    (list x)
		    x)) (1 2 3))
(deftest 259 (ton (let ())) nil)
(deftest 260 (ton (multiple-value-bind (key value) (scan-alist '((a . 1) (b . 2)))
		    (collect (#Mlist key value)))) ((a 1) (b 2)))
(deftest 261 (ton (let ((key (scan-alist '((a . 1) (b . 2)))))
		    (collect key))) (a b))

(deftest 262 (ton (collect-alist #Z(a b) (series (* 2 3)))) ((b . 6) (a . 6)))
(deftest 263 (ton (let ((x 1))
		    (collect-alist #Z(a b) (series (setq x (1+ x)))))) ((b . 2) (a . 2)))

(deftest 264 (ton (collect-sum (#Mcar #Z((1) (2))))) 3)
(deftest 265 (ton (collect-sum (#M(lambda (x) (* 2 x)) #Z(1 2)))) 6)
(deftest 266 (ton (let ((x 1))
		    (collect (#M(lambda (y) (list y (setq x (1+ x)))) #Z(a b))))) ((a 2) (b 3)))
(deftest 267 (ton (let ((x 1))
		    (collect (#Mlist #Z(a b) (series (setq x (1+ x))))))) ((a 2) (b 2)))
(deftest 268 (ton (collect (#M(lambda (x y) (if (plusp x) y))
			      #Z(10 -11 12) (scan-range)))) (0 nil 2))
(deftest 269 (ton (collect (choose (#Mplusp #Z(10 -11 12)) (scan-range)))) (0 2))

(deftest 270 (ton (let ((z #Z(1 2)))
		    (collect (#M(lambda (z) (do ((x 1 (1+ x)) (sum 0 (+ sum x)))
						((> x z) sum))) z))))
  (1 3))
(deftest 271 (ton (let ((z #Z((1 2) (3 4))))
		    (collect (#M(lambda (x) (collect (scan x))) z)))) ((1 2) (3 4)))

(deftest 272 (td (defun foo (list) "doc"
			(declare (optimizable-series-function))
			(#Mcar (scan list)))
		 (list #+symbolics(documentation 'foo 'function)
		       (collect (foo '((a) (b) (c))))))
  (#+symbolics"doc" (a b c)))
(deftest 273 (td (defun foo02 (v)
		   (declare (optimizable-series-function) ((vector *) v))
		   "doc"
		   (#Mcar (scan 'vector v)))
		 (list #+symbolics(documentation 'foo02 'function)
		       (collect (foo02 '#((a) (b) (c))))))
  (#+symbolics"doc" (a b c)))

(deftest 274 (td (defun foo1 (list &optional (plus 1))
		   (declare (optimizable-series-function))
		   (#M(lambda (x) (+ x plus)) (scan list)))
		 (list (collect (foo1 '(1 2 3) 3))
		       (collect (foo1 '(1 2 3)))))
  ((4 5 6) (2 3 4)))

(deftest 275 (td (defun foo2 (list &optional (plus 1 p?))
		   (declare (ignore plus) (optimizable-series-function))
		   (#M(lambda (x) (list x p?)) (scan list)))
		 (list (collect (foo2 '(1 2 3) 3))
		       (collect (foo2 '(1 2 3)))))
  (((1 T) (2 T) (3 T)) ((1 nil) (2 nil) (3 nil))))

(deftest 276 (td (defun foo3 (numbers)
		   (declare (optimizable-series-function))
		   (collect-sum (#M1+ numbers)))
		 (foo3 (mapping ((x (scan '(1 2 3)))) (1+ x))))
  12)

(deftest 277 (td (defun my-collect-last (items &optional (default nil))
		   (declare (optimizable-series-function))
		   (collect-fn '(series-element-type items)
			       #'(lambda () default)
			       #'(lambda (old new) (declare (ignore old)) new)
			       items))
		 (list (my-collect-last #Z()) (my-collect-last #Z(1 2))))
  (nil 2))
(deftest 278 (td (defun my-collect-last2 (items &optional (default nil))
		   (declare (optimizable-series-function))
		   (collect-fn '(series-element-type foo)
			       #'(lambda () default)
			       #'(lambda (old new) (declare (ignore old)) new)
			       items))
		 (list (my-collect-last2 #Z()) (my-collect-last2 #Z(1 2))))
  (nil 2))

(deftest 279 (ton (multiple-value-list
		      (let ((x #Z(a b))) (values (collect x) (collect 'bag x)))))
  ((a b) (b a)))

(deftest 280 (ton (multiple-value-bind (a b)
		      (#2M(lambda (x) (let ((*package* (find-package "USER")))
					(intern (string x))))
			  #Z(x y))
		    (collect (#Mlist a b)))) ((x :internal) (y :internal)))

(deftest 281 (ton (let ((v (list 1 -2 3)))
		    (let ((x (choose-if #'minusp (scan v))))
		      (alter x (#M- x)))
		    v)) (1 2 3))
(deftest 282 (ton (let ((x (list 'a 'b 'c)))
		    (alter (scan x) (scan-range))
		    x)) (0 1 2))
(deftest 283 (ton (let ((x '((a) (b) (c))))
		    (iterate ((a (scan x)) (b (scan-range)))
		      (setf (car a) b))
		    x)) ((0) (1) (2)))

(deftest 284 (ton (let ((e (scan (list 1 2)))) (alter e (#M1+ e)) (collect e))) (1 2))
(deftest 285 (ton (let ((x #Z(1 2 3))
			(y #Z(4 5)))
		    (list (collect-sum x) (collect-sum y)))) (6 9))
(deftest 286 (ton (list (collect-sum #Z(1 2 3)) (collect-sum #Z(4 5)))) (6 9))

(deftest 287 (ton (collect (producing (y) ((x #Z((1) (2))) (item nil))
				      (loop
					  (tagbody
					     (setq item (next-in x (terminate-producing)))
					     (next-out y (car item))))))) (1 2))
(deftest 288 (ton (producing ((number 0)) ((numbers #Z(1 2)) (num 0))
			     (declare ((series integer) numbers) (integer number num))
			     (loop
				 (tagbody
				    (setq num (next-in numbers (terminate-producing)))
				    (setq num (1+ num))
				    (setq number (+ number num)))))) 5)
(deftest 289 (ton (multiple-value-bind (sum y)
		      (producing ((number 0) y) ((numbers #Z(1 2)) num)
				 (loop
				     (tagbody
					(setq num (next-in numbers (terminate-producing)))
					(setq num (1+ num))
					(setq number (+ number num))
					(next-out y num))))
		    (list sum (collect y))))
  (5 (2 3)))
(deftest 290 (ton (multiple-value-bind (sum y)
		      (producing ((number 0) y) ((numbers #Z(1 2)) num)
				 (loop
				     (tagbody
					(setq num (next-in numbers (terminate-producing)))
					(setq num (1+ num))
					(if (evenp num) (go J))
					(next-out y num)
				      J (setq number (+ number num)))))
		    (list sum (collect y))))
  (5 (3)))
(deftest 291 (ton (let ((list nil))
		    (producing ((x nil)) ((numbers #Z(1 2)) num)
			       (loop
				   (tagbody
				      (setq num (next-in numbers (terminate-producing)))
				      (push num list))))
		    list)) (2 1))
(deftest 292 (ton (nreverse (producing ((list nil)) ((items #Z(1 2)) item)
				       (declare (list list))
				       (loop
					   (tagbody
					      (setq item (next-in items (terminate-producing)))
					      (setq list (cons item list))))))) (1 2))
(deftest 293 (ton (collect (producing (items) ((list '(1 2)) item)
				      (loop
					  (tagbody
					     (if (endp list) (terminate-producing))
					     (setq item (car list))
					     (setq list (cdr list))
					     (next-out items item)))))) (1 2))
(deftest 294 (ton (collect (producing (items) ((Nitems1 #Z(1 2)) (Nitems2 #Z(3 4))
					       (done nil) item)
				      (loop
					  (tagbody
					     (if done (go D))
					     (setq item (next-in Nitems1 (setq done T) (go D)))
					     (go F)
					   D (setq item (next-in Nitems2 (terminate-producing)))
					     (setq item (1+ item))
					   F (next-out items item)))))) (1 2 4 5))
(deftest 295 (ton (multiple-value-bind (x+ x-)
		      (producing (Nitems1 Nitems2) ((items #Z(1 -2 3 -4)) (pred #'plusp) item)
				 (loop
				     (tagbody
					(setq item (next-in items (terminate-producing)))
					(if (not (funcall pred item)) (go D))
					(next-out Nitems1 item)
					(go F)
				      D (next-out Nitems2 item)
				      F)))
		    (list (collect-sum x+) (collect-sum x-)))) (4 -6))
(deftest 296 (ton (collect (producing (items) ((Nitems #Z(1 -2 3)) item)
				      (declare (type (series integer) Nitems))
				      (loop
					  (tagbody
					   L (setq item (next-in Nitems (terminate-producing)))
					     (if (not (plusp item)) (go L))
					     (next-out items item)))))) (1 3))
(deftest 297 (ton (let*
		      ((lst (list 1 2 3))
		       (e (producing (items) ((Nitems (scan lst)) item)
				     (declare (type (series integer) Nitems)
					      (propagate-alterability Nitems items))
				     (loop
					 (tagbody
					  L (setq item (next-in Nitems (terminate-producing)))
					    (if (not (plusp item)) (go L))
					    (next-out items item))))))
		    (list (collect e) (alter e (#M1+ e)) (collect e) lst)))
  ((1 2 3) nil (1 2 3) (2 3 4)))
(deftest 298 (ton (let*
		      ((lst (list 1 2 -3 4))
		       (y (producing (items) ((Nitems (scan lst)) item)
				     (declare (type (series integer) Nitems)
					      (propagate-alterability Nitems items))
				     (loop
					 (tagbody
					  L (setq item (next-in Nitems (terminate-producing)))
					    (if (not (plusp item)) (go L))
					    (next-out items item))))))
		    (alter y (#M1+ y))
		    lst)) (2 3 -3 5))
(deftest 299 (ton (let ((x '(1 2 3)))
		    (producing ((xx nil)) ()
			       (loop
				   (tagbody
				      (setq xx (if (null x) (terminate-producing) (pop x))))))))
  3)
(deftest 300 (td (defmacro Rcount (items)
		   (let ((counter (gensym)))
		     `(encapsulated #'(lambda (body)
					(list 'let '((,counter 0))
					      body))
				    (collect-fn 'fixnum
						#'(lambda () 0)
						#'(lambda (count x)
						    (declare (ignore count x))
						    (incf ,counter))
						,items))))
		 (Rcount #Z(1 2 3))) 3)

;
;the following tests all kinds of wierd combinations
;mg1
(deftest 301 (ton (funcall #'(lambda (x) (let ((z (list x))) (list z))) 4))
  ((4)))
(deftest 302 (ton (funcall #'(lambda (x) (nreverse (collect 'bag x))) #Z(a b c)))
  (a b c))
(deftest 303 (ton (funcall #'(lambda (x) (collect (#Mlist x))) #Z(a b c)))
  ((a) (b) (c)))
;mg2
(deftest 304 (ton (funcall #'(lambda (x y)
			       (list (collect x) (collect (choose (#Mplusp y) y))))
			   #Z(a b c) #Z(1 -2 3)))
  ((a b c) (1 3)))
(deftest 305 (ton (funcall #'(lambda (x y)
			       (list (collect (choose (#Mplusp y) y)) (collect x)))
			   #Z(a b c) #Z(1 -2 3)))
  ((1 3) (a b c)))

;mg3
(deftest 306 (ton (collect (funcall #'(lambda (x y z) (catenate (mingle x y #'<) z))
				    #Z(1 2 4) #Z(1 3 3) #Z(0))))
  (1 1 2 3 3 4 0))
(deftest 307 (ton (multiple-value-bind (a b) (scan-plist '(k1 2 k2 4))
		    (list (collect b)
			  (collect (expand (series nil nil T nil T nil nil nil T)
					   a nil)))))
  ((2 4) (nil nil k1 nil k2 nil nil nil)))
(deftest 308 (ton (collect (funcall #'(lambda (x)
					(multiple-value-bind (a b) (scan-plist x)
					  (expand #Z(nil nil T nil T nil nil nil T)
						  a nil)
					  b))
				    '(k1 2 k2 4))))
  (2 4))
(deftest 309 (ton (collect (funcall #'(lambda (x) (catenate (#Mlist x) #Z(5 6)))
				    #Z(1 2 3))))
  ((1) (2) (3) 5 6))
(deftest 310 (ton (collect (funcall #'(lambda (x) (catenate (choose (#Mplusp x) x) #Z(5 6)))
				    #Z(1 -2 3))))
  (1 3 5 6))
(deftest 311 (ton (collect (funcall #'(lambda (x) (choose-if #'evenp (split-if x #'plusp)))
				    #Z(1 2 -2 3 4))))
  (2 4))
(deftest 312 (ton (collect (funcall #'(lambda (x) (#Mlist (split-if x #'plusp)))
				    #Z(1 2 -2 3 4))))
  ((1) (2) (3) (4)))
;mg4
(deftest 313 (ton (multiple-value-bind (a b) (scan-plist '(k1 1 k2 -2))
		    (list (collect a) (collect (choose-if #'plusp b)))))
  ((k1 k2) (1)))
(deftest 314 (ton (collect (funcall #'(lambda (x)
					(multiple-value-bind (a b) (scan-plist x)
					  (collect (choose-if #'plusp b)) a))
				    '(k1 1 k2 -2))))
  (k1 k2))
(deftest 315 (ton (let (z)
		    (list (collect (funcall #'(lambda (x)
						(multiple-value-bind (a b) (scan-plist x)
						  (setq z (collect  'bag (choose-if #'plusp b)))
						  (#Mlist a)))
					    '(k1 1 k2 -2)))
			  z)))
  (((k1) (k2)) (1)))
;mg5
(deftest 316 (ton (multiple-value-bind (A B)
		      (funcall #'(lambda (x y)
				   (cotruncate (choose (#Mplusp x) x) (scan y)))
			       #Z(1 -2 3) '(a b c))
		    (list (collect a) (collect b))))
  ((1 3) (a b)))

(deftest 317 (ton (multiple-value-bind (A B)
		      (funcall #'(lambda (x y)
				   (cotruncate (choose (#Mplusp x) x) (scan y)))
			       #Z(1 -2 3) '(a b c))
		    (list (collect a) (collect b))))
  ((1 3) (a b)))

;these are weird tests checking for particular bugs in old versions
(deftest 318 (ton (multiple-value-list
		      (let ((strings (choose-if #'stringp (scan '(1 2 "COND" 4)))))
			(find-symbol (collect-first strings)))))
  (cond :inherited))
(deftest 319 (td (defun weighted-sum (numbers weights)
		   (declare (optimizable-series-function 2) (off-line-port weights))
		   (values (collect-sum numbers) (collect-sum (#M* numbers weights))))
		 (list (multiple-value-list (weighted-sum #Z(1 2 3) #Z(3 2)))
		       (multiple-value-list (weighted-sum #Z(1 2) #Z(3 2)))
		       (multiple-value-list (weighted-sum #Z(1) #Z(3 2)))))
  ((6 7) (3 7) (1 3)))
(deftest 320 (td (defun non-series-used-twice (x)
		   (declare (optimizable-series-function))
		   (let ((s (scan-range)))
		     (scan (list (collect-nth x s) (+ x (collect-nth x s))))))
		 (collect (non-series-used-twice (1+ 2))))
  (3 6))
(deftest 321 (td (defun baz (items)
		   (declare (optimizable-series-function))
		   (let ((items items))
		     (collect items)))
		 (baz #Z(1 2 3)))
  (1 2 3))
(deftest 322 (ton (let ((z -1))
		    (let ((e #Z(1 2 3)))
		      (setq z (collect-last e))
		      z))) 3)
(deftest 323 (ton (let ((x (list 1 2 3)))
		    (collect-last
		     (#M(lambda (x y) (list (setf (car x) y)))
			(scan-sublists x) #Z(a b c d))) x))
  (a b c))	;don't want to have any complaints from setf here.
(deftest 324 (ton (collect-first (choose-if #'(lambda (x) (and (car x) (cdr x)))
					    #Z((a) (nil . b) (a . b) (c))))) (a . b))
(deftest 325 (ton (let ((l (car '((1 2 3 4)))))
		    (collect (#Mlist (scan l) (scan l))))) ((1 1) (2 2) (3 3) (4 4)))
(deftest 326 (ton (let ((x nil))
		    (iterate ((y #Z(1 2)))
		      (push y x)) x)) (2 1))
;tests 327--330 removed.
(deftest 331 (ton (let ((x #Z(2 -1 0 1 -2)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose-if #'minusp x))))) (3 -3))
(deftest 332 (ton (let ((x #Z(2 -1 0 1 -2)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose (#Mminusp x) x)))))
  (3 -3))
(deftest 333 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x) (collect-sum (choose-if #'minusp x))))) (0 -3))
(deftest 334 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x) (collect 'bag (choose-if #'plusp x))))) (0 (1 2)))
(deftest 335 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x)
			  (collect 'bag (choose-if #'plusp x))
			  (collect-sum (choose-if #'plusp x)))))
  (0 (1 2) 3))
(deftest 336 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose-if #'minusp x))))) (3 -3))
(deftest 337 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose (#Mminusp x) x)))))
  (3 -3))
(deftest 338 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect 'bag (choose-if #'plusp x)))))
  (3 (1 2)))
(deftest 339 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect 'bag (choose-if #'plusp x))
			  (collect-sum (choose-if #'plusp x)))))
  (3 (1 2) 3))

(deftest 340 (ton (let ((v (list 1 -2 3)))
		    (let* ((e (scan v))
			   (x (until-if #'minusp e)))
		      (alter x (#M- x)))
		    v)) (-1 -2 3))
(deftest 341 (ton (collect (subseries (mask (positions #Z(t nil t nil))) 0 5)))
  (t nil t nil nil))
(deftest 342 (ton (let ((x '(1 2 3)))
		    (macrolet ((bab (z) `(list ,z)))
		      (collect (scan (bab x)))))) ((1 2 3)))
(deftest 343 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when x (setq xx x))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) 1))
(deftest 344 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when x (setq xx (collect (scan-range :upto x))))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) (0 1)))
(deftest 345 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when t (setq xx (collect (scan-range :upto 2))))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) (0 1 2)))
(deftest 346 (ton (let ((x #Z(1 2)))
		    (if (collect-sum x) 1 2))) 1)

(deftest 347 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b))) (nil (1 2) (1 2)))
(deftest 348 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (setq a (length a)) a (collect z) b)))
  (nil 0 0 (1 2) (1 2)))
(deftest 349 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (setq a 3) a (collect z) b)))
  (nil 3 3 (1 2) (1 2)))
(deftest 350 (ton (let ((zz nil))
		    (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		      (list a (multiple-value-setq (zz a) (truncate 5 2))
			    a zz (collect z) b))))
  (nil 2 1 2 (1 2) (1 2)))
(deftest 351 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq z (#M1+ z))
		    (list a (collect z) b))) (nil (2 3) (1 2)))
(deftest 352 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list b (setq b 4) a (collect z) b)))
  ((1 2) 4 nil (1 2) 4))
(deftest 353 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq a (#M1+ z))
		    (list (collect a) (collect z) b)))
  ((2 3) (1 2) (1 2)))
(deftest 354 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq z (collect-sum z))
		    (list a z b)))
  (nil 3 (1 2)))
(deftest 355 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (when (not a) (setq b '(3)))
		    (list a (collect z) b)))
  (nil (1 2) (3)))
(deftest 356 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b (when (not a) (setq b '(3))) b)))
  (nil (1 2) (1 2) (3) (3)))
(deftest 357 (ton (let* ((a 2) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b (when a (setq a 3)) a)))
  (2 (1 2) (1 2) 3 3))
(deftest 358 (ton (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect (#M(lambda (y) (setq a (1+ a)) (+ a y)) z))
			  a (collect z) b)))
  (3 (5 7) 5 (1 2) (1 2)))
(deftest 359 (ton (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		    (list (collect z)
			  (collect (#M(lambda (y) (setq z -3) (1+ y)) z))
			  a z b)))
  ((1 2) (2 3) 3 -3 (1 2)))
(deftest 360 (ton (let* ((z 0))
		    (list z
			  (collect (scan-fn t #'(lambda () (car (setq z (list '(1 2 3)))))
					    #'(lambda (l) (car (push (cdr l) z)))
					    #'(lambda (l) (push (car l) z) (null l))))
			  (reverse z))))
  (0 ((1 2 3) (2 3) (3)) ((1 2 3) 1 (2 3) 2 (3) 3 nil nil)))

(deftest 361 (ton (let ((z nil))
		    (let* ((x #Z(1 2 3)))
		      (setq z (cons (collect-sum x) z))
		      (setq z (cons 'a z))
		      (cons 'b z))))
  (b a 6))
(deftest 362 (ton (let ((z nil))
		    (let* ((x #Z(1 2 3)))
		      (setq z (cons 'a z))
		      (setq z (cons (collect-sum x) z))
		      (cons 'b z))))
  (b 6 a))
(deftest 363 (ton (let ((z nil))
		    (let* ((x (scan (progn (push 1 z) '(1 2))))
			   (y (scan (progn (push 2 z) '(1 3)))))
		      (setq z (cons (collect-sum x) z))
		      (setq z (cons (collect-sum y) z))
		      (cons 'b z))))
  (b 4 3 2 1))
(deftest 364 (ton (let ((*print-length* nil) (*print-level* nil))
		    (with-output-to-string (s)
					   (prin1 #Z(1 2) s))))
  "\#Z(1 2)")
(deftest 365 (ton (let ((*print-length* 1) (*print-level* 3))
		    (with-output-to-string (s)
					   (prin1 #Z(1 2) s))))
  "\#Z(1 ...)")
(deftest 366 (ton (let ((x #Z(1 2 3)) (y 4))
		    (setq y (collect (#M+ (series y) x)))))
  (5 6 7))
(deftest 367 (ton (collect (let ((x #Z(1 2 3 4)) (y (series 4)))
			     (setq y (split-if (#M+ y x) #'evenp)))))
  (6 8))
(deftest 368 (td (defun pip1 (x)
		   (declare (optimizable-series-function) (off-line-port 0))
		   (push 3 x)
		   (values (scan x) (scan (cdr x))))
		 (multiple-value-bind (a b) (pip1 '(a b))
		   (list (collect a) (collect b))))
  ((3 a b) (a b)))

;tests of places where some termination points are not connected to every output
(deftest 369 (ton (let ((x #Z(1 2 3)))
		    (list (collect-first x) (collect-sum x))))
  (1 6))
(deftest 370 (ton (let ((x #Z(foo nil bar)))
		    (list (collect-and x) (collect x))))
  (nil (foo nil bar)))
(deftest 371 (ton (let ((z 0))
		    (list (let ((x #Z(1 2 3)))
			    (iterate ((v x))
			      (setq z (+ z v)))
			    (collect-first x))
			  z)))
  (1 6))
(deftest 372 (ton (let ((z 0))
		    (list (let ((x #Z(1 2 3)))
			    (mapping ((v x))
			      (setq z (+ z v)))
			    (collect-first x))
			  z)))
  (1 0))
(deftest 373 (ton (multiple-value-list
		      (let* ((x (list 'a 'b 'c 'd))
			     (e (scan x)))
			(values (collect e)	
				(let () (alter e (scan '(1 2))) (collect e))
				x))))
  ((a b c d) (a b c d) (1 2 c d)))
(deftest 374 (ton (let ((x #Z(1 2 3))
			(y #Z(4 5 6 7)))
		    (list (collect-sum (#M+ x y)) (collect-sum y))))
  (21 22))
(deftest 375 (ton (let ((x #Z(1 2 3))
			(y #Z(4 5 6 7)))
		    (list (collect-sum (#M+ x y)) (collect-sum y) (collect-sum y))))
  (21 22 22))
(deftest 376 (ton (let* ((e #Z(1 -2 -4 3))
			 (f #Z(a)))
		    (multiple-value-bind (w x) (split-if e #'plusp)
		      (list (collect (#Mlist w))
			    (collect (#Mlist x f))))))
  (((1) (3)) ((-2 a))))
(deftest 377 (ton (let ((e #Z(1 -2 -4 3 5 6)))
		    (list (collect e) (collect (subseries e 1 3)))))
  ((1 -2 -4 3 5 6) (-2 -4)))
(deftest 378 (ton (let ((e #Z(1 -2 -4 3 5 6)))
		    (list (collect e) (collect (#Mlist (subseries e 1 3) #Z(a))))))
  ((1 -2 -4 3 5 6) ((-2 a))))
(deftest 379 (ton (let* ((e1 #Z(1 -2 -4 3)) (e2 #Z(1 -2 -4 3)) (e3 #Z(1 -2 -4 3))
			 (w1 (split-if e2 #'plusp)))
		    (multiple-value-bind (x1 x2) (split-if e3 #'plusp)
		      (declare (ignore x1))
		      (list (collect (#Mlist e1 w1)) (collect (#Mlist w1 x2))))))
  (((1 1) (-2 3)) ((1 -2) (3 -4))))
(deftest 380 (td (defun bar1 (numbers)
		   (declare (optimizable-series-function) (off-line-port numbers))
		   (scan (collect numbers)))
		 (collect-sum (bar1 (scan-range :upto 3))))
  6)
(deftest 381 (td (defun bar2 (numbers others)
		   (declare (optimizable-series-function)
			    (type (series integer) numbers)
			    (off-line-port numbers others))
		   (list (collect-sum numbers) (collect-sum others)))
		 (bar2 (scan-range :upto 3) (scan-range :upto -1)))
  (6 0))
(deftest 382 (td (defun bar3 (numbers others)
		   (declare (type series numbers others)
			    (optimizable-series-function)
			    (off-line-port numbers others))
		   (iterate ((n numbers))
		     (setq *x* n))
		   (collect-sum others))
		 (list (bar3 (scan-range :upto 3) (scan-range :upto 0)) *x*))
  (0 3))
(deftest 383 (td (defun bar4 (numbers others)
		   (declare (optimizable-series-function 2)
			    (type (series nil) y others)
			    (off-line-port numbers others))
		   (values (collect-sum numbers) (collect-sum others)))
		 (multiple-value-list
		   (bar4 (scan-range :upto 3) (scan-range :upto -1))))
  (6 0))
(deftest 384 (td (defun bar5 (numbers)
		   (declare (optimizable-series-function 2)
			    (type (series t) numbers))
		   (floor (collect-sum numbers) 4))
		 (multiple-value-list (bar5 (scan-range :upto 3))))
  (1 2))

;tests of declarations and the like that only apply to optimization

(deftest 385
  (to (not (null (member '(type integer x)
			 (decls (let ((x #Z(1 2 3)))
				  (declare (type (series integer) x))
				  (collect-sum x))) :test #'equal)))) T)
(deftest 386
  (to (not (null (member '(type integer x)
			 (decls (let* ((y #Z(1 2))
				       (x (the integer (collect-sum y))))
				  (list x x))) :test #'equal)))) T)
(deftest 387
  (to (not (null (member '(type integer y)
			 (decls (let* ((y (the (series *) #Z(1 2)))
				       (x (collect-sum y)))
				  (list x x))) :test #'equal)))) nil)
(deftest 388
  (to (not (null (member '(type integer y)
			 (decls (let* ((y (the (series integer) #Z(1 2)))
				       (x (collect-sum y)))
				  (list x x))) :test #'equal)))) T)

;tests of some otherwise hard to test internal functions
;these would probably have to be changed a good deal if there were any
;significant internal modifications in the way things worked.

(deftest 389 (ton (series::nsubst-inline nil 1 (list 3 1 2))) (3 2))
(deftest 390 (ton (series::nsubst-inline nil 4 (list 3 1 2))) (3 1 2))
(deftest 391 (ton (series::nsubst-inline nil 2 (list 3 1 2))) (3 1))

(deftest 392 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((if (car x) (go ,series::end)))))) T)
(deftest 393 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((tagbody ,series::end
						   (if (car x) (go ,series::end))))))) nil)
(deftest 394 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((tagbody (if (car x) (go ,series::end))))))) t)

(deftest 395 (ton (series::vars-of '((:foo bar) 1 bar-p))) (bar bar-p))

(deftest 396 (ton (series::make-general-setq '(x y) '(values 1 2))) (psetq x 1 y 2))
(deftest 397 (ton (series::make-general-setq '(x y) '(values 1 2 3)))
  (multiple-value-setq (x y) (values 1 2 3)))

(deftest 398 (ton (let ((code (copy-tree '((setq x 3) (setq y 4)))))
		    (series::clean-code1 '(x) code) code)) ((setq y 4)))
(deftest 399 (ton (let ((code (copy-tree '((setq x 3)))))
		    (series::clean-code1 '(x) code) code)) (3))
(deftest 400 (ton (let ((code (copy-tree  '((progn (setq x 3) . 4)))))
		    (series::clean-code1 '(x) code) code)) ((progn . 4)))
(deftest 401 (ton (let ((code (copy-tree '((progn (setq x 3))))))
		    (series::clean-code1 '(x) code) code)) ((progn)))

(deftest 402 (ton (series::make-test nil)) t)
(deftest 403 (ton (series::make-test '((x y) (x)))) x)
(deftest 404 (ton (series::make-test '((x z) (x y w)))) (or x (and z (or y w))))
(deftest 405 (ton (series::make-test '((x z) (x y w) (z)))) (and (or x y w) z))

;tests of generators and gatherers.

(deftest 406 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x (return nil)) l)
			(push (next-in x (return nil)) l)
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 +))
(deftest 407 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x (next-in x (return nil))) l)
			(push (next-in x (return nil)) l)
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 +))
(deftest 408 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x 44) l)
			(if (= 44 (car l)) (return nil))
		      (push (next-in x 55) l)
		      (if (= 55 (car l)) (return nil))
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 + 44))

(deftest 409 (ton (let ((g (gatherer #'collect-sum)))
		    (next-out g 3)
		    (next-out g 4)
		    (result-of g))) 7)
(deftest 410 (ton (let ((g (gatherer #'(lambda (x) (collect x)))))
		    (result-of g))) nil)
(deftest 411 (ton (let ((g (gatherer #'(lambda (x) (collect (choose-if #'plusp x))))))
		    (next-out g 3)
		    (next-out g -3)
		    (next-out g 4)
		    (result-of g))) (3 4))
(deftest 412 (ton (let ((g (gatherer #'(lambda (x) (collect (choose (#Mplusp x) x))))))
		    (next-out g 3)
		    (next-out g -3)
		    (next-out g 4)
		    (result-of g))) (3 4))
(deftest 413 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (let ((g (gatherer #'(lambda (x) (collect-file test-file x)))))
			   (next-out g 3)
			   (next-out g 4)
			   (list (result-of g) (collect (scan-file test-file)))))) (T (3 4)))
(deftest 414 (ton (let ((x (gatherer #'(lambda (x) (collect x))))
			(y (gatherer #'(lambda (ns) (collect-sum (choose-if #'oddp ns))))))
		    (dotimes (i 4)
		      (next-out x i)
		      (next-out y i)
		      (if (evenp i) (next-out x (* i 10))))
		    (list (result-of x) (result-of y)))) ((0 0 1 2 20 3) 4))

(deftest 415 (ton (gathering ((y collect-sum))
			     (next-out y 1) (next-out y 2))) 3)
(deftest 416 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (prog1 (list (gathering ((g (lambda (x) (collect-file test-file x))))
						 (next-out g 3)
						 (next-out g 4))
				      (collect (scan-file test-file)))
			   (if (probe-file test-file) (delete-file test-file))))) (T (3 4)))
(deftest 417 (ton (multiple-value-list
		      (gathering ((x (lambda (x) (collect x)))
				  (y collect-sum))
				 (dotimes (i 3)
				   (next-out y i)
				   (if (evenp i) (next-out x (* i 10))))))) ((0 20) 3))

;the following tests were introduced to insure that every control path
;in the system is exercised by atleast one test.

(deftest 418 (ton (let ((xx 0))
		    (list (let ((x #Z(1 2 3)))
			    (producing ((number 0) y) ((numbers x) num)
				       (loop
					   (tagbody
					      (setq num (next-in numbers (terminate-producing)))
					      (setq num (1+ num))
					    ;; nil
					      (setq xx (cons num '(1)))
					      (setq number (+ number num))
					      (next-out y num))))
			    (collect x))
			  xx)))
  ((1 2 3) (4 1)))
(deftest 419 (tn (typep #Z(1 2 3) 'series)) t)
(deftest 420 (tn (typep (cotruncate #Z(1 2 3) #Z(1 2)) 'series)) t)

(deftest 421 (ton (collect (funcall #'scan '(1 2)))) (1 2))
(deftest 422 (to (funcall #'collect #Z(1 2))) (1 2))
(deftest 423 (ton (funcall #'(lambda (x &optional (y 2)) (list x y)) 1)) (1 2))

(deftest 424 (ton (let ((x #Z(1 2 3)))
		    (multiple-value-bind (a b) (collect x)
		      (list a b))))
  ((1 2 3) nil))

(deftest 425 (ton (let ((x #Z(a b c)))
		    (list (collect x) (collect (catenate x #Z(1 2))))))
  ((a b c) (a b c 1 2)))
(deftest 426 (ton (let ((xx nil))
		    (list (collect
			   (catenate (producing (a (b nil)) ((x #Z(1 2 3)))
						(loop (tagbody (setq b (next-in x (terminate-producing)))
							 (setq xx b)
							 (next-out a (list b)))))
				     #Z(1 2)))
			  xx)))
  (((1) (2) (3) 1 2) 3))

(deftest 427 (ton (let ((x #Z(1 2 3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag T)
						     j (next-out a xval))))))))
  ((1 2 3) (1 2 3 3)))
(deftest 428 (ton (let ((x #Z(1 2 3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag T)
						     j (next-out a xval))))))))
  ((1 2 3) (1 2)))
(deftest 429 (ton (let ((x #Z(1 2 -3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag T)
						     j (next-out a xval))))))))
  ((1 2 -3) (1 2)))

(deftest 430 (ton (let ((x #Z(a b c)) (y #Z(0)))
		    (list (collect x) (collect y) (collect (catenate x y #Z(1 2))))))
  ((a b c) (0) (a b c 0 1 2)))
(deftest 431 (td (defun comp5 (numbers)
		   (declare (optimizable-series-function) (off-line-port numbers))
		   (let ((x #Z(a b c)))
		     (list (collect x) (collect (catenate x numbers)))))
		 (comp5 #Z(1 2)))
  ((a b c) (a b c 1 2)))
(deftest 432 (ton (let ((x #Z(a b c)))
		    (list (collect (catenate x #Z(-1 -2))) (collect (catenate x #Z(1 2))))))
  ((a b c -1 -2) (a b c 1 2)))
(deftest 433 (td (defun comp6 (numbers)
		   (declare (optimizable-series-function))
		   (let ((numbers (split-if numbers #'plusp)))
		     (list (collect numbers) (collect (subseries numbers 1)))))
		 (comp6 #Z(1 -1 2)))
  ((1 2) (2)))
(deftest 434 (td (defun comp7 (x)
		   (declare (optimizable-series-function))
		   (let ((numbers (split-if #Z(1 -1 2) #'plusp)))
		     (collect (#Mlist numbers x))))
		 (comp7 #Z(a b c)))
  ((1 a) (2 b)))

(deftest 435 (ton (let ((x #Z(1 2 3)) (y #Z(2)))
		    (list (collect-sum x) (collect-sum (#M* x (catenate y #Z(1)))))))
  (6 4))

(deftest 436 (ton (collect (mapping ((x #Z(1 2)))
			     (do* ((i 1 (1+ i))
				   (r 0 (+ x r)))
				  ((> i x) r)))))
  (1 4))

(deftest 437 (ton (collect (mapping ((x #Z(1 2)))
			     ((lambda (i) (list i)) (1+ x)))))
  ((2) (3)))

(deftest 438 (ton (let (a b)
		    (collect (mapping ((x #Z(1 2 3)))
			       (multiple-value-setq (a b) (floor x 2))
			       (list a b)))))
  ((0 1) (1 0) (1 1)))
(deftest 439 (ton (let (a b)
		    (let ((x #Z(1 2 3)))
		      (multiple-value-setq (a b) (floor 1 1))
		      (list a b (collect x)))))
  (1 0 (1 2 3)))

(deftest 440 (ton (let ((x #Z(1 2 3)))
		    (multiple-value-bind (x y)
			(collect-fn '(values t t)
				    #'(lambda () (values 1 2))
				    #'(lambda (x y z) (values (+ x z) (+ y z)))
				    x)
		      (list (list x) (list y)))))
  ((7) (8)))

(deftest 441 (td (defun zz () 2)
		 (zz))
  2)
(deftest 442 (td (defun zz1 (x) (declare (ignore x)) "foo")
		 (zz1 1))
  "foo")

;more special tests

;The following test error checking.
;Some must be tested only when optimized, because they
;cause ordinary errors when non-optmized

(deftest 443 (te (generator 3)) 60)

(deftest 444 (te (gatherer #'(lambda (x) (values (collect x) (collect x))))) 61)
(deftest 445 (te (gatherer #'(lambda (x y) (collect (#M+ x y))))) 61)
(deftest 446 (te (gatherer #'(lambda (x) (scan x)))) 61)
(deftest 447 (te (gatherer #'scan)) 61)
(deftest 448 (te (gatherer #'positions)) 61)
(deftest 449 (te (tr (gatherer #'scan))) 61)

(deftest 450 (te (map-fn '(values) #'1+ #Z(1 2 3))) 62)

(deftest 451 (te (chunk -1 #Z(1 2 3))) 63)

(deftest 452 (te (chunk 1 -1 #Z(1 2 3))) 64)

(deftest 453 (te (alter (scan-range :upto 4) (series 5))) 65)
(deftest 454 (te (alter (#Mcar (scan '((1)))) (series 5))) 65)
(deftest 455 (te (alter (positions (scan '(1))) (series 5))) 65)

(deftest 456 (teo (let ((t #Z(1))) a)) 66)
(deftest 457 (teo (multiple-value-bind ((a)) #Z(1) a)) 66)
(deftest 458 (teo (multiple-value-bind (t b) #Z(1) a)) 66)
(deftest 459 (teo (let ((2 #Z(1))) nil)) 66)
(deftest 460 (teo (let ((a #Z(1) nil)) nil)) 66)

(deftest 461 (teo (funcall #'(lambda (a b) (car a)) #Z(1))) 67)
(deftest 462 (teo (funcall #'(lambda (a) (car a)) #Z(1) #Z(2))) 67)

(deftest 463 (te (encapsulated foo (collect-fn t #'f #'cons #Z(1 2 3)))) 68)

(deftest 464 (te (encapsulated #'foo (collecting-fn t #'f #'cons #Z(1 2 3)))) 69)

(deftest 465 (te (map-fn #'car #Z(1 2 3))) 70)

(deftest 466 (teo (defun ff (&foo b)
		    (declare (optimizable-series-function))
		    (car a))) 71)
(deftest 467 (te (defun ff (a &rest b)
		   (declare (optimizable-series-function))
		   (list a b))) 71)
(deftest 468 (te (defun ff (a &allow-other-keys b)
		   (declare (optimizable-series-function))
		   (list a b))) 71)

(deftest 469 (teo (defun ff ((a) b)
		    (declare (optimizable-series-function))
		    (car a))) 72)
(deftest 470 (teo (defun ff (t b)
		    (declare (optimizable-series-function))
		    (car a))) 72)
(deftest 471 (teo (defun ff (nil b)
		    (declare (optimizable-series-function))
		    (car a))) 72)

(deftest 472 (te (producing (x) (y z) (print y))) 73)

(deftest 473 (te (producing () (y z) (loop (tagbody (print y))))) 74)

(deftest 474 (te (producing (x) ((y #Z(1 2)) z) (loop (tagbody (setq z (next-in)))))) 75)

(deftest 475 (te (producing (x) ((y #Z(1 2)) z) (loop (tagbody (next-out z))))) 76)

(deftest 476 (te (collect (scan-range :upto 5 :below 6))) 77)

(deftest 477 (te (scan-multiple '(values list) '(1 2) '(3 4))) 78)

(deftest 478 (te (collect (latch #Z(1 2) :after 2 :before 3))) 79)

;These test warnings

(deftest 479 (tw (let ((x (scan '(1 2 3))))
		   (declare (integer x))
		   (collect x)))
  (1 2 3) 30)
(deftest 480 (tw (let ((x (scan '(1 2 3))) (y 3))
		   (declare (series y))
		   (collect (#M+ (series y) x))))
  (4 5 6) 31)
(deftest 481 (tw (defun ugh1 (a b)
		   (declare (optimizable-series-function))
		   (collect (#Mcons a (choose-if #'plusp b)))))
  ugh1 40)
(deftest 482 (tw (defun ugh2 (a b) "doc"
			(declare (optimizable-series-function) (off-line-port b) (integer a))
			(collect (#Mcons a b))))
  ugh2 41)
(deftest 483 (tw (defun ugh3 (a b)
		   (declare (optimizable-series-function))
		   (choose a b)))
  ugh3 42)
(deftest 484 (tw (defun ugh4 (a b)
		   (declare (optimizable-series-function) (off-line-port 0))
		   (collect (#Mcons a b))))
  ugh4 43)
(deftest 485 (tw (defun ugh44 (a)
		   (declare (optimizable-series-function))
		   (collect-sum (scan a))))
  ugh44 44)
;here temporarily not working, due to bug in code.
;(deftest 486 (tw (let ((e #Z(1 2 3))) (collect #Z(1 2))))
;  ((1 2) 52))
(deftest 487 (tw (let ((e #Z(1 2 3))) (declare (ignore e)) (collect e)))
  (1 2 3) 53)

;things that are half way from warnings to resriction violations.

(deftest 488 (tw (collect (phys-scan-list '(1 2 3))))
  (1 2 3) 28)
(deftest 489 (tw (let ((f #'(lambda (x) (collect-sum x))))
		   (let ((g (gatherer f)))
		     (next-out g 3)
		     (next-out g 4)
		     (result-of g)))) 7 28)

(deftest 490 (tw (block bar
		   (iterate ((x (series -1 2 3)))
		     (if (plusp x) (return-from bar x))))) 2 29)
#-allegro
(deftest 491 (tw (compiler-let ((*suppress-series-warnings* T))
		   (block bar
		     (iterate ((x (series -1 2 3)))
		       (if (plusp x) (return-from bar x)))))) 2 nil)

#+allegro
(deftest 491 (tw (cltl1::compiler-let ((*suppress-series-warnings* T))
		   (block bar
		     (iterate ((x (series -1 2 3)))
		       (if (plusp x) (return-from bar x)))))) 2 nil)

;These test restriction violation checks

(deftest 492 (tr (let ((*print-length* 2) (x #Z(1 2 3 4 5)))
		   (declare (special *print-length*))
		   (collect x)))
  (1 2 3 4 5) 1)
(deftest 493 (tr (let ((*print-length* 2) (x #Z(1 2 3 4 5)))
		   (declare (off-line-port 2))
		   (collect x)))
  (1 2 3 4 5) 1)
;test 494 removed
(deftest 495 (tr (progn (eval '(defun zzt (x) "doc"
				      (declare (optimizable-series-function 2) (special x))
				      (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 1)
(deftest 496 (tr (progn (eval '(defun zzt (x) "doc"
				      (declare (optimizable-series-function 2)
					       (propagate-alterability x y))
				      (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 1)
(deftest 497 (tr (let ((x #Z(1 2 3)))
		   (declare (optimizable-series-function))
		   (collect x)))
  (1 2 3) 1)

(deftest 498 (tr (let ((x '(values t t)))
		   (multiple-value-bind (a b)
		       (map-fn x #'(lambda (z) (values (- z) z)) #Z(1 2 3))
		     (list (collect a) (collect b)))))
  ((-1 -2 -3) (1 2 3)) 2)

(deftest 499 (tr (let ((x 2))
		   (multiple-value-bind (a b)
		       (chunk x 2 #Z(a b c d e f))
		     (list (collect a) (collect b)))))
  ((a c e) (b d f)) 3)

(deftest 500 (tr (let ((x 2))
		   (multiple-value-bind (a b)
		       (chunk 2 x #Z(a b c d e f))
		     (list (collect a) (collect b)))))
  ((a c e) (b d f)) 4)

(deftest 501 (tr (let ((l (list 1 2 3)))
		   (alter (phys-scan-list l) #Z(a b))
		   l))
  (a b 3) 5)

(deftest 502 (tr (let ((x #Z(1)))
		   (flet ((a (b) (car b)))
		     (a (collect x))))) 1 6)

(deftest 503 (tr (multiple-value-bind (x y) (values #Z(1 2 3) #Z(4 5))
		   (list (collect x) (collect y))))
  ((1 2 3) (4 5)) 7)

(deftest 504 (tr (not (let ((x #Z(1 2)))
			(#M1+ x)))) nil 10)

(deftest 505 (tr (let ((x (scan '(1 2))))
		   (setq xx x)
		   (collect-sum x))) 3 11)

(deftest 506 (tr
	       (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		 (list (collect (#M(lambda (y) (setq z (#M1+ z)) (1+ y)) z))
		       a (collect z) b))) ((2 3) 3 (3 4) (1 2)) 12)

(deftest 507 (tr (let ((x #Z(1 2 3)))
		   (if (null x) 10 20))) 20 13)

(deftest 508 (tr (progn (eval '(defun zzt (x)
				 (declare (optimizable-series-function 2))
				 (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 14)

(deftest 509 (tr
	       (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		 (when (not a) (setq a #Z(9 8)))
		 (list (collect a) (collect z) b))) ((9 8) (1 2) (1 2)) 20)
(deftest 510 (tr (let ((x #Z(1 2)))
		   (list (if T 3 (scan-range :upto 3))
			 (collect x)))) (3 (1 2)) 20)
(deftest 511 (tr (let ((x #Z(1 2)))
		   (if T (collect-sum #Z(2 3)) x))) 5 20)

(deftest 512 (tr (let* ((e #Z(1 2))
			(w (collect e)))
		   (collect (#M(lambda (x) (cons x w)) e)))) ((1 1 2) (2 1 2)) 21)
(deftest 513 (tr (let* ((e #Z((1) (2)))
			(w (collect e)))
		   (collect (#M(lambda (x) (cons (car x) w)) e))))
  ((1 (1) (2)) (2 (1) (2))) 21)
(deftest 514 (tr (let* ((e #Z(1 2))
			(w (collect e))
			(x (collect-sum e)))
		   (list (collect (#M(lambda (z) (list z x)) e))
			 (collect (#M(lambda (z) (list* z w)) e)))))
  (((1 3) (2 3)) ((1 1 2) (2 1 2))) 21)

(deftest 515 (tr (let* ((e #Z(1 -2 3))
			(w (split-if e #'plusp)))
		   (collect (#Mlist e w)))) ((1 1) (-2 3)) 22)
(deftest 516 (tr (let* ((e #Z(1 -2 3))
			(w (split-if e #'plusp)))
		   (collect (#Mlist e e w)))) ((1 1 1) (-2 -2 3)) 22)
(deftest 517 (tr (let* ((e #Z(1 -2 -4 3)))
		   (multiple-value-bind (w x) (split-if e #'plusp)
		     (collect (#Mlist w x))))) ((1 -2) (3 -4)) 22)

(deftest 518 (tr (let* ((e #Z(1 -2 -4 3))
			(w (choose-if #'plusp e)))
		   (collect (#Mlist e w)))) ((1 1) (-2 3)) 23)
(deftest 519 (tr (let* ((e #Z(1 -2 -4 3))
			(w (choose-if #'plusp e)))
		   (collect (#Mlist e e w)))) ((1 1 1) (-2 -2 3)) 23)
(deftest 520 (tr (let* ((e #Z(1 2)))
		   (collect (catenate e e)))) (1 2 1 2) 23)
(deftest 521 (tr (let* ((e #Z(1 2)))
		   (collect (#Mlist e (catenate e e))))) ((1 1) (2 2)) 23)
(deftest 522 (tr (let* ((e #Z(1 -2 -3 4)))
		   (collect (#Mlist e (catenate (choose-if #'plusp e)
						(choose-if #'minusp e))))))
  ((1 1) (-2 4) (-3 -2) (4 -3)) 23)
(deftest 523 (tr (let* ((e #Z(1 -2 -3 4)))
		   (multiple-value-bind (w x) (split-if e #'plusp)
		     (collect (#Mlist e (catenate w x))))))
  ((1 1) (-2 4) (-3 -2) (4 -3)) 23)

; tests due to bugs found and extensions made from 1/1/90 to 3/20/91

(deftest 524 (ton (let ((x 3))
		    (list (collect-last
			   (mapping ((i (scan-range :upto x)))
			     (setq x 4)
			     i))
			  x)))
  (3 4))

(deftest 525 (ton (let ((oddp #Z(1 2 3)))
		    (collect (choose-if #'oddp oddp))))
  (1 3))
(deftest 526 (ton (collect (choose-if #'(lambda (x) (let ((y (1+ x))) (evenp y))) #Z(1 2 3))))
  (1 3))
(deftest 527 (ton (collect (scan-lists-of-lists-fringe '(1 (1 2) (2 3))
						       #'(lambda (x) (let ((y (car x))) (evenp y))))))
  (1 1 2 (2 3)))
(deftest 528 (ton (collect (scan-range :upto ((lambda (x) (let ((y (1+ x))) (* 2 y))) 1))))
  (0 1 2 3 4))

(deftest 529 (ton (let ((x #Z(1 2 3)))
		    (list (collect-sum x) (collect-sum (catenate x #Z(4 5)))))) (6 15))
(deftest 530 (ton (let ((x (scan-range)))
		    (list (collect-sum (subseries x 0 3)) (collect-sum (subseries x 0 5))))) (3 10))
(deftest 531 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum (subseries x+ 0 2)) (collect-sum x-)))) (4 -12))
(deftest 532 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum x-) (collect-sum (subseries x+ 0 2))))) (-12 4))
(deftest 533 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum x+) (collect-first x-)))) (9 -2))

(deftest 534 (tr (let ((x #Z(1 2 3))
		       (g (generator (scan '(1 2 3)))))
		   (list (collect-sum x) (next-in g))))
  (6 1) 24)

(deftest 535 (td (defun zzz1 (x)
		   (declare (optimizable-series-function))
		   (scan-fn t #'(lambda () 10)
			    #'(lambda (z) (funcall #'(lambda (z) (- z x)) z))
			    #'zerop))
		 (collect (zzz1 2)))
  (10 8 6 4 2))

(deftest 536 (tm (collect (car (scan '((1)(2)(3)))))) (1 2 3))
(deftest 537 (tm (let ((*x* 0))
		   (collect (progn (list #Z(1 2 3) (incx)))))) ((1 0) (2 0) (3 0)))
(deftest 538 (tm (let ((*x* 0))
		   (collect (list #Z(1 2 3) (catenate #Z(a) (incx))))))
	 ((1 a) (2 0) (3 0)))
(deftest 539 (tm (let ((e #Z(1 2 3 4)))
		   (collect (choose (evenp e) e)))) (2 4))
(deftest 540 (tm (let ((x #Z(1 nil (a) 3 4)))
		   (collect (and (numberp x) (oddp x)))))
	 (t nil nil t nil))
(deftest 541 (tm (let ((*x* 1))
		   (let* ((x #Z(1 nil (a) 3 4)) (z (list x)))
		     (when (null x) (incx))
		     (collect (if (numberp x) *x* z)))))
	 (1 (nil) ((a)) 2 2))
(deftest 542 (tm (let ((*x* 0))
		   (let* ((x (car (scan '((1) (2) (3)))))
			  (y (1+ x))
			  (z (collect-sum (* x y))))
		     (incx (list x y 4))
		     (incx z)
		     (list (collect (list x (catenate #Z(a) (incx 'b)))) *x*))))
	 (((1 a) (2 b) (3 b)) 5))

(deftest 543 (ton (let* ((x 3) (e (make-series x))) (collect e))) (3))

;Here, temporarily not working because SETQ processing
;assumes the straight-line restriction is satisfied.
;(deftest 543.1 (tm (let* ((x #Z(1 nil (a) 3 4)) (y (+ 0 1)) (z (list x)))
;		     (when (null x) (setq y (1+ y)))
;		     (collect (if (numberp x) y z))))
;  (1 (nil) ((a)) 2 2))

;Here, note this is an example where the optimization is not correctness
;preservering due to side-effects.  Also note that more work happens than
;you might think in the optimized expression in any case.
;Here if not testing mapping, you get an odd message about non-series to series dflow
;before you get the not-straight line error message.
;(deftest 543.2 (let ((*x* 0))
;		 (let ((e #Z(1 2 3 4)))
;		   (list (if (numberp *x*) (collect-sum e) (collect-sum (#Mincx e)))
;			 *x*)))
;  (10 4))
;Here also note this even more extreme case.  It would work right if the
;whole inner-let were nested in where it could be.
;(deftest 543.3 (tm (let ((*x* 0))
;		     (let ((e #Z(1 2 3 4)))
;		       (list (if (numberp *x*) 0 (collect-sum (#Mincx e)))
;			     *x*))))
;  (0 4))

(deftest 544 (ton (let ((a #'car)) (funcall (or a a) '(1 2)))) 1)

;here temporarily not working
;(deftest 544.1 (t (let ((e #Z(a b)))
;		    (list (collect (compiler-let ((*c1* 3) *c2*)
;				     (#Mc1-c2-macro e)))
;			  (collect (compiler-let ((*c2* 4))
;				     (#Mc1-c2-macro e))))))
;  (((3 nil a) (3 nil b)) ((1 4 a) (1 4 b))))

(deftest 545 (ton (let ((mask (mapping ((a (scan '(a b c))))
				a)))
		    (collect mask))) (a b c))
(deftest 546 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (until-if #'(lambda (obj) (eql obj end)) data))))
  (1 2))
(deftest 547 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (choose-if #'(lambda (obj) (eql obj end)) data))))
  (3))
(deftest 548 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (split-if data #'(lambda (obj) (eql obj end))))))
  (3))

; Additional tests that only work on symbolics.

 #+(or cmu symbolics)
(deftest 549 (td (defun foo3 (number)
		   (declare (integer number))
		   (1+ number))
		 (collect (scan-range :below (foo3 4))))
  (0 1 2 3 4))
 #+(or cmu symbolics)
(deftest 550 (ton (collect (mapping ((x #Z(1 2)))
			     (do ((i 1 (1+ i))
				  a (b) (c 0)
				  (r 0 (+ x r)))
				 ((> i x) r)
			       (setq b i a i)
			       (if (> (+ a b c) 100) (return nil))))))
  (1 4))
 #+(or cmu symbolics)
(deftest 551 (tw (defun ugh6 (a)
		   (declare (optimizable-series-function) (ignore a))
		   (scan a)))
  ugh6 51)
;here temporarily not working, due to bug in the code.
; #+symbolics
;(deftest 552 (tw (defun ugh5 (a)
;		    (declare (optimizable-series-function))
;		    (scan '(1 2 3))))
;  ugh5 50)

(deftest 553 (ton (collect 'string #Z(#\B #\A #\R))) "BAR")
(deftest 554 (ton (collect 'simple-string #Z(#\B #\A #\R))) "BAR")
(deftest 555 (ton (collect 'base-string #Z(#\B #\A #\R))) "BAR")
(deftest 556 (ton (collect 'simple-base-string #Z(#\B #\A #\R))) "BAR")
(deftest 557 (ton (collect 'bit-vector #Z(1 0 1 1))) #*1011)
(deftest 558 (ton (collect 'simple-bit-vector #Z(1 0 1 1))) #*1011)

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.
;------------------------------------------------------------------------

