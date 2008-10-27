;-*- Syntax:COMMON-LISP -*-

;;;; This is the November, 26 1991 version of
;;;; the testing file for Richard C. Waters' Series macro package.

;;;;------------------------------------------------------------------------
;;;; Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.
;;;;
;;;; Permission to use, copy, modify, and distribute this software and
;;;; its documentation for any purpose and without fee is hereby
;;;; granted, provided that this copyright and permission notice
;;;; appear in all copies and supporting documentation, and that the
;;;; name of M.I.T. not be used in advertising or publicity pertaining
;;;; to distribution of the software without specific, written prior
;;;; permission. M.I.T. makes no representations about the suitability
;;;; of this software for any purpose.  It is provided "as is" without
;;;; express or implied warranty.
;;;;
;;;;     M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
;;;;     INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;;;     FITNESS, IN NO EVENT SHALL M.I.T. BE LIABLE FOR ANY SPECIAL,
;;;;     INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
;;;;     RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;;     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;;     ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
;;;;     OF THIS SOFTWARE.
;;;;
;;;; ------------------------------------------------------------------------
;;;;
;;;; To test Series just load this file and run the function
;;;; (DO-TESTS). It prompts you for the name of a scratch file to use
;;;; when testing. You must type a string "..."  containing the path
;;;; name.  It then prints out identifying numbers of tests as it
;;;; performs one test after another.  When all of the tests have been
;;;; run a summary line is printed saying how many tests failed.  This
;;;; file includes a copy of the RT regression tester.  (See the
;;;; Documentation for RT in Lisp Pointers Volume 4 number 2 1991 for
;;;; a full description of the features of RT.)  The file previously
;;;; used an older tester.  For easy comparison of results, all of the
;;;; old tests are given numerical names that match the numbers
;;;; printed out when running the old tester.
;;;;
;;;; $Id: s-test.lisp,v 1.27 2008/10/27 14:24:53 rtoy Exp $
;;;;
;;;; $Log: s-test.lisp,v $
;;;; Revision 1.27  2008/10/27 14:24:53  rtoy
;;;; Support SCL.  Just add scl conditionalizations where we have cmucl
;;;; ones, and convert uppercase symbols and symbol-names to use
;;;; symbol-name and uninterned symbols.  This is to support scl's default
;;;; "modern" mode.
;;;;
;;;; Changes from Stelian Ionescu.
;;;;
;;;; Revision 1.26  2007/07/10 17:45:46  rtoy
;;;; s-code.lisp:
;;;; o Add an optimizer for SERIES and update appropriately for the normal
;;;;   path and the optimized path.  This is needed so that (series t nil)
;;;;   returns #z(t nil t nil ...) instead of #z(list t nil list t nil ...)
;;;;
;;;; s-test.lisp:
;;;; o Add two tests for SERIES.  The tests need some work, but are based
;;;;   on the errors reported by Szymon 'tichy' on comp.lang.lisp on Jul 7,
;;;;   2007.
;;;;
;;;; Revision 1.25  2005/01/27 04:19:33  rtoy
;;;; Fix for bug 434120.
;;;;
;;;; s-code.lisp:
;;;; o scan* should initialize the index to -1 instead of 0, to keep in
;;;;   step with scan.
;;;;
;;;; s-test.lisp:
;;;; o Add test from the bug report.
;;;;
;;;; Revision 1.24  2005/01/26 18:37:39  rtoy
;;;; Fix bug reported by Dirk Gerrits, series-users, 2005-01-16.
;;;;
;;;; s-code.lisp:
;;;; o ALTER was not handling some cases where the frag had multiple
;;;;   ALTERABLE forms that matched the var.  Adjust ALTER so that all
;;;;   matching alterable forms are placed in the body.  This only works
;;;;   for optimized series.  Unoptimized series still has the bug.
;;;;
;;;; s-test.lisp:
;;;; o Add :if-exists :supersede when opening files for output.
;;;; o Add a test for the ALTER bug reported by Dirk Gerrits.
;;;;
;;;; Revision 1.23  2004/12/15 17:18:57  rtoy
;;;; Apply fixes from Hannu Koivisto to support sbcl.  Also added asdf
;;;; support.  His comments:
;;;;
;;;;
;;;; 	* series.asd:
;;;; 	  * Initial checkin.
;;;; 	* series.system:
;;;; 	  * Removed logical pathname stuff and made this "self-sufficient", i.e. it is
;;;; 	    sufficient to just load it; no need to edit pathname translations.
;;;; 	  * Removed s-install from series system; we certainly don't want Series to
;;;; 	    install itself to CL-USER whenever the system is compiled/loaded.
;;;;
;;;; 	* s-test.lisp:
;;;; 	  * Replaced all uses of defconstant with series::defconst-once.
;;;;
;;;; 	* s-package.lisp:
;;;; 	  * sb-cltl2 module is now required at compile time too.
;;;;
;;;; 	* s-code.lisp:
;;;; 	  * (defconst-once) New macro.
;;;; 	  * Replaced all uses of defconstant with it.
;;;;
;;;; 	* RELEASE-NOTES:
;;;; 	  * Installation instructions based on system definition files.
;;;; 	  * Updated the list of contributors.
;;;; 	  * Some cosmetic changes.
;;;;
;;;; Revision 1.22  2002/12/12 04:28:50  rtoy
;;;; Add another test.  (Forgot exactly why this is here, but it's a good
;;;; test of the macrolet walker for Clisp.)
;;;;
;;;; Revision 1.21  2001/12/23 16:54:44  rtoy
;;;; Make series support Allegro "modern" lisp with its case-sensitive
;;;; reader.  Mostly just making every that needs to be lower case actually
;;;; lower case.  The tests still work.
;;;;
;;;; Revision 1.20  2001/08/24 14:10:17  rtoy
;;;; Reinstate test 466, but change the bogus &foo to &key.  We're testing
;;;; for unsupported lambda options.
;;;;
;;;; Revision 1.19  2001/08/23 22:21:59  rtoy
;;;; o Comment out test 466 because I don't understand what this is really
;;;;   supposed to test.
;;;; o Update test 491 for Clisp version 2.27 which has compiler-let in the
;;;;   EXT package.
;;;; o Tests 549-551 should apply to all lisps, not just Symbolics.
;;;;
;;;; Revision 1.18  2000/09/22 16:01:43  rtoy
;;;; o Rainer Joswig points out that in several places we use lisp:let.
;;;;   Make that common-lisp:let.
;;;; o Rainer also points out we reference the package USER.  Make that
;;;;   COMMON-LISP-USER.
;;;; o We want to be in the COMMON-LISP-USER package for all platforms.
;;;; o Test 491 was failing because it didn't have access to compiler-let.
;;;;   Add a version for CLISP.
;;;;
;;;; Revision 1.17  2000/09/05 15:54:57  rtoy
;;;; Add test to catch bug 113625:  scan doesn't scan constants very well.
;;;;
;;;; Revision 1.16  2000/06/26 18:17:14  rtoy
;;;; Add a test to catch the bug that (collect 'vector (scan '(1 2 3)))
;;;; was returning the result in reverse order.
;;;;
;;;; Revision 1.15  2000/03/28 10:23:49  matomira
;;;; polycall et all are now tail recursive.
;;;; LETIFICATION WORKS COMPLETELY!!
;;;;
;;;; Revision 1.17  2000/03/17 19:24:24  matomira
;;;; MERGE-FRAGS no longer depends on frag component order.
;;;; purity component of frag is now just a symbol.
;;;; Abstracted use of prolog component of frags.
;;;; Prolog letification almost works. Need to adapt MERGE-FRAGS still.
;;;;
;;;; Revision 1.16  2000/03/15 18:40:36  matomira
;;;; LOCALLY and letification works.
;;;;
;;;; Revision 1.15  2000/03/15 09:05:41  matomira
;;;; Temporary NULL-OR wrap for some declarations.
;;;;
;;;; Revision 1.14  2000/03/14 10:48:10  matomira
;;;; Workaround for ACL 5.0.1 TAGBODY bug added.
;;;; ALL-TIME SERIES BUG FIX: wrappers now inserted more precisely.
;;;; Abstracted use of wrapper component of frags.
;;;; GENERATOR deftyped to CONS, not LIST, when necessary.
;;;;
;;;; Revision 1.13  2000/03/03 19:17:15  matomira
;;;; Series 2.0 - Change details in RELEASE-NOTES.
;;;;
;;;; Revision 1.11  2000/02/09 22:52:00  toy
;;;; Fernando made these changes:  Replace deftest with defok,
;;;; defcmumismatch, and defcmukernel if some older versions of CMUCL
;;;; produce bad results on these tests.  Newer versions of CMUCL pass
;;;; these without problems.
;;;;
;;;; Revision 1.10  1999/07/02 19:46:19  toy
;;;; Select the right package for clisp (maybe).
;;;;
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

(series::eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :common-lisp-user)

  #+(and :allegro-version>= (version>= 5 0) (not (version>= 5 1)))
  (defadvice make-sequence :before
    (cond ((eql (car excl:arglist) 'base-string)
	   (setf (car excl:arglist) '(vector base-char)))
	  ((eql (car excl:arglist) 'simple-base-string)
	   (setf (car excl:arglist) '(simple-array base-char (*))))))

  (series::install)
) ; end of eval-when

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

(defmacro edeftest (name form &rest values)
  `(add-entry (list t ',name ',form ,@values)))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defmacro defok (&rest args)
  ;;'())
  `(deftest ,@args))

(defmacro defcmumismatch (&rest args)
  ;;'())
  `(deftest ,@args))

(defmacro defcmukernel (&rest args)
  ;;'())
  `(deftest ,@args))

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
	   #-(or cmu scl allegro)
	   (*break-on-warnings* t)
	   #+(or cmu scl allegro harlequin)
	   (*break-on-signals* #+(or cmu scl allegro) nil
			       #-(or cmu scl allegro) 'warning)
	   ;; Don't print out "Compiling..." messages
	   #+(or cmu scl)
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
	  (stream out :direction :output :if-exists :supersede)
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
  (cond ((y-or-n-p "continue") f)))

(defun o ()
  (setq series::*optimize-series-expressions*
	(not series::*optimize-series-expressions*)))

;This is the standard tester.
(defvar test-file nil)
(defvar *compile-tests* t)

(defmacro ton (form) `(test-opt-&-non-opt ',form))

(defun test-opt-&-non-opt (form)
  (loop (if test-file (return nil))
    (format t "~%Type a string representing a pathname of a scratch disk file: ")
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
	(series::*optimize-series-expressions* t))
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
	(series::*optimize-series-expressions* t)
	(series::*testing-errors* t))
    (setq series:*last-series-error* nil)
    (setq series:*last-series-loop* nil)
    (with-output-to-string (*error-output*)
      (setq v (eval (series::iterative-copy-tree form))))
    (values v (cadr series:*last-series-error*))))

(defmacro tr (form) `(test-rrs ',form))

(defun test-rrs (form)
  (let ((v nil)
	(series::*series-implicit-map* nil)
	(series::*testing-errors* t)
	(series::*optimize-series-expressions* t)
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
	 (series::*testing-errors* t)
	 (opt (catch :testing-errors (test-opt form)))
	 (non-opt (catch :testing-errors (test-non-opt form))))
   (if (equal non-opt opt) opt
       (list "opt and non-opt disagree" opt non-opt))))

(defmacro teo (form) `(test-ers-opt ',form))

(defun test-ers-opt (form)
  (setq series:*last-series-loop* nil)
  (let* ((series::*series-implicit-map* nil)
	 (series::*testing-errors* t))
    (catch :testing-errors (test-opt form))))

(defmacro tm (form) `(test-mapping ',form))

(defun test-mapping (form)
  (setq series:*last-series-loop* nil)
  (let ((series::*series-implicit-map* t)
        (*suppress-series-warnings* nil))
    (setq form (series::iterative-copy-tree form))
    (if *compile-tests*
	(funcall (compile nil `(lambda () ,form)))
	(eval form))))

(defun decls (arg) (declare (ignore arg)) (decls0 series:*last-series-loop*))
(defun decls0 (tree)
  (cond ((not (consp tree)) nil)
	((eq (car tree) 'declare) tree)
	(t (do ((l tree (cdr l))) ((not (consp l)) nil)
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

(defstruct test-struct)


;The first few pages of tests attempt to test each of the different
;series operations in the series function library.

(rem-all-tests)

;first are individual tests of all the exported series functions
(defok 0 (ton (collect #Z(a b c))) (a b c))
(defok 1 (ton (collect #Z())) ())

(defok 2 (ton (collect (#Mlist (series 'a0) #Z(a b)))) ((a0 a) (a0 b)))
(defok 3 (ton (collect (#1Mlist (series 'a0 'a1) #Z(a b c)))) ((a0 a) (a1 b) (a0 c)))

(defok 4 (ton (collect (make-series 'b 'c))) (b c))
(defok 5 (ton (collect (make-series 'b))) (b))

(defok 6 (ton (collect (#Mcar (scan-fn t #'(lambda () '(a b c)) #'cdr #'null)))) (a b c))
(defok 7 (ton (collect (#Mcar (scan-fn '(values t) #'(lambda () '(a b c)) #'cdr #'null))))
  (a b c))
(defok 8 (ton (collect (#Mcar (scan-fn t #'(lambda () '(a b c))
					 'cdr #'(lambda (x) (null x))))))
  (a b c))
(defok 9 (ton (collect (#M(lambda (x y) (list x (car y)))
			    #Z(a b c)
			    (scan-fn t #'(lambda () '(1 2)) #'cdr))))
  ((a 1) (b 2) (c nil)))
(defok 10 (ton (let* ((lst (list 'a 'b 'c)))
		   (multiple-value-bind (e l)
		       (scan-fn '(values t t) #'(lambda () (values (car lst) lst))
				#'(lambda (element parent)
				    (declare (ignore element))
				    (values (cadr parent) (cdr parent)))
				#'(lambda (element parent)
				    (declare (ignore element))
				    (null parent)))
		     (list (collect e) (collect l)))))
  ((a b c) ((a b c) (b c) (c))))

(defok 11 (ton (collect
		  (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				(scan-fn t #'(lambda () 0)
					 #'(lambda (sum)
					     (incf xx)
					     (+ sum xx))
					 #'(lambda (x) (> x 10)))))) (0 1 3 6 10))
(defok 12 (ton (multiple-value-bind (a b)
		     (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				   (scan-fn '(values t t)
					    #'(lambda () (values 0 1))
					    #'(lambda (sum prod)
						(incf xx)
						(values (+ sum xx) (* prod xx)))
					    #'(lambda (x y) (> (min x y) 10))))
		   (list (collect a) (collect b))))
  ((0 1 3 6 10) (1 1 2 6 24)))

(defok 13 (ton (collect (#Mcar (scan-fn-inclusive t #'(lambda () '(a b c)) #'cdr #'null))))
  (a b c nil))
(defok 14 (ton (collect (#Mcar (scan-fn-inclusive t #'(lambda () ()) #'cdr #'null)))) (nil))
(defok 15 (ton (let* ((lst (list 1 2 3 -4 5)))
		   (multiple-value-bind (e l)
		       (scan-fn-inclusive '(values t t) #'(lambda () (values (car lst) lst))
					  #'(lambda (element parent)
					      (declare (ignore element))
					      (values (cadr parent) (cdr parent)))
					  #'(lambda (element parent)
					      (declare (ignore parent))
					      (minusp element)))
		     (list (collect e) (collect l)))))
  ((1 2 3 -4) ((1 2 3 -4 5) (2 3 -4 5) (3 -4 5) (-4 5))))

(defok 16 (ton (collect
		  (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				(scan-fn-inclusive t #'(lambda () 0)
						   #'(lambda (sum)
						       (incf xx)
						       (+ sum xx))
						   #'(lambda (x) (> x 10))))))
  (0 1 3 6 10 15))
(defok 17 (ton (multiple-value-bind (a b)
		     (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				   (scan-fn-inclusive '(values t t)
						      #'(lambda () (values 0 1))
						      #'(lambda (sum prod)
							  (incf xx)
							  (values (+ sum xx) (* prod xx)))
						      #'(lambda (x y) (> (min x y) 10))))
		   (list (collect a) (collect b))))
  ((0 1 3 6 10 15) (1 1 2 6 24 120)))
(defok 18 (ton (collect (scan ()))) ())
(defok 19 (ton (let* ((x (list 'a 'b 'c)) (e (scan 'list x)))
		   (list (collect e) (alter e (scan-range)) (collect e) x)))
  ((a b c) nil (a b c) (0 1 2)))
(defok 20 (ton (collect (scan 'vector '#()))) ())
(defok 21 (ton (let* ((v (copy-seq "FOO")) (e (scan 'vector v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(defok 22 (ton (let* ((v (copy-seq "FOO")) (e (scan 'sequence v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(defok 23 (ton (let* ((v (copy-seq '(1 2 3))) (e (scan 'sequence v)))
		   (list (collect e) (alter e (series 0)) v)))
  ((1 2 3) nil (0 0 0)))
(defok 24 (ton (let* ((type 'string) (v (copy-seq "FOO")) (e (scan type v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---"))
(defok 25 (ton (let* ((v (copy-seq "FOOBAR")) (e (scan '(simple-vector 3) v)))
		   (list (collect e) (alter e (series #\-)) v)))
  ((#\F #\O #\O) nil "---BAR"))

(defok 26 (ton (multiple-value-bind (a b) (scan-multiple 'list '(1 2 3) '(3 4 5 6))
		   (list (collect a) (collect b))))
  ((1 2 3) (3 4 5)))
(defok 27 (ton (multiple-value-bind (a b)
		     (scan-multiple '(values vector list) '#(1 2 3) '(3 4))
		   (list (collect a) (collect b))))
  ((1 2 3) (3 4 nil)))
(defok 28 (ton (collect (until-if #'null (scan-multiple (cadr '(values list)) '(1 2 nil 3)))))
  (1 2))
(defok 29 (ton (let* ((x (list 'a 'b nil 'c))
			(e (scan-multiple 'list x)))
		   (list (collect e) (alter e (scan-range)) x)))
  ((a b nil c) nil (0 1 2 3)))
(defok 30 (ton (let* ((x (list 'a 'b nil 'c))
			(e (until-if #'null (scan-multiple 'list x))))
		   (list (collect e) (alter e (scan-range)) (collect e) x)))
  ((a b) nil (a b) (0 1 nil c)))

(defok 31 (ton (collect (scan-sublists '(a b c)))) ((a b c) (b c) (c)))
(defok 32 (ton (collect (scan-sublists ()))) ())

(defok 33 (ton (collect (#Mlist (scan-range) #Z(a b c)))) ((0 a) (1 b) (2 c)))
(defok 34 (ton (collect (#Mlist (scan-range :from 4 :by 3) #Z(a b c))))
  ((4 a) (7 b) (10 c)))
(defok 35 (ton (collect (scan-range :upto 3))) (0 1 2 3))
(defok 36 (ton (collect (scan-range :below 3))) (0 1 2))
(defok 37 (ton (collect (scan-range :length 3))) (0 1 2))
(defok 38 (ton (collect (scan-range :from 2 :upto 3))) (2 3))
(defok 39 (ton (collect (scan-range :from 2 :below 3))) (2))
(defok 40 (ton (collect (scan-range :from 2 :length 3))) (2 3 4))
(defok 41 (ton (collect (scan-range :from 4 :upto 3))) ())
(defok 42 (ton (collect (scan-range :from 4 :below 3))) ())
(defok 43 (ton (collect (scan-range :from 4 :length 3))) (4 5 6))
(defok 44 (ton (collect (scan-range :upto 3 :by 2))) (0 2))
(defok 45 (ton (collect (scan-range :upto 4 :by 2))) (0 2 4))
(defok 46 (ton (collect (scan-range :below 3 :by 2))) (0 2))
(defok 47 (ton (collect (scan-range :below 4 :by 2))) (0 2))
(defok 48 (ton (collect (scan-range :length 3 :by 2))) (0 2 4))
(defok 49 (ton (collect (#M(lambda (x) (round (* 10. x)))
			     (scan-range :from 1.5 :by .2 :below 2.0))))
  (15 17 19))
(defok 50 (ton (collect (#Mlist (scan-range :from 4 :by -3) #Z(a b c))))
  ((4 a) (1 b) (-2 c)))
(defok 51 (ton (collect (scan-range :by -1 :downto -3))) (0 -1 -2 -3))
(defok 52 (ton (collect (scan-range :by -1 :above -3))) (0 -1 -2))
(defok 53 (ton (collect (scan-range :by -1 :length 3))) (0 -1 -2))
(defok 54 (ton (collect (scan-range :from 4 :by -1 :downto 3))) (4 3))
(defok 55 (ton (collect (scan-range :from 4 :by -1 :above 3))) (4))
(defok 56 (ton (collect (scan-range :from 4 :by -1 :length 3))) (4 3 2))
(defok 57 (ton (collect (scan-range :downto -3 :by -2))) (0 -2))
(defok 58 (ton (collect (scan-range :downto -4 :by -2))) (0 -2 -4))
(defok 59 (ton (collect (scan-range :above -3 :by -2))) (0 -2))
(defok 60 (ton (collect (scan-range :above -4 :by -2))) (0 -2))
(defok 61 (ton (collect (scan-range :length 3 :by -2))) (0 -2 -4))

(defok 62 (ton (collect (scan-lists-of-lists '(1 (2 3) 4)))) ((1 (2 3) 4) 1 (2 3) 2 3 4))
(defok 63 (ton (collect (scan-lists-of-lists '(1 (2 3) 4) #'atom)))
  ((1 (2 3) 4) 1 (2 3) 2 3 4))
(defok 64 (ton (collect (scan-lists-of-lists '(1 (2 3) 4)
					       #'(lambda (n) (not (and (consp n) (cddr n)))))))
  ((1 (2 3) 4) 1 (2 3) 4))
(defok 65 (ton (collect (scan-lists-of-lists nil))) (nil))

(defok 66 (ton (collect (scan-lists-of-lists-fringe '((1 2 ((3 . 4) 4) (5) () (((6))))))))
  (1 2 3 4 5 nil 6))
(defok 67 (ton (collect (scan-lists-of-lists-fringe '(1 2 ((3 . 4) 4) (5) () (((6))))
						      #'(lambda (n) (not (and (consp n) (cdr n)))))))
  (1 2 3 4 (5) nil (((6)))))
(defok 68 (ton (collect (scan-lists-of-lists-fringe '((2) (nil))
						      #'(lambda (e) (numberp (car e))))))
  ((2) nil))

(defok 69 (ton (collect (scan-lists-of-lists-fringe ()))) (nil))
(defok 70 (ton (let ((tree (list (list 3) 4)))
		   (let ((leaf (choose-if #'evenp (scan-lists-of-lists-fringe tree))))
		     (alter leaf (#M- leaf)))
		   tree)) ((3) -4))
(defok 71 (ton (let ((z (list 'a 'b (cons 3 'e) 'd)))
		   (let* ((x (scan-lists-of-lists-fringe z)))
		     (alter x (#Mlist x)))
		   z)) ((a) (b) ((3) . e) (d)))

(defok 72 (ton (collect (scan-alist '((1 . a) () (2) (1 . c))))) (1 2))
(defok 73 (ton (collect (scan-alist ()))) ())
(defok 74 (ton (multiple-value-bind (key value)
		     (scan-alist '((1 . a) () (2) (1 . c)))
		   (collect (#Mlist key value)))) ((1 a) (2 nil)))
(defok 75 (ton (let ((alist (list (cons 'a  1) (cons 'b 2))))
		   (multiple-value-bind (key val) (scan-alist alist)
		     (alter key (#Mlist key))
		     (alter val (#Mlist val)))
		   alist)) (((a) . (1)) ((b) . (2))))

(defok 76 (ton (collect (scan-plist '(P1 1 P2 2 P1 3 P3 4)))) (P1 P2 P3))
(defok 77 (ton (collect (scan-plist ()))) ())
(defok 78 (ton (multiple-value-bind (key value) (scan-plist '(P1 1 P2 2 P1 3))
		   (collect (#Mlist key value)))) ((P1 1) (P2 2)))
(defok 79 (ton (let ((plist (list 'a 1 'b 2)))
		   (multiple-value-bind (key val) (scan-plist plist)
		     (alter key (#Mlist key))
		     (alter val (#Mlist val)))
		   plist)) ((a) (1) (b) (2)))

(defok 80 (ton (multiple-value-bind (key val)
		     (scan-hash (let ((x (make-hash-table)))
				  (setf (gethash 'color x) 'brown)
				  (setf (gethash 'name x) 'fred)
				  x))
		   (sort (collect (#Mcons key val))
			 #'(lambda (x y) (string-lessp (string (car x)) (string (car y)))))))
  ((color . brown) (name . fred)))

(defok 81 (ton (progn (collect-first (scan-symbols)) nil)) nil) ;grotesquely weak tests
(defok 82 (ton (progn (collect-first (scan-symbols (find-package :series))) nil)) nil)

;scan-file tested in conjunction with collect-file.

(defok 83 (ton (collect (previous #Z(a b c)))) (nil a b))
(defok 84 (ton (collect (previous #Z(a b c) 'fill 2))) (fill fill a))
(defok 85 (ton (collect (previous #Z(a b c) 0))) (0 a b))

(defok 86 (ton (collect (latch #Z(nil 3 nil 4 5)))) (nil 3 nil nil nil))
(defok 87 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2))) (nil 3 nil 4 nil))
(defok 88 (ton (collect (latch #Z(nil 3 nil 4 5) :after 0))) (nil nil nil nil nil))
(defok 89 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :pre 'a))) (a a a a 5))
(defok 90 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :pre 'a :post 'b)))
  (a a a a b))
(defok 91 (ton (collect (latch #Z(nil 3 nil 4 5) :after 2 :post 'b))) (nil 3 nil 4 b))
(defok 92 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2))) (nil 3 nil nil nil))
(defok 93 (ton (collect (latch #Z(nil 3 nil 4 5) :before 0))) (nil nil nil nil nil))
(defok 94 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :pre 'a))) (a a a 4 5))
(defok 95 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :pre 'a :post 'b)))
  (a a a b b))
(defok 96 (ton (collect (latch #Z(nil 3 nil 4 5) :before 2 :post 'b))) (nil 3 nil b b))

(defok 97 (ton (collect (until #Z(nil nil t nil t) #Z(1 2 3)))) (1 2))
(defok 98 (ton (multiple-value-bind (x y)
		     (until #Z(nil nil t nil t) #Z(1 2 3) #Z(a b c d))
		   (list (collect x) (collect y)))) ((1 2) (a b)))
(defok 99 (ton (multiple-value-bind (x y)
		     (until #Z(nil nil) #Z(1 2 3) #Z(a b c d))
		   (list (collect x) (collect y)))) ((1 2) (a b)))
(defok 100 (ton (multiple-value-bind (x y)
		      (until #Z(nil nil nil nil t) #Z(1 2 3) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2 3) (a b c)))
(defok 101 (ton (multiple-value-bind (x y)
		      (until #Z(nil nil nil nil t) #Z(a b c d) #Z(1 2 3))
		    (list (collect x) (collect y)))) ((a b c) (1 2 3)))
(defok 102 (ton (multiple-value-bind (x y z)
		      (until #Z(nil nil t nil t) #Z(a b c d) #Z(1 2 3) #Z(5 6 7))
		    (list (collect x) (collect y) (collect z)))) ((a b) (1 2) (5 6)))
(defok 103 (ton (collect (until #Z() #Z(1 2 3)))) ())
(defok 104 (ton (let ((x #Z(1 2 3 nil nil)))
		    (collect (until (previous (#Mnull x)) x))))
  (1 2 3 nil))

(defok 105 (ton (collect (until-if #'null #Z(1 2 3 nil nil)))) (1 2 3))
(defok 106 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(1 2 (3)) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2) (a b)))
(defok 107 (ton (let ((z #'listp))
		    (multiple-value-bind (x y)
			(until-if z #Z(1 2 (3)) #Z(a b c d))
		      (list (collect x) (collect y))))) ((1 2) (a b)))
(defok 108 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(1 2) #Z(a b c d))
		    (list (collect x) (collect y)))) ((1 2) (a b)))
(defok 109 (ton (multiple-value-bind (x y)
		      (until-if #'listp #Z(a b c d) #Z(1 2))
		    (list (collect x) (collect y)))) ((a b) (1 2)))
(defok 110 (ton (multiple-value-bind (x y z)
		      (until-if #'listp #Z(a b (c) d) #Z(1 2 3) #Z(5 6 7))
		    (list (collect x) (collect y) (collect z)))) ((a b) (1 2) (5 6)))
(defok 111 (ton (let ((fn #'null))
		    (collect (until-if fn #Z(1 2 3 nil nil))))) (1 2 3))
(defok 112 (ton (let ((v (list 1 -2 3)))
		    (let ((x (until-if #'minusp (scan v))))
		      (collect-sum x)
		      (alter x (#M- x)))
		    v)) (-1 -2 3))

(defok 113 (ton (collect (map-fn t #'list #Z(1 2 3)))) ((1) (2) (3)))
(defok 114 (ton (collect (map-fn 'list #'list #Z(1 2 3)))) ((1) (2) (3)))
(defok 115 (ton (collect (map-fn '(values list) #'list #Z(1 2 3)))) ((1) (2) (3)))
(defok 116 (ton (collect (map-fn '(values *) #'list #Z(1 2 3)))) ((1) (2) (3)))
(defok 117 (ton (collect (map-fn t 'list #Z(1 2 3)))) ((1) (2) (3)))
(defok 118 (ton (collect (map-fn t #'(lambda (z) (list z)) #Z(1 2 3)))) ((1) (2) (3)))
(defok 119 (ton (multiple-value-bind (a b)
		      (map-fn '(values integer integer) #'(lambda (x) (values x (1+ x)))
			      #Z(1 2 3))
		    (collect (#Mlist a b)))) ((1 2) (2 3) (3 4)))
(defok 120 (ton (let ((z 2))
		    (collect (map-fn t #'(lambda (x) (+ x z)) #Z(1 2 3))))) (3 4 5))
(defok 121 (ton (let ((z 2))
		    (collect (map-fn t #'(lambda (x) (+ x z)) #Z(1 2 3))))) (3 4 5))

(defok 122 (ton (collect (mapping ((e #Z(1 2 3))) (1+ e)))) (2 3 4))
(defok 123 (ton (collect (mapping (((e f) (scan-plist '(a 1 b 2))))
			     (cons e f)))) ((a . 1) (b . 2)))
(defok 124 (ton (collect (mapping ((d #Z(10 20 30 40))
				     ((e f) (scan-plist '(a 1 b 2))))
			     (list* d e f)))) ((10 a . 1) (20 b . 2)))

(defok 125 (ton (let ((c 1))
		    (collect (#Mcons #Z(a b c) (#M(lambda () (incf c)))))))
  ((a . 2) (b . 3) (c . 4)))
(defok 126 (ton (let* ((tt '((1 2) (3 4)))
			 (e (scan tt)))
		    (collect (#M(lambda (x y) (list (collect 'bag (scan x)) y)) e e))))
  (((2 1) (1 2)) ((4 3) (3 4))))
(defok 127 (ton (let ((e #Z((1 2) (3 4))))
		    (collect (#M(lambda (x) (collect-sum (scan x))) e)))) (3 7))

(defok 128 (ton (collect (collecting-fn t #'(lambda () 0) #'+ #Z(1 2 3)))) (1 3 6))
(defok 129 (ton (collect (collecting-fn 'integer #'(lambda () 0) #'+ #Z(1 2 3)))) (1 3 6))
(defok 130 (ton (collect (collecting-fn t #'(lambda () 0) '+ #Z(1 2 3)))) (1 3 6))
(defok 131 (ton (collect (collecting-fn t #'(lambda () 0) #'(lambda (s z) (+ s z)) #Z(1 2 3))))
  (1 3 6))
(defok 132 (ton (collect (collecting-fn '(values t t) #'(lambda () (values nil t))
					  #'(lambda (max flag n)
					      (values (if flag n (max max n)) nil))
					  #Z(1 4 2)))) (1 4 4))
(defok 133 (ton (collect 'list
			   (collecting-fn '(values list integer) #'(lambda () (values nil 0))
					  #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
					  #Z(a b c) #Z(1 2 3))))
  (((a 1 0)) ((b 2 1) (a 1 0)) ((c 3 2) (b 2 1) (a 1 0))))
(defok 134 (ton (collect (collecting-fn t #'(lambda () 0) #'- #Z(1 2 3)))) (-1 -3 -6))

(defok 135 (ton (multiple-value-bind (x y) (cotruncate #Z(1 2 3) #Z(4 5))
		    (list (collect-sum x) (collect-sum y)))) (3 9))
(defok 136 (ton (multiple-value-bind (x y z) (cotruncate #Z(1 2 3) #Z(4 5) #Z(9 8 7))
		    (list (collect-sum x) (collect-sum y) (collect-sum z)))) (3 9 17))
(defok 137 (ton (multiple-value-bind (x y) (cotruncate #Z() #Z(4 5))
		    (list (collect-sum x) (collect-sum y)))) (0 0))
(defok 138 (ton (multiple-value-bind (x y) (cotruncate #Z(1 2 3) #Z(4 5))
		    (list (collect-sum (#M+ x y)) (collect-sum y)))) (12 9))
(defok 139 (ton (let ((ll (list 1 2 3 4)))
		    (multiple-value-bind (x y) (cotruncate (scan ll) #Z(4 5))
		      (list (collect x) (alter x y) ll (collect-sum y)))))
  ((1 2) nil (4 5 3 4) 9))

(defok 140 (ton (let ((x '(b c)))
		    (collect (catenate (scan (cons 'a x)) #Z(1 2 3)))))
  (a b c 1 2 3))
(defok 141 (ton (collect (catenate #Z() #Z(a b c) #Z() #Z(a b c))))
  (a b c a b c))
(defok 142 (ton (let ((x #Z(1 2)) (y #Z(3 4)))
		    (collect (catenate x y)))) (1 2 3 4))

(defok 143 (ton (let ((x '(b c)))
		    (collect (subseries (scan (cons 'a x)) 1 2)))) (b))
(defok 144 (ton (collect (subseries #Z(a b c) 1))) (b c))
(defok 145 (ton (let ((v (list 1 -2 3)))
		    (let ((x (subseries (scan v) 1)))
		      (alter x (#M- x)))
		    v)) (1 2 -3))

(defok 146 (ton (collect (positions #Z(a nil 3 nil t nil)))) (0 2 4))
(defok 147 (ton (let ((x '(3 t nil)))
		    (collect (positions (scan  (cons nil x)))))) (1 2))
(defok 148 (ton (collect (positions #Z(nil nil)))) ())

(defok 149 (ton (collect (subseries (mask #Z()) 0 6))) (nil nil nil nil nil nil))
(defok 150 (ton (collect (subseries (mask #Z(0 2 4)) 0 6))) (t nil t nil t nil))

(defok 151 (ton (collect (mingle #Z(1 3 7 9) #Z(4 5 8) #'<))) (1 3 4 5 7 8 9))
(defok 152 (ton (collect (mingle #Z(4 5 8) #Z(1 3 7 9) #'<))) (1 3 4 5 7 8 9))
(defok 153 (ton (collect (mingle #Z((1 a) (2 b)) #Z((1 c) (3 d))
				   #'(lambda (x y) (< (car x) (car y))))))
  ((1 a) (1 c) (2 b) (3 d)))

(defok 154 (ton (collect (choose #Z(t t nil nil t) #Z(1 2 nil nil -4))))
  (1 2 -4))
(defok 155 (ton (collect (choose #Z(1 2 nil nil -4)))) (1 2 -4))
(defok 156 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (choose (#Mplusp x) x)))) (1 2))
(defok 157 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (#M(lambda (x) (if (plusp x) x)) x)))) (1 nil 2 nil))
(defok 158 (ton (let ((x #Z(1 -1 2 -2)))
		    (collect (#M(lambda (x) (if (plusp x) x (- x))) x)))) (1 1 2 2))
(defok 159 (ton (let ((x #Z(0 1 -1 2 -2)))
		    (collect (#Mlist (choose (#Mplusp x) x) (scan-range))))) ((1 0) (2 1)))
(defok 160 (ton (let ((x #Z(0 1 -1 2 -2))
			(tag (scan-range)))
		    (collect (#Mlist (choose (#Mplusp x) x) tag)))) ((1 0) (2 1)))
(defok 161 (ton (let* ((l (list 1 2 nil nil -4))
			 (e (choose #Z(t t nil nil t) (scan l))))
		    (list (collect e) (alter e (#Mlist e)) l)))
  ((1 2 -4) nil ((1) (2) nil nil (-4))))

(defok 162 (ton (collect (choose-if #'minusp #Z(1 2 -2 3 -4)))) (-2 -4))
(defok 163 (ton (let ((fn #'minusp))
		    (collect (choose-if fn #Z(1 2 -2 3 -4))))) (-2 -4))
(defok 164 (ton (let ((v (list 1 -2 3)))
		    (let ((x (choose-if #'minusp (scan v))))
		      (alter x (#M- x)))
		    v)) (1 2 3))

(defok 165 (ton (collect (expand #Z(nil t nil t nil) #Z(a b c))))
  (nil a nil b nil))
(defok 166 (ton (collect (expand #Z(nil t nil t) #Z(a b c) t))) (t a t b))

(defok 167 (ton (collect (spread #Z(1 1) #Z(2 4) -1))) (-1 2 -1 4))
(defok 168 (ton (collect (spread #Z(0 2 4) #Z(a b)))) (a nil nil b))
(defok 169 (ton (collect (spread #Z(1) #Z(a b)))) (nil a))

(defok 170 (ton (let* ((x #Z(1 -1 2 -2)))
		    (multiple-value-bind (y+ y-) (split x (series t nil t nil))
		      (list (collect x) (collect y+) (collect y-)))))
  ((1 -1 2 -2) (1 2) (-1 -2)))
(defok 171 (ton (let* ((x #Z(1 0 -1 2 0 -2)))
		    (multiple-value-bind (y+ y- y0) (split x (series t nil nil t nil nil)
							   (series nil nil t nil nil t))
		      (list (collect y+) (collect y-) (collect y0) (collect x)))))
  ((1 2) (-1 -2) (0 0) (1 0 -1 2 0 -2)))
(defok 172 (ton (let* ((l (list 1 -1 2 -2))
			 (x (scan l)))
		    (multiple-value-bind (y+ y-) (split x (series t nil t nil))
		      (list (collect x) (alter y+ (#Mlist y+)) (collect y+) (collect y-) l))))
  ((1 -1 2 -2) nil (1 2) (-1 -2) ((1) -1 (2) -2)))

(defok 173 (ton (let* ((x #Z(1 -1 2 -2)))
		    (multiple-value-bind (y+ y-) (split-if x #'plusp)
		      (list (collect x) (collect y+) (collect y-)))))
  ((1 -1 2 -2) (1 2) (-1 -2)))
(defok 174 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (collect (#M+ y+ y+))))
  (2 4))
(defok 175 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (list (collect y+) (collect-sum y+))))
  ((1 2) 3))
(defok 176 (ton (let* ((x #Z(1 -1 2 -2))
			 (y+ (split-if x #'plusp)))
		    (collect (catenate y+ #Z(5 6)))))
  (1 2 5 6))
(defok 177 (ton (let* ((x #Z(1 0 -1 2 0 -2)))
		    (multiple-value-bind (y+ y- y0) (split-if x #'plusp #'minusp)
		      (list (collect y+) (collect y-) (collect y0) (collect x)))))
  ((1 2) (-1 -2) (0 0) (1 0 -1 2 0 -2)))
(defok 178 (ton (let* ((x #Z(1 (nil) (3))))
		    (multiple-value-bind (y+ y- y0) (split-if x #'numberp #'car)
		      (list (collect y+) (collect y-) (collect y0)))))
  ((1) ((3)) ((nil))))

(defok 179 (ton (multiple-value-bind (x y) (chunk 2 #Z(1 2 3 4))
		    (list (collect x) (collect y))))
  ((1 2 3) (2 3 4)))
(defok 180 (ton (multiple-value-bind (x y) (chunk 2 2 #Z(1 2 3 4))
		    (list (collect x) (collect y))))
  ((1 3) (2 4)))
(defok 181 (ton (multiple-value-bind (x y) (chunk 2 3 #Z(1 2 3 4 5))
		    (list (collect x) (collect y))))
  ((1 4) (2 5)))
(defok 182 (ton (multiple-value-bind (x y) (chunk 2 #Z(1 2))
		    (list (collect x) (collect y))))
  ((1) (2)))
(defok 183 (ton (multiple-value-bind (x y) (chunk 2 #Z(1))
		    (list (collect x) (collect y))))
  (() ()))
(defok 184 (ton (collect (chunk 1 2 #Z(1 2 3)))) (1 3))
(defok 185 (ton (multiple-value-bind (x y z) (chunk 3 2 #Z(1 2 3 4 5))
		    (list (collect x) (collect y) (collect z))))
  ((1 3) (2 4) (3 5)))

(defok 186 (ton (collect #Z(a b c))) (a b c))

(defok 187 (ton (collect 'bag #Z(a b c))) (c b a))
(defok 188 (ton (collect-append 'list #Z((a b c) (a b c)))) (a b c a b c))
(defok 189 (ton (collect-append (car '(list)) #Z((a b c) (a b c)))) (a b c a b c))
(defok 190 (ton (collect-append 'list #Z())) ())
(defok 191 (ton (let ((a (list 1 2)) (b '(3 4)))
		    (collect-append (scan (list a b)))
		    a)) (1 2))

(defok 192 (ton (collect-nconc (scan (list nil (list 'a 'b) nil
					     (list 'c 'd) (list 'e) nil))))
  (a b c d e))
(defok 193 (ton (collect-nconc #Z())) ())
(defok 194 (ton (let ((a (list 1 2)) (b '(3 4)))
		    (collect-nconc (scan (list a b)))
		    a)) (1 2 3 4))

(defok 195 (ton (collect-alist #Z(d e d) #Z(a b c))) ((d . c) (e . b) (d . a)))
(defok 196 (ton (collect-alist #Z(d e d) #Z())) ())

(defok 197 (ton (collect-plist #Z(d e d) #Z(a b c))) (d c e b d a))
(defok 198 (ton (collect-plist #Z(d e d) #Z())) ())

(defok 199 (ton (let ((h (collect-hash #Z(color name) #Z(brown fred))))
		    (multiple-value-bind (key val) (scan-hash h)
		      (sort (collect (#Mcons key val))
			    #'(lambda (x y)
				(string-lessp (string (car x)) (string (car y))))))))
  ((color . brown) (name . fred)))

(defok 200 (ton (coerce (collect 'vector #Z(a b c)) 'list)) (a b c))
(defok 201 (ton (coerce (collect 'vector #Z()) 'list)) ())
(defok 202 (ton (collect '(simple-string 3) #Z(#\b #\a #\r))) "bar")
(defok 203 (ton (coerce (collect '(vector t 3) #Z(a b c)) 'list)) (a b c))

(defok 204 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (collect-file test-file #Z(a b c))
			 (collect (scan-file test-file)))) (a b c))

(defok 205 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (collect-file test-file #Z(#\a #\space #\newline #\c) #'write-char)
			 (collect (scan-file test-file #'read-line))))
  ("a " "c"))

(defok 206 (ton (collect-last #Z(a b c))) c)
(defok 207 (ton (collect-last #Z())) nil)
(defok 208 (ton (collect-last #Z() 'fill)) fill)

(defok 209 (ton (collect-length #Z(a b c))) 3)
(defok 210 (ton (collect-length #Z())) 0)
(defok 211 (ton (collect-length (choose (#Mplusp #Z(1 -1 2 -2))))) 2)

(defok 212 (ton (collect-sum #Z(1 2 3))) 6)
(defok 213 (ton (collect-sum #Z(1 2 3) 'float)) 6.0)
(defok 214 (ton (collect-sum #Z() 'float)) 0.0)

(defok 215 (ton (collect-min #Z(1 2 3))) 1)
(defok 216 (ton (collect-min #Z(2 1 3) #Z(a b c) 4)) b)
(defok 217 (ton (collect-min #Z())) nil)
(defok 218 (ton (collect-min #Z() #Z(a b c) 4)) 4)
(defok 219 (ton (collect-min #Z(a b c) #Z() 4)) 4)

(defok 220 (ton (collect-max #Z(1 2 3))) 3)
(defok 221 (ton (collect-max #Z(1 3 2) #Z(a b c))) b)
(defok 222 (ton (collect-max #Z())) nil)
(defok 223 (ton (collect-max #Z() #Z(a b c) 4)) 4)

(defok 224 (ton (collect-fn t #'(lambda () 0) #'+ #Z(1 2 3))) 6)
(defok 225 (ton (collect-fn 'integer #'(lambda () 0) #'+ #Z(1 2 3))) 6)
(defok 226 (ton (collect-fn 'integer #'(lambda () 0) #'(lambda (x y) (+ x y)) #Z(1 2 3))) 6)
(defok 227 (ton (collect-fn t #'(lambda () 0) #'(lambda (&rest args) (apply #'+ args))
			      #Z(1 2 3))) 6)
(defok 228 (ton (collect-fn t #'(lambda () 0) #'- #Z(1 2 3))) -6)
(defok 229 (ton (collect-fn t #'(lambda () 0) #'+ #Z())) 0)
(defok 230 (ton (collect-fn t #'(lambda () t) #'+ #Z())) t)
(defok 231 (ton (multiple-value-list
		      (collect-fn ' (values list integer) #'(lambda () (values nil 0))
				    #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
				    #Z(a b c) #Z(1 2 3))))
  (((c 3 2) (b 2 1) (a 1 0)) 3))
(defok 232 (ton (multiple-value-list
		      (collect-fn '(values list integer) #'(lambda () (values nil 0))
				  #'(lambda (a b x y) (values (cons (list x y b) a) (1+ b)))
				  #Z(a b c) #Z(1 2 3))))
  (((c 3 2) (b 2 1) (a 1 0)) 3))

(defok 233 (ton (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				(collect-fn t #'(lambda () 0)
					    #'(lambda (sum x)
						(incf xx)
						(+ sum x xx))
					    #Z(10 20 30)))) 66)
(defok 234 (ton (multiple-value-list
		      (encapsulated #'(lambda (b) `(common-lisp:let ((xx 0)) ,b))
				    (collect-fn '(values t t)
						#'(lambda () (values 0 1))
						#'(lambda (sum prod x)
						    (incf xx)
						    (values (+ sum x xx) (* prod x xx)))
						#Z(10 20 30))))) (66 36000))

(defok 235 (ton (collect-first #Z(a b c))) a)
(defok 236 (ton (collect-first #Z())) nil)
(defok 237 (ton (collect-first #Z() 't)) t)
(defok 238 (ton (collect-first (#Mcar #Z((t) (nil) 4)))) t)
(defok 239 (ton (collect-first (positions (#Mplusp #Z(-3 1 -1 3 -2))))) 1)
(defok 240 (ton (collect-first (choose #Z(nil t nil) #Z(0 1 -1 3 -2)))) 1)

(defok 241 (ton (collect-nth 1 #Z(a b c))) b)
(defok 242 (ton (collect-nth 1 #Z())) nil)
(defok 243 (ton (collect-nth 1 #Z() 't)) t)
(defok 244 (ton (collect-nth 1 (#Mcar #Z((t) (nil) 4)))) nil)

(defok 245 (ton (collect-and #Z(1 2))) 2)
(defok 246 (ton (collect-and (#Mcar #Z((t) (nil) 4)))) nil)
(defok 247 (ton (collect-and #Z())) t)

(defok 248 (ton (collect-or #Z(nil))) nil)
(defok 249 (ton (collect-or (#Mcar #Z((t) (nil) 4)))) t)
(defok 250 (ton (collect-or #Z())) nil)

;this contains tests of the various special forms supported.
(defok 251 (ton (let* ((lst (list 'a 'b 'c))
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
(defok 252 (ton (let* ((lst (list 1 2 3))
			 (l (scan-sublists lst))
			 (e (to-alter (#Mcar l)
				      #'(lambda (new parent num)
					  (rplaca parent (+ new num)))
				      l (series 10))))
		    (alter e (#M1+ e))
		    lst))
  (12 13 14))

(defok 253 (ton (let* ((x #Z(a b c))
			 (xx (#Mlist x)))
		    (collect (#Mlist x xx)))) ((a (a)) (b (b)) (c (c))))
(defok 254 (ton (let* ((x #Z(a b c))
			 (x (#Mlist x)))
		    (collect x))) ((a) (b) (c)))
(defok 255 (ton (let ((x 9))
		    (let ((x #Z(a b c))
			  (xx (series (list x))))
		      (collect (#Mlist x xx))))) ((a (9)) (b (9)) (c (9))))
(defok 256 (ton (let () (collect #Z(a b c)))) (a b c))
(defok 257 (ton (let* ((e 3)
			 (f #Z(a b c))
			 (g (collect f))
			 (h (collect #Z(a b c))))
		    (list e g h))) (3 (a b c) (a b c)))
(defok 258 (ton (let ((x (collect #Z(1 2 3))))
		    (list x)
		    x)) (1 2 3))
(defok 259 (ton (let ())) nil)
(defok 260 (ton (multiple-value-bind (key value) (scan-alist '((a . 1) (b . 2)))
		    (collect (#Mlist key value)))) ((a 1) (b 2)))
(defok 261 (ton (let ((key (scan-alist '((a . 1) (b . 2)))))
		    (collect key))) (a b))

(defok 262 (ton (collect-alist #Z(a b) (series (* 2 3)))) ((b . 6) (a . 6)))
(defok 263 (ton (let ((x 1))
		    (collect-alist #Z(a b) (series (setq x (1+ x)))))) ((b . 2) (a . 2)))

(defok 264 (ton (collect-sum (#Mcar #Z((1) (2))))) 3)
(defok 265 (ton (collect-sum (#M(lambda (x) (* 2 x)) #Z(1 2)))) 6)
(defok 266 (ton (let ((x 1))
		    (collect (#M(lambda (y) (list y (setq x (1+ x)))) #Z(a b))))) ((a 2) (b 3)))
(defok 267 (ton (let ((x 1))
		    (collect (#Mlist #Z(a b) (series (setq x (1+ x))))))) ((a 2) (b 2)))
(defok 268 (ton (collect (#M(lambda (x y) (if (plusp x) y))
			      #Z(10 -11 12) (scan-range)))) (0 nil 2))
(defok 269 (ton (collect (choose (#Mplusp #Z(10 -11 12)) (scan-range)))) (0 2))

(defok 270 (ton (let ((z #Z(1 2)))
		    (collect (#M(lambda (z) (do ((x 1 (1+ x)) (sum 0 (+ sum x)))
						((> x z) sum))) z))))
  (1 3))
(defok 271 (ton (let ((z #Z((1 2) (3 4))))
		    (collect (#M(lambda (x) (collect (scan x))) z)))) ((1 2) (3 4)))

(defok 272 (td (defun foo (list) "doc"
			(declare (optimizable-series-function))
			(#Mcar (scan list)))
		 (list #+symbolics(documentation 'foo 'function)
		       (collect (foo '((a) (b) (c))))))
  (#+symbolics"doc" (a b c)))
(defok 273 (td (defun foo02 (v)
		   (declare (optimizable-series-function) ((vector *) v))
		   "doc"
		   (#Mcar (scan 'vector v)))
		 (list #+symbolics(documentation 'foo02 'function)
		       (collect (foo02 '#((a) (b) (c))))))
  (#+symbolics"doc" (a b c)))

(defok 274 (td (defun foo1 (list &optional (plus 1))
		   (declare (optimizable-series-function))
		   (#M(lambda (x) (+ x plus)) (scan list)))
		 (list (collect (foo1 '(1 2 3) 3))
		       (collect (foo1 '(1 2 3)))))
  ((4 5 6) (2 3 4)))

(defok 275 (td (defun foo2 (list &optional (plus 1 p?))
		   (declare (ignore plus) (optimizable-series-function))
		   (#M(lambda (x) (list x p?)) (scan list)))
		 (list (collect (foo2 '(1 2 3) 3))
		       (collect (foo2 '(1 2 3)))))
  (((1 t) (2 t) (3 t)) ((1 nil) (2 nil) (3 nil))))

(defok 276 (td (defun foo3 (numbers)
		   (declare (optimizable-series-function))
		   (collect-sum (#M1+ numbers)))
		 (foo3 (mapping ((x (scan '(1 2 3)))) (1+ x))))
  12)

(defok 277 (td (defun my-collect-last (items &optional (default nil))
		   (declare (optimizable-series-function))
		   (collect-fn '(series-element-type items)
			       #'(lambda () default)
			       #'(lambda (old new) (declare (ignore old)) new)
			       items))
		 (list (my-collect-last #Z()) (my-collect-last #Z(1 2))))
  (nil 2))
(defok 278 (td (defun my-collect-last2 (items &optional (default nil))
		   (declare (optimizable-series-function))
		   (collect-fn '(series-element-type foo)
			       #'(lambda () default)
			       #'(lambda (old new) (declare (ignore old)) new)
			       items))
		 (list (my-collect-last2 #Z()) (my-collect-last2 #Z(1 2))))
  (nil 2))

(defok 279 (ton (multiple-value-list
		      (let ((x #Z(a b))) (values (collect x) (collect 'bag x)))))
  ((a b) (b a)))

(defok 280 (ton (multiple-value-bind (a b)
		      (#2M(lambda (x) (let ((*package* (find-package :common-lisp-user)))
					(intern (string x))))
			  #Z(x y))
		    (collect (#Mlist a b)))) ((x :internal) (y :internal)))

(defok 281 (ton (let ((v (list 1 -2 3)))
		    (let ((x (choose-if #'minusp (scan v))))
		      (alter x (#M- x)))
		    v)) (1 2 3))
(defok 282 (ton (let ((x (list 'a 'b 'c)))
		    (alter (scan x) (scan-range))
		    x)) (0 1 2))
(defok 283 (ton (let ((x '((a) (b) (c))))
		    (iterate ((a (scan x)) (b (scan-range)))
		      (setf (car a) b))
		    x)) ((0) (1) (2)))

(defok 284 (ton (let ((e (scan (list 1 2)))) (alter e (#M1+ e)) (collect e))) (1 2))
(defok 285 (ton (let ((x #Z(1 2 3))
			(y #Z(4 5)))
		    (list (collect-sum x) (collect-sum y)))) (6 9))
(defok 286 (ton (list (collect-sum #Z(1 2 3)) (collect-sum #Z(4 5)))) (6 9))

(defok 287 (ton (collect (producing (y) ((x #Z((1) (2))) (item nil))
				      (loop
					  (tagbody
					     (setq item (next-in x (terminate-producing)))
					     (next-out y (car item))))))) (1 2))
(defok 288 (ton (producing ((number 0)) ((numbers #Z(1 2)) (num 0))
			     (declare ((series integer) numbers) (integer number num))
			     (loop
				 (tagbody
				    (setq num (next-in numbers (terminate-producing)))
				    (setq num (1+ num))
				    (setq number (+ number num)))))) 5)
(defok 289 (ton (multiple-value-bind (sum y)
		      (producing ((number 0) y) ((numbers #Z(1 2)) num)
				 (loop
				     (tagbody
					(setq num (next-in numbers (terminate-producing)))
					(setq num (1+ num))
					(setq number (+ number num))
					(next-out y num))))
		    (list sum (collect y))))
  (5 (2 3)))
(defok 290 (ton (multiple-value-bind (sum y)
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
(defcmukernel 291 (ton (let ((list nil))
		    (producing ((x nil)) ((numbers #Z(1 2)) num)
			       (loop
				   (tagbody
				      (setq num (next-in numbers (terminate-producing)))
				      (push num list))))
		    list)) (2 1))
(defok 292 (ton (nreverse (producing ((list nil)) ((items #Z(1 2)) item)
				       (declare (list list))
				       (loop
					   (tagbody
					      (setq item (next-in items (terminate-producing)))
					      (setq list (cons item list))))))) (1 2))
(defok 293 (ton (collect (producing (items) ((list '(1 2)) item)
				      (loop
					  (tagbody
					     (if (endp list) (terminate-producing))
					     (setq item (car list))
					     (setq list (cdr list))
					     (next-out items item)))))) (1 2))
(defcmukernel 294 (ton (collect (producing (items) ((Nitems1 #Z(1 2)) (Nitems2 #Z(3 4))
					       (done nil) item)
				      (loop
					  (tagbody
					     (if done (go D))
					     (setq item (next-in Nitems1 (setq done t) (go D)))
					     (go F)
					   D (setq item (next-in Nitems2 (terminate-producing)))
					     (setq item (1+ item))
					   F (next-out items item)))))) (1 2 4 5))
(defok 295 (ton (multiple-value-bind (x+ x-)
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
(defok 296 (ton (collect (producing (items) ((Nitems #Z(1 -2 3)) item)
				      (declare (type (series integer) Nitems))
				      (loop
					  (tagbody
					   L (setq item (next-in Nitems (terminate-producing)))
					     (if (not (plusp item)) (go L))
					     (next-out items item)))))) (1 3))
(defok 297 (ton (let*
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
(defok 298 (ton (let*
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
(defok 299 (ton (let ((x '(1 2 3)))
		    (producing ((xx nil)) ()
			       (loop
				   (tagbody
				      (setq xx (if (null x) (terminate-producing) (pop x))))))))
  3)
(defok 300 (td (defmacro Rcount (items)
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
(defok 301 (ton (funcall #'(lambda (x) (let ((z (list x))) (list z))) 4))
  ((4)))
(defok 302 (ton (funcall #'(lambda (x) (nreverse (collect 'bag x))) #Z(a b c)))
  (a b c))
(defok 303 (ton (funcall #'(lambda (x) (collect (#Mlist x))) #Z(a b c)))
  ((a) (b) (c)))
;mg2
(defok 304 (ton (funcall #'(lambda (x y)
			       (list (collect x) (collect (choose (#Mplusp y) y))))
			   #Z(a b c) #Z(1 -2 3)))
  ((a b c) (1 3)))
(defok 305 (ton (funcall #'(lambda (x y)
			       (list (collect (choose (#Mplusp y) y)) (collect x)))
			   #Z(a b c) #Z(1 -2 3)))
  ((1 3) (a b c)))

;mg3
(defok 306 (ton (collect (funcall #'(lambda (x y z) (catenate (mingle x y #'<) z))
				    #Z(1 2 4) #Z(1 3 3) #Z(0))))
  (1 1 2 3 3 4 0))
(defok 307 (ton (multiple-value-bind (a b) (scan-plist '(k1 2 k2 4))
		    (list (collect b)
			  (collect (expand (series nil nil t nil t nil nil nil t)
					   a nil)))))
  ((2 4) (nil nil k1 nil k2 nil nil nil)))
(defok 308 (ton (collect (funcall #'(lambda (x)
					(multiple-value-bind (a b) (scan-plist x)
					  (expand #Z(nil nil t nil t nil nil nil t)
						  a nil)
					  b))
				    '(k1 2 k2 4))))
  (2 4))
(defok 309 (ton (collect (funcall #'(lambda (x) (catenate (#Mlist x) #Z(5 6)))
				    #Z(1 2 3))))
  ((1) (2) (3) 5 6))
(defok 310 (ton (collect (funcall #'(lambda (x) (catenate (choose (#Mplusp x) x) #Z(5 6)))
				    #Z(1 -2 3))))
  (1 3 5 6))
(defok 311 (ton (collect (funcall #'(lambda (x) (choose-if #'evenp (split-if x #'plusp)))
				    #Z(1 2 -2 3 4))))
  (2 4))
(defok 312 (ton (collect (funcall #'(lambda (x) (#Mlist (split-if x #'plusp)))
				    #Z(1 2 -2 3 4))))
  ((1) (2) (3) (4)))
;mg4
(defok 313 (ton (multiple-value-bind (a b) (scan-plist '(k1 1 k2 -2))
		    (list (collect a) (collect (choose-if #'plusp b)))))
  ((k1 k2) (1)))
(defok 314 (ton (collect (funcall #'(lambda (x)
					(multiple-value-bind (a b) (scan-plist x)
					  (collect (choose-if #'plusp b)) a))
				    '(k1 1 k2 -2))))
  (k1 k2))
(defok 315 (ton (let (z)
		    (list (collect (funcall #'(lambda (x)
						(multiple-value-bind (a b) (scan-plist x)
						  (setq z (collect  'bag (choose-if #'plusp b)))
						  (#Mlist a)))
					    '(k1 1 k2 -2)))
			  z)))
  (((k1) (k2)) (1)))
;mg5
(defok 316 (ton (multiple-value-bind (a b)
		      (funcall #'(lambda (x y)
				   (cotruncate (choose (#Mplusp x) x) (scan y)))
			       #Z(1 -2 3) '(a b c))
		    (list (collect a) (collect b))))
  ((1 3) (a b)))

(defok 317 (ton (multiple-value-bind (a b)
		      (funcall #'(lambda (x y)
				   (cotruncate (choose (#Mplusp x) x) (scan y)))
			       #Z(1 -2 3) '(a b c))
		    (list (collect a) (collect b))))
  ((1 3) (a b)))

;these are weird tests checking for particular bugs in old versions
(defok 318 (ton (multiple-value-list
		      (let ((strings (choose-if #'stringp
						(scan `(1 2 ,(symbol-name 'cond) 4)))))
			(find-symbol (collect-first strings)))))
  (cond :inherited))
(defcmukernel 319 (td (defun weighted-sum (numbers weights)
		   (declare (optimizable-series-function 2) (off-line-port weights))
		   (values (collect-sum numbers) (collect-sum (#M* numbers weights))))
		 (list (multiple-value-list (weighted-sum #Z(1 2 3) #Z(3 2)))
		       (multiple-value-list (weighted-sum #Z(1 2) #Z(3 2)))
		       (multiple-value-list (weighted-sum #Z(1) #Z(3 2)))))
  ((6 7) (3 7) (1 3)))
(defok 320 (td (defun non-series-used-twice (x)
		   (declare (optimizable-series-function))
		   (let ((s (scan-range)))
		     (scan (list (collect-nth x s) (+ x (collect-nth x s))))))
		 (collect (non-series-used-twice (1+ 2))))
  (3 6))
(defok 321 (td (defun baz (items)
		   (declare (optimizable-series-function))
		   (let ((items items))
		     (collect items)))
		 (baz #Z(1 2 3)))
  (1 2 3))
(defok 322 (ton (let ((z -1))
		    (let ((e #Z(1 2 3)))
		      (setq z (collect-last e))
		      z))) 3)
(defok 323 (ton (let ((x (list 1 2 3)))
		    (collect-last
		     (#M(lambda (x y) (list (setf (car x) y)))
			(scan-sublists x) #Z(a b c d))) x))
  (a b c))	;don't want to have any complaints from setf here.
(defok 324 (ton (collect-first (choose-if #'(lambda (x) (and (car x) (cdr x)))
					    #Z((a) (nil . b) (a . b) (c))))) (a . b))
(defok 325 (ton (let ((l (car '((1 2 3 4)))))
		    (collect (#Mlist (scan l) (scan l))))) ((1 1) (2 2) (3 3) (4 4)))
(defok 326 (ton (let ((x nil))
		    (iterate ((y #Z(1 2)))
		      (push y x)) x)) (2 1))
;tests 327--330 removed.
(defok 331 (ton (let ((x #Z(2 -1 0 1 -2)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose-if #'minusp x))))) (3 -3))
(defok 332 (ton (let ((x #Z(2 -1 0 1 -2)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose (#Mminusp x) x)))))
  (3 -3))
(defok 333 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x) (collect-sum (choose-if #'minusp x))))) (0 -3))
(defok 334 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x) (collect 'bag (choose-if #'plusp x))))) (0 (1 2)))
(defok 335 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum x)
			  (collect 'bag (choose-if #'plusp x))
			  (collect-sum (choose-if #'plusp x)))))
  (0 (1 2) 3))
(defok 336 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose-if #'minusp x))))) (3 -3))
(defok 337 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect-sum (choose (#Mminusp x) x)))))
  (3 -3))
(defok 338 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect 'bag (choose-if #'plusp x)))))
  (3 (1 2)))
(defok 339 (ton (let ((x (split-if #Z(2 -1 a 0 b 1 -2) #'numberp)))
		    (list (collect-sum (choose-if #'plusp x))
			  (collect 'bag (choose-if #'plusp x))
			  (collect-sum (choose-if #'plusp x)))))
  (3 (1 2) 3))

(defok 340 (ton (let ((v (list 1 -2 3)))
		    (let* ((e (scan v))
			   (x (until-if #'minusp e)))
		      (alter x (#M- x)))
		    v)) (-1 -2 3))
(defok 341 (ton (collect (subseries (mask (positions #Z(t nil t nil))) 0 5)))
  (t nil t nil nil))
(defok 342 (ton (let ((x '(1 2 3)))
		    (macrolet ((bab (z) `(list ,z)))
		      (collect (scan (bab x)))))) ((1 2 3)))
(defok 343 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when x (setq xx x))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) 1))
(defok 344 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when x (setq xx (collect (scan-range :upto x))))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) (0 1)))
(defok 345 (ton (let (xx)
		    (list (collect (let ((x (car '(1))) (y (scan '(1 2))))
				     (when t (setq xx (collect (scan-range :upto 2))))
				     (#M+ y (scan-range :upto x))))
			  xx))) ((1 3) (0 1 2)))
(defok 346 (ton (let ((x #Z(1 2)))
		    (if (collect-sum x) 1 2))) 1)

(defok 347 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b))) (nil (1 2) (1 2)))
(defok 348 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (setq a (length a)) a (collect z) b)))
  (nil 0 0 (1 2) (1 2)))
(defok 349 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (setq a 3) a (collect z) b)))
  (nil 3 3 (1 2) (1 2)))
(defok 350 (ton (let ((zz nil))
		    (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		      (list a (multiple-value-setq (zz a) (truncate 5 2))
			    a zz (collect z) b))))
  (nil 2 1 2 (1 2) (1 2)))
(defok 351 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq z (#M1+ z))
		    (list a (collect z) b))) (nil (2 3) (1 2)))
(defok 352 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list b (setq b 4) a (collect z) b)))
  ((1 2) 4 nil (1 2) 4))
(defok 353 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq a (#M1+ z))
		    (list (collect a) (collect z) b)))
  ((2 3) (1 2) (1 2)))
(defok 354 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (setq z (collect-sum z))
		    (list a z b)))
  (nil 3 (1 2)))
(defok 355 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (when (not a) (setq b '(3)))
		    (list a (collect z) b)))
  (nil (1 2) (3)))
(defok 356 (ton (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b (when (not a) (setq b '(3))) b)))
  (nil (1 2) (1 2) (3) (3)))
(defok 357 (ton (let* ((a 2) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect z) b (when a (setq a 3)) a)))
  (2 (1 2) (1 2) 3 3))
(defok 358 (ton (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		    (list a (collect (#M(lambda (y) (setq a (1+ a)) (+ a y)) z))
			  a (collect z) b)))
  (3 (5 7) 5 (1 2) (1 2)))
(defok 359 (ton (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		    (list (collect z)
			  (collect (#M(lambda (y) (setq z -3) (1+ y)) z))
			  a z b)))
  ((1 2) (2 3) 3 -3 (1 2)))
(defok 360 (ton (let* ((z 0))
		    (list z
			  (collect (scan-fn t #'(lambda () (car (setq z (list '(1 2 3)))))
					    #'(lambda (l) (car (push (cdr l) z)))
					    #'(lambda (l) (push (car l) z) (null l))))
			  (reverse z))))
  (0 ((1 2 3) (2 3) (3)) ((1 2 3) 1 (2 3) 2 (3) 3 nil nil)))

(defok 361 (ton (let ((z nil))
		    (let* ((x #Z(1 2 3)))
		      (setq z (cons (collect-sum x) z))
		      (setq z (cons 'a z))
		      (cons 'b z))))
  (b a 6))
(defok 362 (ton (let ((z nil))
		    (let* ((x #Z(1 2 3)))
		      (setq z (cons 'a z))
		      (setq z (cons (collect-sum x) z))
		      (cons 'b z))))
  (b 6 a))
(defok 363 (ton (let ((z nil))
		    (let* ((x (scan (progn (push 1 z) '(1 2))))
			   (y (scan (progn (push 2 z) '(1 3)))))
		      (setq z (cons (collect-sum x) z))
		      (setq z (cons (collect-sum y) z))
		      (cons 'b z))))
  (b 4 3 2 1))
(defok 364 (ton (let ((*print-length* nil) (*print-level* nil))
		    (with-output-to-string (s)
					   (prin1 #Z(1 2) s))))
  "\#Z(1 2)")
(defok 365 (ton (let ((*print-length* 1) (*print-level* 3))
		    (with-output-to-string (s)
					   (prin1 #Z(1 2) s))))
  "\#Z(1 ...)")
(defok 366 (ton (let ((x #Z(1 2 3)) (y 4))
		    (setq y (collect (#M+ (series y) x)))))
  (5 6 7))
(defok 367 (ton (collect (let ((x #Z(1 2 3 4)) (y (series 4)))
			     (setq y (split-if (#M+ y x) #'evenp)))))
  (6 8))
(defok 368 (td (defun pip1 (x)
		   (declare (optimizable-series-function) (off-line-port 0))
		   (push 3 x)
		   (values (scan x) (scan (cdr x))))
		 (multiple-value-bind (a b) (pip1 '(a b))
		   (list (collect a) (collect b))))
  ((3 a b) (a b)))

;tests of places where some termination points are not connected to every output
(defok 369 (ton (let ((x #Z(1 2 3)))
		    (list (collect-first x) (collect-sum x))))
  (1 6))
(defok 370 (ton (let ((x #Z(foo nil bar)))
		    (list (collect-and x) (collect x))))
  (nil (foo nil bar)))
(defok 371 (ton (let ((z 0))
		    (list (let ((x #Z(1 2 3)))
			    (iterate ((v x))
			      (setq z (+ z v)))
			    (collect-first x))
			  z)))
  (1 6))
(defok 372 (ton (let ((z 0))
		    (list (let ((x #Z(1 2 3)))
			    (mapping ((v x))
			      (setq z (+ z v)))
			    (collect-first x))
			  z)))
  (1 0))
(defok 373 (ton (multiple-value-list
		      (let* ((x (list 'a 'b 'c 'd))
			     (e (scan x)))
			(values (collect e)	
				(let () (alter e (scan '(1 2))) (collect e))
				x))))
  ((a b c d) (a b c d) (1 2 c d)))
(defok 374 (ton (let ((x #Z(1 2 3))
			(y #Z(4 5 6 7)))
		    (list (collect-sum (#M+ x y)) (collect-sum y))))
  (21 22))
(defok 375 (ton (let ((x #Z(1 2 3))
		      (y #Z(4 5 6 7)))
		    (list (collect-sum (#M+ x y)) (collect-sum y) (collect-sum y))))
  (21 22 22))
(defok 376 (ton (let* ((e #Z(1 -2 -4 3))
		       (f #Z(a)))
		    (multiple-value-bind (w x) (split-if e #'plusp)
		      (list (collect (#Mlist w))
			    (collect (#Mlist x f))))))
  (((1) (3)) ((-2 a))))
(defok 377 (ton (let ((e #Z(1 -2 -4 3 5 6)))
		    (list (collect e) (collect (subseries e 1 3)))))
  ((1 -2 -4 3 5 6) (-2 -4)))
(defok 378 (ton (let ((e #Z(1 -2 -4 3 5 6)))
		    (list (collect e) (collect (#Mlist (subseries e 1 3) #Z(a))))))
  ((1 -2 -4 3 5 6) ((-2 a))))
(defok 379 (ton (let* ((e1 #Z(1 -2 -4 3)) (e2 #Z(1 -2 -4 3)) (e3 #Z(1 -2 -4 3))
			 (w1 (split-if e2 #'plusp)))
		    (multiple-value-bind (x1 x2) (split-if e3 #'plusp)
		      (declare (ignore x1))
		      (list (collect (#Mlist e1 w1)) (collect (#Mlist w1 x2))))))
  (((1 1) (-2 3)) ((1 -2) (3 -4))))
(defok 380 (td (defun bar1 (numbers)
		   (declare (optimizable-series-function) (off-line-port numbers))
		   (scan (collect numbers)))
		 (collect-sum (bar1 (scan-range :upto 3))))
  6)
(defcmumismatch 381 (td (defun bar2 (numbers others)
		   (declare (optimizable-series-function)
			    (type (series integer) numbers)
			    (off-line-port numbers others))
		   (list (collect-sum numbers) (collect-sum others)))
		 (bar2 (scan-range :upto 3) (scan-range :upto -1)))
  (6 0))
(defcmumismatch 382 (td (defun bar3 (numbers others)
		   (declare (type series numbers others)
			    (optimizable-series-function)
			    (off-line-port numbers others))
		   (iterate ((n numbers))
		     (setq *x* n))
		   (collect-sum others))
		 (list (bar3 (scan-range :upto 3) (scan-range :upto 0)) *x*))
  (0 3))
(defcmukernel 383 (td (defun bar4 (numbers others)
		   (declare (optimizable-series-function 2)
			    (type (series nil) y others)
			    (off-line-port numbers others))
		   (values (collect-sum numbers) (collect-sum others)))
		 (multiple-value-list
		   (bar4 (scan-range :upto 3) (scan-range :upto -1))))
  (6 0))
(defok 384 (td (defun bar5 (numbers)
		   (declare (optimizable-series-function 2)
			    (type (series t) numbers))
		   (floor (collect-sum numbers) 4))
		 (multiple-value-list (bar5 (scan-range :upto 3))))
  (1 2))

;tests of declarations and the like that only apply to optimization

(defok 385
  (to (not (null (member '(type integer x)
			 (decls (let ((x #Z(1 2 3)))
				  (declare (type (series integer) x))
				  (collect-sum x))) :test #'equal)))) t)
#-:series-letify
(defok 386
  (to (not (null (member '(type integer x)
			 (decls (let* ((y #Z(1 2))
				       (x (the integer (collect-sum y))))
				  (list x x))) :test #'equal)))) t)
(defok 387
  (to (not (null (member '(type integer y)
			 (decls (let* ((y (the (series *) #Z(1 2)))
				       (x (collect-sum y)))
				  (list x x))) :test #'equal)))) nil)
(defok 388
  (to (not (null (member '(type integer y)
			 (decls (let* ((y (the (series integer) #Z(1 2)))
				       (x (collect-sum y)))
				  (list x x))) :test #'equal)))) t)

;tests of some otherwise hard to test internal functions
;these would probably have to be changed a good deal if there were any
;significant internal modifications in the way things worked.

(defok 389 (ton (series::nsubst-inline nil 1 (list 3 1 2))) (3 2))
(defok 390 (ton (series::nsubst-inline nil 4 (list 3 1 2))) (3 1 2))
(defok 391 (ton (series::nsubst-inline nil 2 (list 3 1 2))) (3 1))

(defok 392 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((if (car x) (go ,series::end)))))) t)
(defok 393 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((tagbody ,series::end
						   (if (car x) (go ,series::end))))))) nil)
(defok 394 (ton (series::active-terminator-p
		   (series::make-frag :prolog `((tagbody (if (car x) (go ,series::end))))))) t)

(defok 395 (ton (series::vars-of '((:foo bar) 1 bar-p))) (bar bar-p))

(defok 396 (ton (series::make-general-setq '(x y) '(values 1 2))) (psetq x 1 y 2))
(defok 397 (ton (series::make-general-setq '(x y) '(values 1 2 3)))
  (multiple-value-setq (x y) (values 1 2 3)))

(defok 398 (ton (let ((code (copy-tree '((setq x 3) (setq y 4)))))
		    (series::clean-code1 '(x) nil code) code)) ((setq y 4)))
(defok 399 (ton (let ((code (copy-tree '((setq x 3)))))
		    (series::clean-code1 '(x) nil code) code)) (3))
(defok 400 (ton (let ((code (copy-tree  '((progn (setq x 3) . 4)))))
		    (series::clean-code1 '(x) nil code) code)) ((progn . 4)))
(defok 401 (ton (let ((code (copy-tree '((progn (setq x 3))))))
		    (series::clean-code1 '(x) nil code) code)) ((progn)))

(defok 402 (ton (series::make-test nil)) t)
(defok 403 (ton (series::make-test '((x y) (x)))) x)
(defok 404 (ton (series::make-test '((x z) (x y w)))) (or x (and z (or y w))))
(defok 405 (ton (series::make-test '((x z) (x y w) (z)))) (and (or x y w) z))

;tests of generators and gatherers.

(defok 406 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x (return nil)) l)
			(push (next-in x (return nil)) l)
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 +))
(defok 407 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x (next-in x (return nil))) l)
			(push (next-in x (return nil)) l)
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 +))
(defok 408 (ton (let ((l nil)
			(x (generator (scan '(1 2 3 4)))))
		    (loop (push (next-in x 44) l)
			(if (= 44 (car l)) (return nil))
		      (push (next-in x 55) l)
		      (if (= 55 (car l)) (return nil))
		      (push '+ l))
		    (nreverse l))) (1 2 + 3 4 + 44))

(defok 409 (ton (let ((g (gatherer #'collect-sum)))
		    (next-out g 3)
		    (next-out g 4)
		    (result-of g))) 7)
(defok 410 (ton (let ((g (gatherer #'(lambda (x) (collect x)))))
		    (result-of g))) nil)
(defok 411 (ton (let ((g (gatherer #'(lambda (x) (collect (choose-if #'plusp x))))))
		    (next-out g 3)
		    (next-out g -3)
		    (next-out g 4)
		    (result-of g))) (3 4))
(defok 412 (ton (let ((g (gatherer #'(lambda (x) (collect (choose (#Mplusp x) x))))))
		    (next-out g 3)
		    (next-out g -3)
		    (next-out g 4)
		    (result-of g))) (3 4))
(defok 413 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (let ((g (gatherer #'(lambda (x) (collect-file test-file x)))))
			   (next-out g 3)
			   (next-out g 4)
			   (list (result-of g) (collect (scan-file test-file)))))) (t (3 4)))
(defok 414 (ton (let ((x (gatherer #'(lambda (x) (collect x))))
		      (y (gatherer #'(lambda (ns) (collect-sum (choose-if #'oddp ns))))))
		    (dotimes (i 4)
		      (next-out x i)
		      (next-out y i)
		      (if (evenp i) (next-out x (* i 10))))
		    (list (result-of x) (result-of y)))) ((0 0 1 2 20 3) 4))

(defok 415 (ton (gathering ((y collect-sum))
			     (next-out y 1) (next-out y 2))) 3)
(defok 416 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (prog1 (list (gathering ((g (lambda (x) (collect-file test-file x))))
						 (next-out g 3)
						 (next-out g 4))
				      (collect (scan-file test-file)))
			   (if (probe-file test-file) (delete-file test-file))))) (t (3 4)))
(defok 417 (ton (multiple-value-list
		      (gathering ((x (lambda (x) (collect x)))
				  (y collect-sum))
				 (dotimes (i 3)
				   (next-out y i)
				   (if (evenp i) (next-out x (* i 10))))))) ((0 20) 3))

;the following tests were introduced to insure that every control path
;in the system is exercised by atleast one test.

(defok 418 (ton (let ((xx 0))
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
(defok 419 (tn (typep #Z(1 2 3) 'series)) t)
(defok 420 (tn (typep (cotruncate #Z(1 2 3) #Z(1 2)) 'series)) t)

(defok 421 (ton (collect (funcall #'scan '(1 2)))) (1 2))
(defok 422 (to (funcall #'collect #Z(1 2))) (1 2))
(defok 423 (ton (funcall #'(lambda (x &optional (y 2)) (list x y)) 1)) (1 2))

(defok 424 (ton (let ((x #Z(1 2 3)))
		    (multiple-value-bind (a b) (collect x)
		      (list a b))))
  ((1 2 3) nil))

(defok 425 (ton (let ((x #Z(a b c)))
		    (list (collect x) (collect (catenate x #Z(1 2))))))
  ((a b c) (a b c 1 2)))
(defok 426 (ton (let ((xx nil))
		    (list (collect
			   (catenate (producing (a (b nil)) ((x #Z(1 2 3)))
						(loop (tagbody (setq b (next-in x (terminate-producing)))
							 (setq xx b)
							 (next-out a (list b)))))
				     #Z(1 2)))
			  xx)))
  (((1) (2) (3) 1 2) 3))

(defcmukernel 427 (ton (let ((x #Z(1 2 3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag t)
						     j (next-out a xval))))))))
  ((1 2 3) (1 2 3 3)))
(defcmukernel 428 (ton (let ((x #Z(1 2 3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag t)
						     j (next-out a xval))))))))
  ((1 2 3) (1 2)))
(defcmukernel 429 (ton (let ((x #Z(1 2 -3)))
		    (list (collect x)
			  (collect (producing (a) ((xx x) (yy #Z(0 0 0 0 1)) (flag nil) xval yval)
					      (loop (tagbody
						       (setq yval (next-in yy (terminate-producing)))
						       (if (plusp yval) (terminate-producing))
						       (if flag (go j))
						       (setq xval (next-in xx (go f)))
						       (if (minusp xval) (terminate-producing))
						       (go j)
						     f (setq flag t)
						     j (next-out a xval))))))))
  ((1 2 -3) (1 2)))

(defok 430 (ton (let ((x #Z(a b c)) (y #Z(0)))
		    (list (collect x) (collect y) (collect (catenate x y #Z(1 2))))))
  ((a b c) (0) (a b c 0 1 2)))
(defok 431 (td (defun comp5 (numbers)
		   (declare (optimizable-series-function) (off-line-port numbers))
		   (let ((x #Z(a b c)))
		     (list (collect x) (collect (catenate x numbers)))))
		 (comp5 #Z(1 2)))
  ((a b c) (a b c 1 2)))
(defok 432 (ton (let ((x #Z(a b c)))
		    (list (collect (catenate x #Z(-1 -2))) (collect (catenate x #Z(1 2))))))
  ((a b c -1 -2) (a b c 1 2)))
(defok 433 (td (defun comp6 (numbers)
		   (declare (optimizable-series-function))
		   (let ((numbers (split-if numbers #'plusp)))
		     (list (collect numbers) (collect (subseries numbers 1)))))
		 (comp6 #Z(1 -1 2)))
  ((1 2) (2)))
(defok 434 (td (defun comp7 (x)
		   (declare (optimizable-series-function))
		   (let ((numbers (split-if #Z(1 -1 2) #'plusp)))
		     (collect (#Mlist numbers x))))
		 (comp7 #Z(a b c)))
  ((1 a) (2 b)))

(defok 435 (ton (let ((x #Z(1 2 3)) (y #Z(2)))
		    (list (collect-sum x) (collect-sum (#M* x (catenate y #Z(1)))))))
  (6 4))

(defok 436 (ton (collect (mapping ((x #Z(1 2)))
			     (do* ((i 1 (1+ i))
				   (r 0 (+ x r)))
				  ((> i x) r)))))
  (1 4))

(defok 437 (ton (collect (mapping ((x #Z(1 2)))
			     ((lambda (i) (list i)) (1+ x)))))
  ((2) (3)))

(defok 438 (ton (let (a b)
		    (collect (mapping ((x #Z(1 2 3)))
			       (multiple-value-setq (a b) (floor x 2))
			       (list a b)))))
  ((0 1) (1 0) (1 1)))
(defok 439 (ton (let (a b)
		    (let ((x #Z(1 2 3)))
		      (multiple-value-setq (a b) (floor 1 1))
		      (list a b (collect x)))))
  (1 0 (1 2 3)))

(defok 440 (ton (let ((x #Z(1 2 3)))
		    (multiple-value-bind (x y)
			(collect-fn '(values t t)
				    #'(lambda () (values 1 2))
				    #'(lambda (x y z) (values (+ x z) (+ y z)))
				    x)
		      (list (list x) (list y)))))
  ((7) (8)))

(defok 441 (td (defun zz () 2)
		 (zz))
  2)
(defok 442 (td (defun zz1 (x) (declare (ignore x)) "foo")
		 (zz1 1))
  "foo")

;more special tests

;The following test error checking.
;Some must be tested only when optimized, because they
;cause ordinary errors when non-optmized

(defok 443 (te (generator 3)) 60)

(defok 444 (te (gatherer #'(lambda (x) (values (collect x) (collect x))))) 61)
(defok 445 (te (gatherer #'(lambda (x y) (collect (#M+ x y))))) 61)
(defok 446 (te (gatherer #'(lambda (x) (scan x)))) 61)
(defok 447 (te (gatherer #'scan)) 61)
(defok 448 (te (gatherer #'positions)) 61)
(defok 449 (te (tr (gatherer #'scan))) 61)

(defok 450 (te (map-fn '(values) #'1+ #Z(1 2 3))) 62)

(defok 451 (te (chunk -1 #Z(1 2 3))) 63)

(defok 452 (te (chunk 1 -1 #Z(1 2 3))) 64)

(defok 453 (te (alter (scan-range :upto 4) (series 5))) 65)
(defok 454 (te (alter (#Mcar (scan '((1)))) (series 5))) 65)
(defok 455 (te (alter (positions (scan '(1))) (series 5))) 65)

(defok 456 (teo (let ((t #Z(1))) a)) 66)
(defok 457 (teo (multiple-value-bind ((a)) #Z(1) a)) 66)
(defok 458 (teo (multiple-value-bind (t b) #Z(1) a)) 66)
(defok 459 (teo (let ((2 #Z(1))) nil)) 66)
(defok 460 (teo (let ((a #Z(1) nil)) nil)) 66)

(defok 461 (teo (funcall #'(lambda (a b) (car a)) #Z(1))) 67)
(defok 462 (teo (funcall #'(lambda (a) (car a)) #Z(1) #Z(2))) 67)

(defok 463 (te (encapsulated foo (collect-fn t #'f #'cons #Z(1 2 3)))) 68)

(defok 464 (te (encapsulated #'foo (collecting-fn t #'f #'cons #Z(1 2 3)))) 69)

(defok 465 (te (map-fn #'car #Z(1 2 3))) 70)

;; This tests for unsupported lambda keyword options.

(defok 466 (teo (defun ff (&key b)
		    (declare (optimizable-series-function))
		    (car a))) 71)
(defok 467 (te (defun ff (a &rest b)
		   (declare (optimizable-series-function))
		   (list a b))) 71)
(defok 468 (te (defun ff (a &allow-other-keys b)
		   (declare (optimizable-series-function))
		   (list a b))) 71)

(defok 469 (teo (defun ff ((a) b)
		    (declare (optimizable-series-function))
		    (car a))) 72)
(defok 470 (teo (defun ff (t b)
		    (declare (optimizable-series-function))
		    (car a))) 72)
(defok 471 (teo (defun ff (nil b)
		    (declare (optimizable-series-function))
		    (car a))) 72)

(defok 472 (te (producing (x) (y z) (print y))) 73)

(defok 473 (te (producing () (y z) (loop (tagbody (print y))))) 74)

(defok 474 (te (producing (x) ((y #Z(1 2)) z) (loop (tagbody (setq z (next-in)))))) 75)

(defok 475 (te (producing (x) ((y #Z(1 2)) z) (loop (tagbody (next-out z))))) 76)

(defok 476 (te (collect (scan-range :upto 5 :below 6))) 77)

(defok 477 (te (scan-multiple '(values list) '(1 2) '(3 4))) 78)

(defok 478 (te (collect (latch #Z(1 2) :after 2 :before 3))) 79)

;These test warnings

(defok 479 (tw (let ((x (scan '(1 2 3))))
		   (declare (integer x))
		   (collect x)))
  (1 2 3) 30)
(defok 480 (tw (let ((x (scan '(1 2 3))) (y 3))
		   (declare (series y))
		   (collect (#M+ (series y) x))))
  (4 5 6) 31)
(defok 481 (tw (defun ugh1 (a b)
		   (declare (optimizable-series-function))
		   (collect (#Mcons a (choose-if #'plusp b)))))
  ugh1 40)
(defok 482 (tw (defun ugh2 (a b) "doc"
			(declare (optimizable-series-function) (off-line-port b) (integer a))
			(collect (#Mcons a b))))
  ugh2 41)
(defok 483 (tw (defun ugh3 (a b)
		   (declare (optimizable-series-function))
		   (choose a b)))
  ugh3 42)
(defok 484 (tw (defun ugh4 (a b)
		   (declare (optimizable-series-function) (off-line-port 0))
		   (collect (#Mcons a b))))
  ugh4 43)
(defok 485 (tw (defun ugh44 (a)
		   (declare (optimizable-series-function))
		   (collect-sum (scan a))))
  ugh44 44)
;here temporarily not working, due to bug in code.
;(defok 486 (tw (let ((e #Z(1 2 3))) (collect #Z(1 2))))
;  ((1 2) 52))
(defok 487 (tw (let ((e #Z(1 2 3))) (declare (ignore e)) (collect e)))
  (1 2 3) 53)

;things that are half way from warnings to resriction violations.

(defok 488 (tw (collect (phys-scan-list '(1 2 3))))
  (1 2 3) 28)
(defok 489 (tw (let ((f #'(lambda (x) (collect-sum x))))
		   (let ((g (gatherer f)))
		     (next-out g 3)
		     (next-out g 4)
		     (result-of g)))) 7 28)

(defok 490 (tw (block bar
		   (iterate ((x (series -1 2 3)))
		     (if (plusp x) (return-from bar x))))) 2 29)
#-(or allegro clisp)
(defok 491 (tw (compiler-let ((*suppress-series-warnings* t))
		   (block bar
		     (iterate ((x (series -1 2 3)))
		       (if (plusp x) (return-from bar x)))))) 2 nil)

#+clisp
(defok 491 (tw (ext::compiler-let ((*suppress-series-warnings* t))
		   (block bar
		     (iterate ((x (series -1 2 3)))
		       (if (plusp x) (return-from bar x)))))) 2 nil)

#+allegro
(defok 491 (tw (cltl1::compiler-let ((*suppress-series-warnings* t))
		   (block bar
		     (iterate ((x (series -1 2 3)))
		       (if (plusp x) (return-from bar x)))))) 2 nil)

;These test restriction violation checks

(defok 492 (tr (let ((*print-length* 2) (x #Z(1 2 3 4 5)))
		   (declare (special *print-length*))
		   (collect x)))
  (1 2 3 4 5) 1)
(defok 493 (tr (let ((*print-length* 2) (x #Z(1 2 3 4 5)))
		   (declare (off-line-port 2))
		   (collect x)))
  (1 2 3 4 5) 1)
;test 494 removed
(defok 495 (tr (progn (eval '(defun zzt (x) "doc"
				      (declare (optimizable-series-function 2) (special x))
				      (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 1)
(defok 496 (tr (progn (eval '(defun zzt (x) "doc"
				      (declare (optimizable-series-function 2)
					       (propagate-alterability x y))
				      (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 1)
(defok 497 (tr (let ((x #Z(1 2 3)))
		   (declare (optimizable-series-function))
		   (collect x)))
  (1 2 3) 1)

(defok 498 (tr (let ((x '(values t t)))
		   (multiple-value-bind (a b)
		       (map-fn x #'(lambda (z) (values (- z) z)) #Z(1 2 3))
		     (list (collect a) (collect b)))))
  ((-1 -2 -3) (1 2 3)) 2)

(defok 499 (tr (let ((x 2))
		   (multiple-value-bind (a b)
		       (chunk x 2 #Z(a b c d e f))
		     (list (collect a) (collect b)))))
  ((a c e) (b d f)) 3)

(defok 500 (tr (let ((x 2))
		   (multiple-value-bind (a b)
		       (chunk 2 x #Z(a b c d e f))
		     (list (collect a) (collect b)))))
  ((a c e) (b d f)) 4)

(defok 501 (tr (let ((l (list 1 2 3)))
		   (alter (phys-scan-list l) #Z(a b))
		   l))
  (a b 3) 5)

(defok 502 (tr (let ((x #Z(1)))
		   (flet ((a (b) (car b)))
		     (a (collect x))))) 1 6)

(defok 503 (tr (multiple-value-bind (x y) (values #Z(1 2 3) #Z(4 5))
		   (list (collect x) (collect y))))
  ((1 2 3) (4 5)) 7)

(defok 504 (tr (not (let ((x #Z(1 2)))
			(#M1+ x)))) nil 10)

(defok 505 (tr (let ((x (scan '(1 2))))
		   (setq xx x)
		   (collect-sum x))) 3 11)

(defok 506 (tr
	       (let* ((a 3) (z (scan '(1 2))) (b (collect z)))
		 (list (collect (#M(lambda (y) (setq z (#M1+ z)) (1+ y)) z))
		       a (collect z) b))) ((2 3) 3 (3 4) (1 2)) 12)

(defok 507 (tr (let ((x #Z(1 2 3)))
		   (if (null x) 10 20))) 20 13)

(defok 508 (tr (progn (eval '(defun zzt (x)
				 (declare (optimizable-series-function 2))
				 (values (null x) (collect x))))
			(multiple-value-list (zzt #Z(1 2 3)))))
  (nil (1 2 3)) 14)

(defok 509 (tr
	       (let* ((a nil) (z (scan '(1 2))) (b (collect z)))
		 (when (not a) (setq a #Z(9 8)))
		 (list (collect a) (collect z) b))) ((9 8) (1 2) (1 2)) 20)
(defok 510 (tr (let ((x #Z(1 2)))
		   (list (if t 3 (scan-range :upto 3))
			 (collect x)))) (3 (1 2)) 20)
(defok 511 (tr (let ((x #Z(1 2)))
		   (if t (collect-sum #Z(2 3)) x))) 5 20)

(defok 512 (tr (let* ((e #Z(1 2))
			(w (collect e)))
		   (collect (#M(lambda (x) (cons x w)) e)))) ((1 1 2) (2 1 2)) 21)
(defok 513 (tr (let* ((e #Z((1) (2)))
			(w (collect e)))
		   (collect (#M(lambda (x) (cons (car x) w)) e))))
  ((1 (1) (2)) (2 (1) (2))) 21)
(defok 514 (tr (let* ((e #Z(1 2))
			(w (collect e))
			(x (collect-sum e)))
		   (list (collect (#M(lambda (z) (list z x)) e))
			 (collect (#M(lambda (z) (list* z w)) e)))))
  (((1 3) (2 3)) ((1 1 2) (2 1 2))) 21)

(defok 515 (tr (let* ((e #Z(1 -2 3))
			(w (split-if e #'plusp)))
		   (collect (#Mlist e w)))) ((1 1) (-2 3)) 22)
(defok 516 (tr (let* ((e #Z(1 -2 3))
			(w (split-if e #'plusp)))
		   (collect (#Mlist e e w)))) ((1 1 1) (-2 -2 3)) 22)
(defok 517 (tr (let* ((e #Z(1 -2 -4 3)))
		   (multiple-value-bind (w x) (split-if e #'plusp)
		     (collect (#Mlist w x))))) ((1 -2) (3 -4)) 22)

(defok 518 (tr (let* ((e #Z(1 -2 -4 3))
			(w (choose-if #'plusp e)))
		   (collect (#Mlist e w)))) ((1 1) (-2 3)) 23)
(defok 519 (tr (let* ((e #Z(1 -2 -4 3))
			(w (choose-if #'plusp e)))
		   (collect (#Mlist e e w)))) ((1 1 1) (-2 -2 3)) 23)
(defok 520 (tr (let* ((e #Z(1 2)))
		   (collect (catenate e e)))) (1 2 1 2) 23)
(defok 521 (tr (let* ((e #Z(1 2)))
		   (collect (#Mlist e (catenate e e))))) ((1 1) (2 2)) 23)
(defok 522 (tr (let* ((e #Z(1 -2 -3 4)))
		   (collect (#Mlist e (catenate (choose-if #'plusp e)
						(choose-if #'minusp e))))))
  ((1 1) (-2 4) (-3 -2) (4 -3)) 23)
(defok 523 (tr (let* ((e #Z(1 -2 -3 4)))
		   (multiple-value-bind (w x) (split-if e #'plusp)
		     (collect (#Mlist e (catenate w x))))))
  ((1 1) (-2 4) (-3 -2) (4 -3)) 23)

; tests due to bugs found and extensions made from 1/1/90 to 3/20/91

(defok 524 (ton (let ((x 3))
		    (list (collect-last
			   (mapping ((i (scan-range :upto x)))
			     (setq x 4)
			     i))
			  x)))
  (3 4))

(defok 525 (ton (let ((oddp #Z(1 2 3)))
		    (collect (choose-if #'oddp oddp))))
  (1 3))
(defok 526 (ton (collect (choose-if #'(lambda (x) (let ((y (1+ x))) (evenp y))) #Z(1 2 3))))
  (1 3))
(defok 527 (ton (collect (scan-lists-of-lists-fringe '(1 (1 2) (2 3))
						       #'(lambda (x) (let ((y (car x))) (evenp y))))))
  (1 1 2 (2 3)))
(defok 528 (ton (collect (scan-range :upto ((lambda (x) (let ((y (1+ x))) (* 2 y))) 1))))
  (0 1 2 3 4))

(defok 529 (ton (let ((x #Z(1 2 3)))
		    (list (collect-sum x) (collect-sum (catenate x #Z(4 5)))))) (6 15))
(defok 530 (ton (let ((x (scan-range)))
		    (list (collect-sum (subseries x 0 3)) (collect-sum (subseries x 0 5))))) (3 10))
(defok 531 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum (subseries x+ 0 2)) (collect-sum x-)))) (4 -12))
(defok 532 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum x-) (collect-sum (subseries x+ 0 2))))) (-12 4))
(defok 533 (ton (multiple-value-bind (x+ x-) (split-if #Z(1 -2 3 -4 5 -6) #'plusp)
		    (list (collect-sum x+) (collect-first x-)))) (9 -2))

(defok 534 (tr (let ((x #Z(1 2 3))
		       (g (generator (scan '(1 2 3)))))
		   (list (collect-sum x) (next-in g))))
  (6 1) 24)

(defok 535 (td (defun zzz1 (x)
		   (declare (optimizable-series-function))
		   (scan-fn t #'(lambda () 10)
			    #'(lambda (z) (funcall #'(lambda (z) (- z x)) z))
			    #'zerop))
		 (collect (zzz1 2)))
  (10 8 6 4 2))

(defok 536 (tm (collect (car (scan '((1)(2)(3)))))) (1 2 3))
(defok 537 (tm (let ((*x* 0))
		   (collect (progn (list #Z(1 2 3) (incx)))))) ((1 0) (2 0) (3 0)))
(defok 538 (tm (let ((*x* 0))
		   (collect (list #Z(1 2 3) (catenate #Z(a) (incx))))))
	 ((1 a) (2 0) (3 0)))
(defok 539 (tm (let ((e #Z(1 2 3 4)))
		   (collect (choose (evenp e) e)))) (2 4))
(defok 540 (tm (let ((x #Z(1 nil (a) 3 4)))
		   (collect (and (numberp x) (oddp x)))))
	 (t nil nil t nil))
(defok 541 (tm (let ((*x* 1))
		   (let* ((x #Z(1 nil (a) 3 4)) (z (list x)))
		     (when (null x) (incx))
		     (collect (if (numberp x) *x* z)))))
	 (1 (nil) ((a)) 2 2))
(defok 542 (tm (let ((*x* 0))
		   (let* ((x (car (scan '((1) (2) (3)))))
			  (y (1+ x))
			  (z (collect-sum (* x y))))
		     (incx (list x y 4))
		     (incx z)
		     (list (collect (list x (catenate #Z(a) (incx 'b)))) *x*))))
	 (((1 a) (2 b) (3 b)) 5))

(defok 543 (ton (let* ((x 3) (e (make-series x))) (collect e))) (3))

;Here, temporarily not working because SETQ processing
;assumes the straight-line restriction is satisfied.
;(defok 543.1 (tm (let* ((x #Z(1 nil (a) 3 4)) (y (+ 0 1)) (z (list x)))
;		     (when (null x) (setq y (1+ y)))
;		     (collect (if (numberp x) y z))))
;  (1 (nil) ((a)) 2 2))

;Here, note this is an example where the optimization is not correctness
;preservering due to side-effects.  Also note that more work happens than
;you might think in the optimized expression in any case.
;Here if not testing mapping, you get an odd message about non-series to series dflow
;before you get the not-straight line error message.
;(defok 543.2 (let ((*x* 0))
;		 (let ((e #Z(1 2 3 4)))
;		   (list (if (numberp *x*) (collect-sum e) (collect-sum (#Mincx e)))
;			 *x*)))
;  (10 4))
;Here also note this even more extreme case.  It would work right if the
;whole inner-let were nested in where it could be.
;(defok 543.3 (tm (let ((*x* 0))
;		     (let ((e #Z(1 2 3 4)))
;		       (list (if (numberp *x*) 0 (collect-sum (#Mincx e)))
;			     *x*))))
;  (0 4))

(defok 544 (ton (let ((a #'car)) (funcall (or a a) '(1 2)))) 1)

;here temporarily not working
;(defok 544.1 (t (let ((e #Z(a b)))
;		    (list (collect (compiler-let ((*c1* 3) *c2*)
;				     (#Mc1-c2-macro e)))
;			  (collect (compiler-let ((*c2* 4))
;				     (#Mc1-c2-macro e))))))
;  (((3 nil a) (3 nil b)) ((1 4 a) (1 4 b))))

(defok 545 (ton (let ((mask (mapping ((a (scan '(a b c))))
				a)))
		    (collect mask))) (a b c))
(defok 546 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (until-if #'(lambda (obj) (eql obj end)) data))))
  (1 2))
(defok 547 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (choose-if #'(lambda (obj) (eql obj end)) data))))
  (3))
(defok 548 (ton (let ((end 3)
			(data #Z(1 2 3 4)))
		    (collect (split-if data #'(lambda (obj) (eql obj end))))))
  (3))

;; Additional tests

(defok 549 (td (defun foo4 (number)
		   (declare (integer number))
		   (1+ number))
		 (collect (scan-range :below (foo4 4))))
  (0 1 2 3 4))

(defok 550 (ton (collect (mapping ((x #Z(1 2)))
			     (do ((i 1 (1+ i))
				  a (b) (c 0)
				  (r 0 (+ x r)))
				 ((> i x) r)
			       (setq b i a i)
			       (if (> (+ a b c) 100) (return nil))))))
  (1 4))
(defok 551 (tw (defun ugh6 (a)
		   (declare (optimizable-series-function) (ignore a))
		   (scan a)))
  ugh6 51)
;here temporarily not working, due to bug in the code.
; #+symbolics
;(defok 552 (tw (defun ugh5 (a)
;		    (declare (optimizable-series-function))
;		    (scan '(1 2 3))))
;  ugh5 50)

(defok 553 (ton (collect 'string #Z(#\B #\A #\R))) "BAR")
(defok 554 (ton (collect 'simple-string #Z(#\B #\A #\R))) "BAR")
(defok 555 (ton (collect 'base-string #Z(#\B #\A #\R))) "BAR")
(defok 556 (ton (collect 'simple-base-string #Z(#\B #\A #\R))) "BAR")
(defok 557 (ton (collect 'bit-vector #Z(1 0 1 1))) #*1011)
(defok 558 (ton (collect 'simple-bit-vector #Z(1 0 1 1))) #*1011)

(defok 559 (td (defun dtest ()
		 (with-open-file (s "/tmp/foo" :direction :output :if-exists :supersede)
		   (format s "Hello, world!~%"))
		 (let* ((foo (scan 'list '(1 2 3)))
			(max (collect-max foo))
			(bar (with-open-file (file "/tmp/foo" :direction :input)
			       (loop for line of-type (or null simple-string) =
				     (read-line file nil)
				     until (null line)
				     ))))
		   max))
	       (collect (scan-range :below (dtest))))
  (0 1 2))

;; Test from bug report by Dirk Gerrits, 2005-01-16.
(defstruct vec x y z)
(defun scan-vec (vec)
    (declare (optimizable-series-function))
    (to-alter (make-series (vec-x vec) (vec-y vec) (vec-z vec))
              #'(lambda (new-value index v)
                  (ecase index
                    (0 (setf (vec-x v) new-value))
                    (1 (setf (vec-y v) new-value))
                    (2 (setf (vec-z v) new-value))))
              (scan-range :from 0)
	      (make-series vec vec vec)))

;; We should test non-opt too, but that fails.  I don't know how to
;; fix the non-opt version so that this test passes.
(defok 560 (to (let ((vec (make-vec :x 1 :y 2 :z 3)))
		 (alter (scan-vec vec) (series 0))
		 (list (vec-x vec) (vec-y vec) (vec-z vec))))
  (0 0 0))

;; Bug 434120
(defok 561 (ton (let* ((m1 (make-array 8 :element-type 'fixnum))
		       (m2 (make-array 8 :element-type 'fixnum))
		       (m3 (make-array 9 :element-type 'fixnum)))
		  (loop :for j :of-type fixnum :from 0 :below 8
		     :do (setf (aref m1 j) (+ 1 j))
		     :do (setf (aref m2 j) (+ 1 j))
		     :do (setf (aref m3 j) 0))
		  (setf (aref m3 8) 0)
		  (multiple-value-bind (s1 s2 s3)
		      (scan-multiple '(simple-array fixnum) m1 m2 m3)
		    (alter s3 (map-fn 'fixnum #'* s1 s2)))
		  (coerce m3 'list)))
  (1 4 9 16 25 36 49 64 0))

;; Is there a better way to do this?  Print to a string seems rather
;; fragile.
(defok 562 (ton
	    (let ((*print-length* 20))
	      (subseq (with-output-to-string (s)
			(print (series t nil) s)) 1)))
  "#Z(T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL ...) ")

(defok 563 (ton
	     (let ((*print-length* 20))
	      (subseq (with-output-to-string (s)
			(print (positions (series t nil)) s)) 1)))
  "#Z(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 ...) ")

;;; Some simple consistency tests.  (Some of these were broken by
;;; changes in series, so we include them here to prevent these from
;;; happening again!)

(defok 1000 (ton (coerce (collect 'vector (scan '(1 2 3))) 'list))
  (1 2 3))

(eval-when (compile load eval)
(series::defconst-once +constant+
  '#(1 2 3 4))
)

(defok 1001 (ton (collect 'list (scan 'vector +constant+)))
  (1 2 3 4))

;;; New basic tests
(defvar *str* (make-test-struct))
(edeftest 3000
  (ton (elt (collect 'vector (map-fn 'test-struct #'identity
				     (scan (list *str*))))
	    0))
  *str*)

;;; New GATHERING tests

(defok 5000
  (ton (let* ((g 1)
	 (res (gathering ((y collect-sum))
		(declare (indefinite-extent y))
		(next-out y 1) (next-out y 2) (setq g y))))
    (declare (ignore res))
    (result-of g))) 3)
(defok 5001 (ton (fgathering ((y collect-sum))
			     (fgather-next y 1) (fgather-next y 2))) 3)
(defok 5002
  (ton (let* ((g 1)
	 (res (fgathering ((y collect-sum))
		(declare (indefinite-extent #'y))
		(fgather-next y 1) (fgather-next y 2) (setq g #'y))))
    (declare (ignore res))
    (result-of g))) 3)
(defok 5003 (ton (progn (if (probe-file test-file) (delete-file test-file))
			 (prog1 (list (fgathering ((g (lambda (x) (collect-file test-file x))))
						 (fgather-next g 3)
						 (fgather-next g 4))
				      (collect (scan-file test-file)))
			   (if (probe-file test-file) (delete-file test-file))))) (t (3 4)))
(defok 5004 (ton (multiple-value-list
		      (fgathering ((x (lambda (x) (collect x)))
				   (y collect-sum))
				 (dotimes (i 3)
				   (fgather-next y i)
				   (if (evenp i) (fgather-next x (* i 10))))))) ((0 20) 3))
;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.t. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.t. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.t. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.t. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.
;------------------------------------------------------------------------

