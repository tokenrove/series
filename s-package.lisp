;;;;-*- Mode: lisp; Package: (SERIES :use "COMMON-LISP" :colon-mode :external) -*-

;;;; The package initialization stuff is done here now, instead of in s-code.lisp.  This is based a comment by Bruno Haible who said
;;;;
;;;; "The important point is that the packages setup when you compile
;;;; a file must be identical to the packages setup when you load the
;;;; file....  What will help, 100%, is to have a file which issues
;;;; all the necessary `defpackage' forms, and make sure this file is
;;;; loaded before anything else and before any `compile-file'.

;;;; $Id: s-package.lisp,v 1.14 2008/10/27 14:24:53 rtoy Exp $
;;;;
;;;; $Log: s-package.lisp,v $
;;;; Revision 1.14  2008/10/27 14:24:53  rtoy
;;;; Support SCL.  Just add scl conditionalizations where we have cmucl
;;;; ones, and convert uppercase symbols and symbol-names to use
;;;; symbol-name and uninterned symbols.  This is to support scl's default
;;;; "modern" mode.
;;;;
;;;; Changes from Stelian Ionescu.
;;;;
;;;; Revision 1.13  2008/10/27 14:19:23  rtoy
;;;; Add support for ecl.
;;;;
;;;; (From Stelian Ionescu.)
;;;;
;;;; Revision 1.12  2004/12/15 17:18:57  rtoy
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
;;;; Revision 1.11  2003/06/08 12:53:21  rtoy
;;;; From Alexey Dejneka:
;;;;
;;;; o Add support for SBCL
;;;; o Import COMPILER-LET from SBCL.
;;;;
;;;; Revision 1.10  2001/12/23 17:11:17  rtoy
;;;; COMPILER-LET is in the EXT package in Clisp now.
;;;;
;;;; Revision 1.9  2001/12/23 16:54:44  rtoy
;;;; Make series support Allegro "modern" lisp with its case-sensitive
;;;; reader.  Mostly just making every that needs to be lower case actually
;;;; lower case.  The tests still work.
;;;;
;;;; Revision 1.8  2000/09/22 15:58:39  rtoy
;;;; Add support for MCL (from Rainer Joswig).
;;;;
;;;; Revision 1.7  2000/03/28 10:23:49  matomira
;;;; polycall et all are now tail recursive.
;;;; LETIFICATION WORKS COMPLETELY!!
;;;;
;;;; Revision 1.8  2000/03/14 10:48:10  matomira
;;;; Workaround for ACL 5.0.1 TAGBODY bug added.
;;;; ALL-TIME SERIES BUG FIX: wrappers now inserted more precisely.
;;;; Abstracted use of wrapper component of frags.
;;;; GENERATOR deftyped to CONS, not LIST, when necessary.
;;;;
;;;; Revision 1.7  2000/03/11 17:36:34  matomira
;;;; Added eval-when compatibility magic.
;;;;
;;;; Revision 1.6  2000/03/03 19:17:15  matomira
;;;; Series 2.0 - Change details in RELEASE-NOTES.
;;;;
;;;; Revision 1.4  1999/12/01 16:09:05  toy
;;;; Need to import compiler-let from the extensions package in CMUCL.
;;;;
;;;; Revision 1.3  1999/09/14 20:19:43  toy
;;;; Export the new function collect-stream.
;;;;
;;;; Revision 1.2  1999/07/02 20:38:13  toy
;;;; Forgot a few items from s-code.lisp, and had to put the in-package
;;;; stuff back into s-code.lisp.
;;;;
;;;; Revision 1.1  1999/07/02 19:52:32  toy
;;;; Initial revision
;;;;

;;; Add a feature to say if we are a Lisp that can hack ansi-cl style
;;; stuff, as far as series goes anyway.  This implies:
;;;	ansi style packages (DEFPACKAGE, CL not LISP as main package)
;;;
;;; if you don't have this you need to make the LISP package have CL
;;; as a nickname somehow, in any case.
;;;
#+gcl
(eval-when (compile load eval)
  (unless (find-package "CL")
    (rename-package "LISP" "COMMON-LISP" '("LISP" "CL"))))

;;; Note this is really too early, but we need it here
#+(or draft-ansi-cl draft-ansi-cl-2 ansi-cl allegro cmu scl sbcl Genera Harlequin-Common-Lisp CLISP mcl)
(cl:eval-when (load eval compile)
  (cl:pushnew ':series-ansi cl:*features*))

#+allegro
(cl:eval-when(compile load eval)
  ;; Simple way to figure out if we are running Allegro modern lisp
  ;; with its case-sensitive reader.
  (when (find-package "cl")
    (cl:pushnew ':allegro-modern cl:*features*)))

#+sbcl
(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :sb-cltl2))

(defpackage #:series
    (:use #:cl)
  (:export 
   ;;(2) readmacros (#M and #Z)

   ;;(5) declarations and types (note dual meaning of series)
   #:optimizable-series-function  #:off-line-port  ;series
   #:series-element-type  #:propagate-alterability
   #:indefinite-extent
   
   ;;(10) special functions
   #:alter #:to-alter #:encapsulated #:terminate-producing
   #:next-in #:next-out
   #:generator 
   #:gatherer  #:result-of 
   #:gather-next #:gather-result #:gatherlet #:gathering
   #:fgather-next #:fgather-result #:fgatherlet #:fgathering

   ;;(55) main line functions
   #:make-series #:series #:scan #:scan-multiple #:scan-range
   #:scan-sublists #:scan-fn #:scan-fn-inclusive #:scan-lists-of-lists
   #:scan-lists-of-lists-fringe #:scan-file #:scan-stream #:scan-hash #:scan-alist
   #:scan-plist #:scan-symbols #:collect-fn #:collect #:collect-append
   #:collect-nconc #:collect-file #:collect-alist #:collect-plist
   #:collect-hash #:collect-length #:collect-stream
   #:collect-sum #:collect-product #:collect-max #:collect-min
   #:collect-last #:collect-first #:collect-nth
   #:collect-and #:collect-or #:previous #:map-fn #:iterate #:mapping
   #:collecting-fn #:cotruncate #:latch #:until #:until-if #:positions
   #:choose #:choose-if #:spread #:expand #:mask #:subseries #:mingle
   #:catenate #:split #:split-if #:producing #:chunk

   ;;(5) variables
    #:*series-expression-cache*
    #:*last-series-loop*
    #:*last-series-error*
    #:*suppress-series-warnings*
    )
  (:shadow
   #:let #:let* #:multiple-value-bind #:funcall #:defun
   #+(or cmu scl) #:collect #+(or cmu scl) #:iterate)
  #+Harlequin-Common-Lisp
  (:import-from "LISPWORKS" "COMPILER-LET")
  #+Genera
  (:import-from "LISP" "COMPILER-LET")
  #+allegro
  (:import-from #:cltl1 #:compiler-let)
  #+CLISP
  (:import-from "EXT" "COMPILER-LET")
  #+(or cmu scl)
  (:import-from :ext #:compiler-let)
  #+mcl
  (:import-from "CCL" "COMPILER-LET")
  #+sbcl
  (:import-from "SB-CLTL2" "COMPILER-LET")
  #+ecl
  (:import-from "SI" "COMPILER-LET")
)

#-(or series-ansi)
(export ;74 total concepts in the interface
  '(;(2) readmacros (#M and #Z)

    ;(5) declarations and types (note dual meaning of series)
    indefinite-extent
    optimizable-series-function off-line-port ;series
    series-element-type propagate-alterability

    ;(10) special functions
    alter to-alter encapsulated terminate-producing
    next-in next-out generator gatherer result-of 
    gather-next gather-result fgather-next fgather-result
    gathering fgathering gatherlet fgatherlet

    ;(55) main line functions
    make-series series scan scan-multiple scan-range scan-sublists scan-fn
    scan-fn-inclusive scan-lists-of-lists scan-lists-of-lists-fringe scan-file
    scan-stream scan-hash scan-alist scan-plist scan-symbols collect-fn collect
    collect-append collect-nconc collect-file collect-alist collect-plist
    collect-hash collect-length
    collect-sum collect-product collect-max collect-min
    collect-last collect-first collect-nth collect-and collect-or
    previous map-fn iterate mapping collecting-fn cotruncate
    latch until until-if positions choose choose-if
    spread expand mask subseries mingle catenate split split-if
    producing chunk 

    ;(5) variables
    *series-expression-cache*
    *last-series-loop*
    *last-series-error*
    *suppress-series-warnings*))

#-(or series-ansi)
(eval-when (compile load eval)
  (in-package "SERIES" :use '("LISP"))
  (shadow '(let let* multiple-value-bind funcall defun eval-when #+(or cmu scl) collect #+(or cmu scl) iterate))
) ; end of eval-when

#-(or series-ansi)
(cl:eval-when (compile load eval)
  (defmacro eval-when ((&rest times) &body body)
    `(cl:eval-when ,(append
		     (when (member :compile-toplevel times)
		       '(compile))
		     (when (member :load-toplevel times)
		       '(load))
		     (when (member :execute times)
		       '(eval)))
       ,@body))
) ; end of eval-when
