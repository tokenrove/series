;-*- Mode: lisp; syntax:ANSI-COMMON-LISP; Package: (SERIES :use "COMMON-LISP" :colon-mode :external) -*-

;This is the November, 26 1991 version of
;Richard C. Waters' Series macro package.

;The standard version of this program is available by anonymous FTP
;from MERL.COM in the files /pub/series/s*.  If you have gotten the file
;from somewhere else, or copied the files a long time ago, you might
;consider copying them from MERL.COM now to obtain the latest version.

;;;; $Id: s-code.lisp,v 1.28 1999/06/30 19:34:49 toy Exp $
;;;;
;;;; This is modified version of Richard Water's Series package.
;;;;
;;;; $Log: s-code.lisp,v $
;;;; Revision 1.28  1999/06/30 19:34:49  toy
;;;; "Pekka P. Pirinen" <pekka@harlequin.co.uk> says:
;;;;
;;;; 	"Tests 289, 290, 417, and 426 [on Liquid CL] fail because of
;;;; 	incorrect type decls generated in ADD-PHYSICAL-OUT-INTERFACE.
;;;; 	The variable NEW-OUT is used for two conflicting purposes; The
;;;; 	fix is to split it into two."
;;;;
;;;; Thanks!
;;;;
;;;; Revision 1.27  1999/04/29 22:06:49  toy
;;;; Fix some problems in aux-init not handling some strings and
;;;; bit-vectors correctly.
;;;;
;;;; Revision 1.26  1999/04/23 17:51:24  toy
;;;; For CMUCL, decode-seq-type didn't handle base-string types.  Make it
;;;; work.
;;;;
;;;; In aux-init, change the test for simple-string to be string instead.
;;;;
;;;; Revision 1.25  1999/04/15 17:09:41  toy
;;;; Rework aux-init once again.  The bit-vector entry goes away, and the
;;;; entry for vector and simple-array are changed to create the proper
;;;; types when the length is not given.  I hope this is the last change
;;;; here! :-)
;;;;
;;;; Revision 1.24  1999/04/15 16:35:54  toy
;;;; In aux-init, the bit-vector entry is actually applicable to all Lisps
;;;; because bit-vectors would get initialized to #() instead of #*, which
;;;; is wrong.  Remove the CMU conditionalization.
;;;;
;;;; Revision 1.23  1999/04/13 16:51:32  toy
;;;; o  Back up the gensym changes made in 1.21 because CLISP doesn't like
;;;;    it.
;;;; o  type-expand for CLISP is in the LISP package.
;;;; o  For CMUCL, in aux-init need to check for bit-vector before
;;;;    simple-array because simple-bit-vector was done as simple-array
;;;;    instead of bit-vector, which is wrong.
;;;;
;;;; Revision 1.22  1999/04/09 12:32:26  toy
;;;; Add definition of canonical-type for CLISP.
;;;;
;;;; Revision 1.21  1999/04/08 21:46:08  toy
;;;; Use gensym instead of gentemp, which is deprecated in ANSI CL.
;;;;
;;;; Revision 1.20  1999/04/08 21:41:16  toy
;;;; Add CLISP to the Series-ANSI.  Then need to import COMPILER-LET.
;;;;
;;;; In aux-init, the bit-vector entry is only for CMUCL which
;;;; canonicalizes (vector bit) into bit-vector.
;;;;
;;;; Revision 1.19  1999/04/06 18:36:13  toy
;;;; Some fixes from Arthur Lemmens <lemmens@simplex.nl>:
;;;; MAKE-SEQUENCE was being called with things like (BIT-VECTOR BIT) and
;;;; (STRING STRING-CHAR).  Change DECODE-SEQ-TYPE to convert BIT-VECTOR's
;;;; and STRING's to the the underlying array types.  Also change
;;;; STRING-CHAR to CHARACTER.
;;;;
;;;; This change necessitates adding a BIT-VECTOR case in AUX-INIT. (This
;;;; is probably only need by CMUCL where CANONICAL-TYPE actually does
;;;; something.)
;;;;
;;;; Revision 1.18  1998/06/12 20:45:23  toy
;;;; An addition to scan-hash for CLISP.  This reduces consing and should
;;;; be at least as fast.  From Bruno Haible.
;;;;
;;;; Also, defstruct alter-fn needs to be defined twice for CLISP and
;;;; probably also for CMUCL.  From Brun Haible.
;;;;
;;;; Revision 1.17  1998/06/10 18:59:51  toy
;;;; Fixed long-standing bug in scan-range where the initial values of the
;;;; loop variables didn't match the specified :type.  I'm not sure this is
;;;; the correct solution, but it seems to produce the desired macro
;;;; expansions.  I would be interested in a better solution, if possible.
;;;;
;;;; Revision 1.16  1998/06/08 17:34:59  toy
;;;; Couple of small changes for CLISP.
;;;;
;;;; Revision 1.15  1998/05/26 16:23:25  toy
;;;; One last fix from Reginald:  Don't make series a declaration.  With
;;;; this fix, this should now run correctly for lispworks.
;;;;
;;;; Revision 1.14  1998/05/24 19:19:22  toy
;;;; Fixes from Reginald S. Perry were incompletely applied:  Forgot to
;;;; import compiler-let and messed up a fix for uninterning SERIES for
;;;; Harlequin.
;;;;
;;;; Revision 1.13  1998/05/21 15:18:27  toy
;;;; Added a few fixes from "Reginald S. Perry" <reggie@aa.net> to make
;;;; this work with LWW.
;;;;
;;;; Revision 1.12  1997/10/02 13:36:45  toy
;;;; Forgot to export scan-stream.
;;;;
;;;; Revision 1.11  1997/10/02 13:25:18  toy
;;;; Added canonical-type function to extract out the "real" type if
;;;; something has been deftype'd.  Changed code to support this new
;;;; function.
;;;;
;;;; Do a better job in decode-seq-type.  Needed for CMUCL to complain
;;;; less.
;;;;
;;;; Added scan-stream series function.  Just like scan-file, except that
;;;; we have a stream instead of a file name.
;;;;
;;;; Revision 1.10  1997/01/16 14:38:27  toy
;;;; Took out part of Tim's last change: Removed tests for :defpackage
;;;; feature.  Gcl with M. Kantrowitz's defpackage doesn't work and I'm too
;;;; lazy to figure out why.
;;;;
;;;; Revision 1.9  1997/01/16 14:26:44  toy
;;;; Some more patches from Tim (tfb@aiai.ed.ac.uk):  Conditionalize on
;;;; :defpackage too for package stuff.
;;;;
;;;; Revision 1.8  1997/01/16 14:23:59  toy
;;;; Put in changes from Tim (tfb@aiai.ed.ac.uk) to conditionalize on
;;;; Series-ANSI.
;;;;
;;;; Revision 1.7  1997/01/16 14:20:23  toy
;;;; GCL normally doesn't have defpackage, so don't use defpackage form.
;;;; It also doesn't have a "CL" package, so rename "LISP" to
;;;; "COMMON-LISP" with appropriate nicknames.
;;;;
;;;; Revision 1.6  1997/01/13 17:47:19  toy
;;;; Added some changes from Tim Bradshaw (tfb@aiai.ed.ac.uk):
;;;;   Replace "LISP:" with "CL:"
;;;;   Added :import-from for Genera and Allegro.
;;;; With these changes, everything still works under CMUCL.
;;;;
;;;; Revision 1.5  1997/01/13 16:04:11  toy
;;;; Don't install the package on load.  Let the user do it himself.
;;;;
;;;; Revision 1.4  1997/01/10 22:37:03  toy
;;;; A patch from Tim Bradshaw that fixes a bug.  The code walker
;;;; improperly handles nth-value.  Doesn't seem to have any affect in
;;;; CMUCL but can be seen in others like lispm.
;;;;
;;;; Revision 1.3  1997/01/07 19:09:30  toy
;;;; Changed aux-init to initialize variables better.  I think it handles
;;;; just about all cases now.
;;;;
;;;; Modified clean-dcls to handle simple-arrays.
;;;;
;;;; Changed collect so that it handles types better by passing the correct
;;;; type to fragL.  This allows better optimization by the compiler (at
;;;; least for CMUCL).
;;;;
;;;; Added code at the end so that the package is installed whenever it's
;;;; loaded.  You don't have to explicitly install the package anymore.
;;;; However, there's a bug:  It assumes you were originally in the USER
;;;; package.  This needs to be fixed.
;;;;
;;;; Revision 1.2  1997/01/07 18:58:51  toy
;;;; Changes from Paul Werkowski to make series work/run under CMUCL.
;;;; Raymond Toy added the defpackage stuff.  There are probably other
;;;; changes here, but I wasn't careful to keep everything straight,
;;;; unfortunately.
;;;;
;;;;
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

;This file implements efficient computation with series
;expressions in Common Lisp.  The functions in this file
;are documented in Appendices A and B of Common Lisp: the Language,
;Second Edition, Guy L. Steele Jr, Digital press, 1990,
;and in even greater detail in
;  MIT/AIM-1082 and MIT/AIM-1083 both dated December 1989
;These reports can be obtained by writing to:

;               Publications
;               MIT AI Laboratory
;               545 Tech. Sq.
;               Cambridge MA 02139

;This file attempts to be as compatible with standard Common Lisp as possible.
;It has been tested on the following Common Lisps to date (1/18/89).
;  Symbolics CL version 8.
;  LUCID CL version 3.0.2 on a sun.
;  Allegro CL version 1.2.1 on a Macintosh.
;  LispWorks CL version 2.1.

;The companion file "STEST.LISP" contains several hundred tests.  You should
;run these tests after the first time you compile this file on a new system.

;The companion file "SDOC.TXT" contains brief documentation.


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
#+(or draft-ansi-cl draft-ansi-cl-2 ansi-cl allegro CMU Genera Harlequin-Common-Lisp CLISP)
(cl:eval-when (load eval compile)
  (cl:pushnew ':SERIES-ANSI cl:*features*))

(provide "SERIES")

#+(or Series-ANSI)
(defpackage "SERIES"
    (:use "CL")
  (:export 
   ;;(2) readmacros (#M and #Z)

   ;;(5) declarations and types (note dual meaning of series)
   "OPTIMIZABLE-SERIES-FUNCTION"  "OFF-LINE-PORT"  ;series
   "SERIES-ELEMENT-TYPE"  "PROPAGATE-ALTERABILITY"

   ;;(10) special functions
   "ALTER" "TO-ALTER" "ENCAPSULATED" "TERMINATE-PRODUCING"
   "NEXT-IN" "NEXT-OUT" "GENERATOR" "GATHERER" "RESULT-OF" "GATHERING"

   ;;(55) main line functions
   "MAKE-SERIES" "SERIES" "SCAN" "SCAN-MULTIPLE" "SCAN-RANGE"
   "SCAN-SUBLISTS" "SCAN-FN" "SCAN-FN-INCLUSIVE" "SCAN-LISTS-OF-LISTS"
   "SCAN-LISTS-OF-LISTS-FRINGE" "SCAN-FILE" "SCAN-STREAM" "SCAN-HASH" "SCAN-ALIST"
   "SCAN-PLIST" "SCAN-SYMBOLS" "COLLECT-FN" "COLLECT" "COLLECT-APPEND"
   "COLLECT-NCONC" "COLLECT-FILE" "COLLECT-ALIST" "COLLECT-PLIST"
   "COLLECT-HASH" "COLLECT-LENGTH" "COLLECT-SUM" "COLLECT-MAX"
   "COLLECT-MIN" "COLLECT-LAST" "COLLECT-FIRST" "COLLECT-NTH"
   "COLLECT-AND" "COLLECT-OR" "PREVIOUS" "MAP-FN" "ITERATE" "MAPPING"
   "COLLECTING-FN" "COTRUNCATE" "LATCH" "UNTIL" "UNTIL-IF" "POSITIONS"
   "CHOOSE" "CHOOSE-IF" "SPREAD" "EXPAND" "MASK" "SUBSERIES" "MINGLE"
   "CATENATE" "SPLIT" "SPLIT-IF" "PRODUCING" "CHUNK"

   ;;(5) variables
    "*SERIES-EXPRESSION-CACHE*"
    "*LAST-SERIES-LOOP*"
    "*LAST-SERIES-ERROR*"
    "*SUPPRESS-SERIES-WARNINGS*"
    )
  (:shadow
   "LET" "LET*" "MULTIPLE-VALUE-BIND" "FUNCALL" "DEFUN" #+cmu "COLLECT" #+cmu "ITERATE")
  #+Harlequin-Common-Lisp
  (:import-from "LISPWORKS" "COMPILER-LET")
  #+Genera
  (:import-from "LISP" "COMPILER-LET")
  #+Allegro
  (:import-from "CLTL1" "COMPILER-LET")
  #+CLISP
  (:import-from "LISP" "COMPILER-LET")
)

#+(or Series-ANSI)
(in-package "SERIES")

#-(or Series-ANSI)
(progn
  (in-package "SERIES" :use '("LISP"))
  (shadow '(let let* multiple-value-bind funcall defun)))

(defvar *series-forms* '(let let* multiple-value-bind funcall defun)
  "Forms redefined by Series.")

#-(or Series-ANSI)
(export ;74 total concepts in the interface
  '(;(2) readmacros (#M and #Z)

    ;(5) declarations and types (note dual meaning of series)
    optimizable-series-function off-line-port ;series
    series-element-type propagate-alterability

    ;(10) special functions
    alter to-alter encapsulated terminate-producing
    next-in next-out generator gatherer result-of gathering

    ;(55) main line functions
    make-series series scan scan-multiple scan-range scan-sublists scan-fn
    scan-fn-inclusive scan-lists-of-lists scan-lists-of-lists-fringe scan-file
    scan-stream scan-hash scan-alist scan-plist scan-symbols collect-fn collect
    collect-append collect-nconc collect-file collect-alist collect-plist
    collect-hash collect-length collect-sum collect-max collect-min
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

(declaim (declaration optimizable-series-function off-line-port
		      ;; Genera barfs at this (correctly I think)
		      #-(or Genera lispworks4) series
		      propagate-alterability))

(defvar *suppress-series-warnings* nil
  "Suppress warnings when the restrictions are violated.")

(defvar *series-expression-cache* T
  "Avoids multiple expansions")

(defvar *last-series-loop* nil
  "Loop most recently created by SERIES.")

(defvar *last-series-error* nil
  "Info about error found most recently by SERIES.")

(defvar *series-implicit-map* nil 
  "T enables implicit mapping in optimized expressions")

(cl:defun install (&key (pkg *package*) (macro T) (shadow T) (implicit-map nil)
			  (remove nil))
  (setq *series-implicit-map* implicit-map)
  (when (not (packagep pkg)) (setq pkg (find-package pkg)))
  (when (not remove)
    (when macro
      (set-dispatch-macro-character #\# #\Z (function series-reader))
      (set-dispatch-macro-character #\# #\M (function abbreviated-map-fn-reader)))
    (when (not (eq pkg (find-package "SERIES")))
      ;This is here because UNTIL and COLLECT are loop clauses.
      (cl:multiple-value-bind (sym code) (find-symbol "UNTIL" pkg)
	(when (and sym (eq code :internal)
		   (not (boundp sym)) (not (fboundp sym)) (null (symbol-plist sym)))
	  (unintern sym pkg)))
      (cl:multiple-value-bind (sym code) (find-symbol "COLLECT" pkg)
	(when (and sym (eq code :internal)
		   (not (boundp sym)) (not (fboundp sym)) (null (symbol-plist sym)))
	  (unintern sym pkg)))
      #+(or cmu Harlequin-Common-Lisp)
      (cl:let ((ext (find-package "EXTENSIONS")))
	;; CMU Lisp has COLLECT and ITERATE in the EXTENSIONS package.
	;; Make them go away.
	(and ext
	     (unintern 'collect ext)
	     (unintern 'iterate ext))
	(unintern 'series "COMMON-LISP-USER"))
      
      (use-package "SERIES" pkg)
      (when shadow (shadowing-import *series-forms* pkg))))
  (when (and remove (member (find-package "SERIES") (package-use-list pkg)))
    (unuse-package "SERIES" pkg)
    (dolist (sym (intersection *series-forms* (package-shadowing-symbols pkg)))
      (unintern sym pkg)))
  T)

;Internally used special variables.  Every one is collected here except some
;scan templates used in macro expansion.

(defvar *optimize-series-expressions* T)
(defvar *in-series-expr* nil "the topmost series expression")
(defvar *testing-errors* nil "Used only be the file of tests")
(defvar *not-straight-line-code* nil "not-nil if nested in non-straight-line code")

(eval-when (eval load compile) 
  (proclaim
    '(special *graph*                   ;list of frags in expression
	      *renames*                 ;alist of variable renamings
	      *user-names*              ;series::let var names used by user
	      *env*                     ;environment of containing series macro call
	      *call*                    ;bound to whole form when running optimizer
	      *being-setqed*            ;T if in the assignment part of a setq
	      *fn*                      ;FN being scanned over code
	      *type*)))                 ;Communicates types to frag instantiations

;*renames* has three kinds of entries on it.  Each is a cons of a
;variable and something else:  (type 1 cannot ever be setqed.)
; 1- a ret, var is a series::let var or a series::lambda var.  You
;  can tell between the two because series::lambda var frags are not in *graph*.
; 2- a new var, var is an aux var.
; 3- nil, var is rebound and protected from renaming.

(defvar *short-hand-types*
	'(array atom bignum bit bit-vector character common compiled-function
	  complex cons double-float fixnum float function hash-table integer
	  keyword list long-float nil null number package pathname random-state
	  ratio rational readtable sequence short-float simple-array
	  simple-bit-vector simple-string simple-vector single-float standard-char
	  stream string string-char symbol t vector series) "table 4.1 from CLTL")

(defvar *standard-function-reader* (get-dispatch-macro-character #\# #\'))

;             ---- UTILITIES FOR MANIPULATING FRAGMENTS ----

(eval-when (eval load compile)

(defvar end 'END "used to force copying of frag lists")

;The key internal form is an entity called a frag (short for fragment).

(defstruct (frag (:conc-name nil) (:type list) :named)
  (code :||)      ;the surface code corresponding to this, for error messages
  (marks 0)       ;mark bits used in sweeps over a graph
  (must-run nil)  ;indicates contains computation that must be run completely
  (args nil)      ;a list of sym structures for the args of the frag.
  (rets nil)      ;a list of sym structs for the return values of the frag.
  (aux nil)       ;the auxiliary variables if any and their types.
  (alterable nil) ;specifications for alterable outputs
  (prolog nil)    ;list of forms (without labels).
  (body nil)      ;list of forms (possibly containing labels).
  (epilog nil)    ;list of forms (without labels).
  (wrappers nil)) ;functions that wrap forms around the whole loop.

;There cannot be any redundancy in or between the args and aux.  Each
;ret variable must be either on the args list or the aux list.  The args
;and ret have additional data as discussed below.  The aux is just a
;list of lists of a symbol and a type specifier.  Every symbol used in a
;frag which could possible clash with other frags (eg args, rets, aux,
;and also labels) must be gensyms and unique in the whole world.

;The order of the args is important when the frag is first
;instantiated and funcalled.  However, it does not matter after that.
;Similarly, the order of the rets also matters at the time it is
;instantiated, and at the time that a whole expression is turned into
;one frag, but it does not matter at other times.

;There are two basic kinds of frags, series frags and non-series
;frags.  A non-series frag is a frag which just has a simple
;computation which has to be performed only once.  The rets and
;args must be non-series values, and the body and epilog must be
;empty.  (The code below maintains the invariant that if all the
;ports of a frag are non-series then the body and epilog are
;empty.)

;a frag has three internal parts so that a wide variety of fragmentary
;series functions can be compressed into a single frag.

;Inside frags there is a label which has a special meaning.
; END is used as the label after the end of the loop created.  If the
;    body of a fragment contains (go END) then the fragment is an
;    active terminator.

;If a programmer uses these symbols in his program, very bad things could
;happen.  However, it is in the series package, so there should not be any
;conflict problems.  the code in this file assumes in many places that no
;symbol in the "SERIES" package can possibly clash with a user symbol.

;The code field is used solely to generate error messages.  However, it is
;never the less very important.  In particular, it is important that the
;code field only contain things that were actually in the user's source
;code.  It is also important that it always contain something.

;  There are several reasons why the code might end up being something the
;user did not write.  The foremost reason is macro expansion.  It might be
;the result of some expansion that turns into a frag.  To fix this,
;my-macroexpand saves the first form before macro expansion, and puts that
;in the code field.  To make this work, it must be the case that every
;macro that can possibly expand into something that will trigger the
;process of converting an expression into a loop must call PROCESS-TOP.  To
;ensure this it must be the case that every macro the user can type must be
;defined with defS or DEFUN with an OPTIMIZABLE-SERIES-FUNCTION
;declaration.  (Note it is fine for things the user cannot type anyway to
;be defined with defmacro.)  (Unfortunately, the end user can break this
;rule if they define a new collector with DEFMACRO, but you cannot make
;everything work.  At least all they will see is things generated by their
;own macro)

(cl:defun annotate (code frag)
  (when (frag-p frag)
    (setf (code frag) code))
  frag)

;Considerable effort is expended to see that the code field usually
;contains code that makes sense to the user.  Extensive testing
;indicates that it never ends up containing :||, and that the code it
;contains always is part of the code the user types except that an
;optional argument can end up having the default value which ends up
;in the annotation.

;Each arg and ret has the following parts.

(defstruct (sym (:conc-name nil) (:type list) :named)
  (back-ptrs (make-array 2 :initial-element nil))
  (var nil)              ;gensymed variable.
  (series-var-p nil)     ;T if holds a series.
  (off-line-spot nil)    ;if off-line, place to insert the computation.
  (off-line-exit nil))   ;if non-passive input, label to catch exit.

;If there is an on-line-spot, it must appear in the frag code exactly
;once at top level.  It cannot be nested in a form.  It also can only be
;referred to from a single input or output.

;A number of functions depend on the fact that frags and syms are list
;structures which can be traversed by functions like nsubst.  The
;following three circular pointers are hidden in an array so they
;won't be followed.  (Note that ins only have prv and rets only have
;nxts, as a result, they can both be stored in the same place.  two
;names are used in order to enhance the readability of the program.)

(defmacro fr (s)       ;back pointer to containing frag.
  `(aref (back-ptrs ,s) 0))
(defmacro nxts (s)     ;list of destinations of dflows starting here.
  `(aref (back-ptrs ,s) 1))
(defmacro prv (s)      ;the single source of dflow to here.
  `(aref (back-ptrs ,s) 1))

;The sym vars are symbols which appear in the body of the frag where they
;should.  All of the symbols must be unique in all the world.  Every instance
;of the symbol anywhere must be a use of the symbol.
;  Output variables can be freely read and written.
;Input variables can be read freely, but cannot ever be written.
;  These restrictions guarantee that when frags are combined, it is OK to
;rename the input var of one to be the output var of the other.  In
;addition, the creator of an output can depend on the output variable
;being unchanged by the user(s).  However, this is not the main point.
;More critical is the situation where two frags use the same value.
;The second frag can be sure that the first frag did not mess up the value.
;(Side-effects could still cause problems.  The user must guard
;against destroying some other fragment's internal state.)
;  In the interest of good output code, some work is done to simplify
;things when frags are merged.  If an output is of the form (setq out c)
;where c is T, nil, or a number, then c is substituted directly for the
;input.  Substitution is also applied if c is a variable which is not
;bound in the destination frag.  In addition, other kinds of constants
;are substituted if they are only used in one place.  A final pass
;gets rid of setqs to variables that are never used for anything.

(defmacro free-out (s) ;var output is assigned to if any.
  `(off-line-exit ,s))

;only inputs can have off-line exits, so we can reuse the same field for
;this.  if an output is assigned to a variable on *renames*, the variable
;is recorded here.  This is used in some situations to hook up data flow
;correctly.  It also indicates a few additional things.
; (A) you cannot every kill this ret, because you may need it even if
;     you do not need it for dflow by nesting of expressions.
; (B) if you have it still existing at the end of everything, because
;     it was never used, then this is something to issue a warning about,
;     but it is not a value to be returned by the expression as a whole.

;The third key internal form is a graph of frags.  This is
;represented in an indirect way.  The special variable *graph*
;contains a list of all of the frags in the series expression currently
;being processed.  The order of the frags in this list is vitally
;important.  It corresponds to their lexical order in the input
;expression and controls the default way things with no data flow
;between them are ordered when combined.  In addition, many of the
;algorithms depend on the fact that the order in *graph* is compatible
;with the data flow in that there can never be data flow from a frag
;to an earlier frag in the list.

;Subexpressions and regions within the expression as a whole are
;delineated by setting marking bits in the frags in the region.

;lambda-series makes special frags for arguments which are not in the list
;*graph*.  They exist to record info about the arguments and to
;preserve an invariant that every input of every frag in *graph* must
;have data flow ending on it.  A related invariant states that if a
;frag in *graph* has a ret then this ret must be used either by having
;dflow from it, or as an output of the expression as a whole.  Unused
;rets are removed from frags when the frags are created.

;for the purposes of testing whether a subexpression is strongly
;connected to its outputs, a frag with no rets is considered to be an
;output of the subexpression.

(cl:defun non-series-p (frag)
  (and (notany #'(lambda (x) (series-var-p x)) (rets frag))
       (notany #'(lambda (x) (series-var-p x)) (args frag))))

(cl:defun active-terminator-p (frag)
  (or (branches-to END (prolog frag))
      (branches-to END (body frag))))

;this assumes that every instance of one of series's funny labels is
;really an instance of that label made by the macros below.

(cl:defun branches-to (label tree)
  (cond ((and (eq-car tree 'tagbody) (member label tree)) nil)
	((and (eq-car tree 'go) (eq-car (cdr tree) label)) T)
	(T (do ((tt tree (cdr tt)))
	       ((not (consp tt)) nil)
	     (if (branches-to label (car tt)) (return T))))))

;hacking marks

(cl:defun reset-marks (&optional (value 0))
  (dolist (f *graph*)
    (setf (marks f) value)))

(cl:defun mark (mask frag)   ;sets bits on
  (setf (marks frag) (logior mask (marks frag))))

(cl:defun marked-p (mask frag) ;checks that all bits are on
  (zerop (logandc2 mask (marks frag))))

(defmacro dofrags ((var . mask) &body body) ;mask should be a constant
  (when mask
    (setq body `((when (marked-p ,(car mask) ,var) ,@ body))))
  `(dolist (,var *graph*) ,@ body))

;Making unique variables.
;Each call on this uses a different atom, so that you can tell
;where a given variable came from when debugging.

(cl:defun new-var (atom)
  (cl:let ((root (string atom)))
    (if (not (eql (aref root (1- (length root))) #\-))
	(setq root (concatenate 'string root "-")))
    (gensym root)))

;many of the functions in this file depend on the fact that frags and
;syms are list structures.  However, only the following functions
;depend on the exact position of parts of these structures.  Note that
;the CL manual guarantees that these positions are correct in all
;implementations.

(cl:defun merge-frags (frag1 frag2)
  (when (must-run frag1) (setf (must-run frag2) T))
  (mapc #'(lambda (s) (setf (fr s) frag2)) (rets frag1))
  (mapc #'(lambda (s) (setf (fr s) frag2)) (args frag1))
  (mapl #'(lambda (f1 f2) (rplaca f2 (nconc (car f1) (car f2))))
	(cddddr frag1) (cddddr frag2))
  frag2)

(cl:defun copy-fragment (frag)
  (cl:let* ((alist (mapcar #'(lambda (v) (cons v (gensym (root v))))
			     (find-gensyms frag)))
	      (new-frag (list* 'frag (code frag)
			       (nsublis alist (iterative-copy-tree (cddr frag))))))
    (dolist (a (args new-frag))
      (copy-ptrs a new-frag))
    (dolist (r (rets new-frag))
      (copy-ptrs r new-frag))
    new-frag))

(cl:defun copy-ptrs (sym frag)
  (setf (back-ptrs sym) (make-array 2))
  (setf (nxts sym) nil)
  (setf (fr sym) frag))

(cl:defun frag->list (frag)
  (setq frag (copy-list frag))
  (setf (rets frag) (copy-tree (mapcar #'cddr (rets frag))))
  (setf (args frag) (copy-tree (mapcar #'cddr (args frag))))
  (cl:let ((gensyms (find-gensyms frag)))
    (sublis (mapcar #'(lambda (v) (cons v (gentemp (root v)))) gensyms)
      (cons gensyms (iterative-copy-tree (cddddr frag))))))

(cl:defun find-gensyms (tree &optional (found nil))
  (do ((tt tree (cdr tt)))
      ((not (consp tt))
       (if (and (symbolp tt) (null (symbol-package tt)))
	   (adjoin tt found)
	   found))
    (setq found (find-gensyms (car tt) found))))

(cl:defun root (symbol)
  (cl:let* ((string (string symbol))
	      (pos (position #\- string :start (min (length string) 1))))
    (if pos (subseq string 0 (1+ pos)) (concatenate 'string string "-"))))

(cl:defun list->frag (list)
  (cl:let* ((alist (mapcar #'(lambda (v) (cons v (gensym (root v)))) (pop list)))
	      (frag (list* 'frag :|| 0 nil
			   (nsublis alist (iterative-copy-tree list)))))
    (setf (args frag) (mapcar #'(lambda (s) (list->sym s frag)) (args frag)))
    (setf (rets frag) (mapcar #'(lambda (s) (list->sym s frag)) (rets frag)))
    (values frag alist)))

(cl:defun list->sym (list frag)
  (cl:let ((s (make-sym :var (car list) :series-var-p (cadr list)
			  :off-line-spot (caddr list)
			  :off-line-exit (cadddr list))))
    (setf (fr s) frag)
    s))

;some Common Lisps implement copy-tree tail recursively.

(cl:defun iterative-copy-tree (tree)
  (if (not (consp tree)) tree
      (prog (tail result ptr)
	    (setq tail (cdr tree))
	    (setq result (cons (iterative-copy-tree (car tree)) nil))
	    (setq ptr result)
	  L (when (not (consp tail))
	      (setf (cdr ptr) tail)
	      (return result))
	    (setf (cdr ptr) (cons (iterative-copy-tree (car tail)) nil))
	    (setq ptr (cdr ptr))
	    (setq tail (cdr tail))
	    (go L))))

;Special form for defining series functions directly in the internal form.
;The various variables and the exit label must be unique in the body.
;The exit label must be END.  Also everything is arranged just as it is
;in an actual frag structure.

(cl:defun literal-frag (stuff) ;(args rets aux alt prolog body epilog wraprs)
  (cl:let ((gensyms (nconc (mapcar #'car (nth 0 stuff))
			     (mapcar #'car (nth 2 stuff)))))
    (dolist (f (nth 5 stuff))
      (if (symbolp f) (push f gensyms)))
    (list->frag (cons gensyms stuff))))

(defmacro delete1 (thing list)
  `(setf ,list (delete1a ,thing ,list)))

(cl:defun delete1a (item list)
  (if (eq item (car list)) (cdr list)
      (do ((l list (cdr l)))
	  ((null (cdr list)))
	(when (eq item (cadr l))
	  (rplacd l (cddr l))
	  (return list)))))

(cl:defun +arg (arg frag)
  (setf (fr arg) frag)
  (setf (args frag) (nconc (args frag) (list arg)))) ;needed by cotruncate

(cl:defun -arg (arg)
  (delete1 arg (args (fr arg))))

(cl:defun +ret (ret frag)
  (setf (fr ret) frag)
  (setf (rets frag) (nconc (rets frag) (list ret)))) ;needed by coerce-to-type

(cl:defun -ret (ret)
  (delete1 ret (rets (fr ret))))

(cl:defun kill-ret (ret)
  (when (off-line-spot ret)
    (setf (body (fr ret))
	  (nsubst-inline nil (off-line-spot ret) (body (fr ret)))))
  (when (and (not (series-var-p ret))
	     (every #'(lambda (r) (or (eq r ret) (series-var-p r)))
		     (rets (fr ret))))
    (setf (must-run (fr ret)) T)) ;signal must run to cause any side-effects.
  (-ret ret))

(cl:defun +frag (frag)
  (setf *graph* (nconc *graph* (list frag))) ;needed to keep order right
  frag)

(cl:defun -frag (frag)
  (delete1 frag *graph*)
  (setf (marks frag) 0) ;important so dofrags will notice deletions
  frag)

(cl:defun +dflow (source dest)
  (push dest (nxts source))
  (setf (prv dest) source))

(cl:defun -dflow (source dest)
  (delete1 dest (nxts source))
  (setf (prv dest) nil))

(cl:defun all-nxts (frag)
  (apply #'append (mapcar #'(lambda (r) (nxts r)) (rets frag))))

(cl:defun all-prvs (frag)
  (delete nil (mapcar #'(lambda (a) (prv a)) (args frag))))

(cl:defun yes (call) (declare (ignore call)) T)
(cl:defun no (call) (declare (ignore call)) nil)
);end of eval-when

;                ---- FUNCTIONS FOR CODE WALKING ----

;M-&-R takes in a piece of code.  It assumes CODE is a semantic whole.  Ie, it
;is something which could be evaled (as opposed to a disembodied cond clause).
;It scans over CODE macroexpanding all of the parts of it, and performing
;renames as specified by *RENAMES*.  M-&-R puts entries on the variable
;*RENAMES* which block the renaming of bound variables.
;  M-&-R also calls FN (if any) on every subpart of CODE (including the whole
;thing) which could possibly be evaluated.  The result of consing together all
;of the results of FN is returned.  Ie, the result is isomorphic to the input
;with each part replaced with what FN returned.  This is done totally by
;copying.  The input is not altered.
;  In addition, m-&-R checks to see that the code isn't setqing variables
;it shouldn't be.

;In order to do the above, M-&-R has to be able to understand fexprs.  It
;understands fexprs by having a description of each of the standard ones (see
;below).  It will not work on certain weird ones.
;  fexprs are understood by means of templates which are (usually circular)
;lists of function names.  These fns are called in order to processes the
;various fields of the fexpr.  The template can be a single fn in which case
;this fn is called to process the fexpr as a whole.

(defmacro make-template (head rest)
  `(cl:let ((h (append ',head nil))
	      (r (append ',rest nil)))
     (nconc h r r)))

(defmacro deft (name head rest)
  `(setf (get ',name 'scan-template) (make-template ,head ,rest)))

(defvar *expr-template* (make-template (Q) (E)))

(defvar *eval-all-template* (make-template () (E)))

;on lispm '(lambda ...) macroexpands to (function (lambda ...)) ugh!

(cl:defun my-macroexpand (original-code)
  (cl:let ((flag (not (frag-p original-code))) (code original-code) head temp)
    (loop
      (when (not (and flag (consp code))) (return code))
      (when (setq temp (or (and (symbolp (setq head (car code)))
                                (get head 'my-macro))
                           (and (consp head)
                                (symbolp (car head))
                                (get (car head) 'my-macro))))
        (setq code (cl:funcall temp code)))
      (when (not (symbolp (setq head (car code)))) (return code))
      (loop
        (if (not (get (setq head (car code)) 'series-optimizer)) (return nil))
        (when (not *in-series-expr*)
          (when (and *not-straight-line-code*
                     (cl:funcall (get head 'returns-series) code))
            (rrs 20 "~%Not straight-line code~%" *not-straight-line-code*))
          (return nil))
        (cl:let ((*call* code))
          (setq code (apply (get head 'series-optimizer) (cdr code)))))
      (when (frag-p code) (annotate original-code code) (return code))
      (when (get (car code) 'scan-template) (return code))
      ;; protects from any macro side-effects
      (if (eq code original-code) (setq code (iterative-copy-tree code)))
      (multiple-value-setq (code flag) (macroexpand-1 code *env*)))))

;special macro-like forms to handle setq forms.  Note psetq is already a macro.

(cl:defun my-lambda-macro (form)
  (if (not (consp (car form))) form
      (cl:let ((args (cadar form))
		 (body (cddar form))
		 (vals (cdr form)))
	(if (and (every #'(lambda (a) 
			    (and (symbolp a) (not (member a lambda-list-keywords))))
			args)
		 (= (length args) (length vals)))
	    `(let ,(mapcar #'list args vals) . ,body)
	    form))))

(setf (get 'lambda 'my-macro) #'my-lambda-macro)

(cl:defun my-setq-macro (form)
  (cond ((null (cdr form)) nil)
        ((cdddr form)
         `(progn ,@(do ((l (cdr form) (cddr l))
                        (r nil (cons `(setq ,(car l) ,(cadr l)) r)))
                       ((null l) (nreverse r)))))
        (T form)))

(setf (get 'setq 'my-macro) #'my-setq-macro)

(cl:defun m-&-r (code &optional (*fn* nil))
  (cl:let ((*being-setqed* nil))
    (m-&-r1 code)))

(defvar *fexprs-not-handled* '(FLET LABELS MACROLET))

(cl:defun m-&-r1 (code)
  (cl:let ((*renames* *renames*))
    (setq code (my-macroexpand code))
    (if (symbolp code)
        (setq code (or (cdr (assoc code *renames*)) code)))
    (if *fn* (setq code (cl:funcall *fn* code)))
    (if (not (consp code)) code
        (cl:let* ((head (car code))
		    (template (and (symbolp head) (get head 'scan-template))))
          (if (or (member head *fexprs-not-handled*)
                  (and (not-expr-like-special-form-p head) (null template))
                  (and *in-series-expr* (eq head 'multiple-value-call)))
	      (rrs 6 "~%The form " head " not allowed in SERIES expressions."))
          (m-&-r2 code
                  (if (symbolp head)
		      (or template *expr-template*) 
		      *eval-all-template*))))))

(cl:defun m-&-r2 (code template)
  (if (not (listp template)) (cl:funcall template code)
      (mapcar #'(lambda (tm c) (cl:funcall tm c)) template code)))

;The following are the fns allowed in templates.

(cl:defun Q   (code) code)
(cl:defun E   (code) (m-&-r1 code))
(cl:defun EX  (code)
  (cl:let* ((*not-straight-line-code* *in-series-expr*)
              (*in-series-expr* nil))
    (m-&-r2 code *expr-template*)))
(cl:defun EL  (code)
  (cl:let* ((*not-straight-line-code* *in-series-expr*)
              (*in-series-expr* nil))
    (m-&-r1 code)))
(cl:defun ELM  (code)
  (if *series-implicit-map* (m-&-r1 code) (EL code)))
(cl:defun S   (code) (cl:let ((*being-setqed* T)) (m-&-r1 code)))
(cl:defun B   (code) (bind-list code nil))
(cl:defun B*  (code) (bind-list code T))
(cl:defun A   (code) (arg-list code))
(cl:defun LAB (code) (if (symbolp code) code (EL code)))
(cl:defun FUN (code) (if (not (consp code)) code (process-fn code)))

;This handles binding lists for LET.

(cl:defun bind-list (args sequential &aux (pending nil))
  (prog1 (mapcar #'(lambda (arg)
                     (cl:let* ((val-p (and (consp arg) (cdr arg)))
				 (new-val (if val-p (m-&-r1 (cadr arg))))
				 (var (if (consp arg) (car arg) arg)))
                       (if sequential (push (list var) *renames*)
                           (push (list var) pending))
                       (if val-p (list (car arg) new-val) arg)))
                 args)
    (setq *renames* (append pending *renames*))))

(cl:defun arg-list (args)
  (mapcar #'(lambda (arg)
              (cl:let* ((vars (vars-of arg))
			  (val-p (and (consp arg) (cdr arg)))
			  (new-val (if val-p (m-&-r1 (cadr arg)))))
                (setq *renames* (append (mapcar #'list vars) *renames*))
                (if val-p (list* (car arg) new-val (cddr arg)) arg)))
          args))

(cl:defun compiler-let-template (form)
  (cl:let ((symbols (mapcar #'(lambda (p) (if (consp p) (car p) p)) (cadr form)))
	     (values (mapcar #'(lambda (p) (when (consp p) (eval (cadr p)))) (cadr form)))
	     (body (cddr form)))
    (progv symbols values
      (E (if (null (cdr body)) (car body) (list* 'let nil body))))))

(setf (get 'compiler-let 'scan-template) #'compiler-let-template)

;What the following is doing with the free variables may not be
;quite right.  All in all, it is pretty scary if you refer to local lexical
;vars in a fn in a series expression.  
;HERE Note that for the moment, Series does not realize that you have used
;a variable if this is the only way you use it.

(cl:defun process-fn (code)
  (cl:let ((*in-series-expr* nil) (*not-straight-line-code* nil)
	     (*user-names* nil) (*renames* *renames*))
    (cl:multiple-value-bind (fn free-ins free-outs)
        (handle-non-series-stuff code)
      (dolist (f free-ins)
        (setq fn (nsubst (var (cdr f)) (car f) fn)))
      (dolist (f free-outs)
        (setq fn (nsubst (cdr f) (car f) fn)))
      fn)))

;templates for special forms.  Note that the following are not handled
;  COMPILER-LET FLET LABELS MACROLET but must not macroexpand.
;FLET and DECLARE in particular are macros in lucid and messed things up
;by expanding at the wrong time.

(deft                block (Q Q)  (EL))
(deft                catch (Q E)  (EL))
(deft              declare (Q)    (EX))  ;needed by Xerox CL
(deft            eval-when (Q Q)  (E))
(deft             function (Q FUN)())
(deft                   go (Q Q)  ())
(deft                   if (Q E)  (ELM))
(deft               cl:let (Q B)  (E))
(deft              cl:let* (Q B*) (E))
(deft  multiple-value-call (Q)    (E))
(deft multiple-value-prog1 (Q)    (E))
(deft                progn (Q)    (E))
(deft                progv (Q)    (E))
(deft                quote (Q Q)  ())
(deft          return-from (Q Q)  (E))
(deft                 setq (Q)    (S E))
(deft              tagbody (Q)    (Lab))
(deft                  the (Q Q)  (E))
(deft                throw (Q)    (E))
(deft       unwind-protect (Q)    (EL))

(deft               lambda (Q A)  (E))

(deft                 flet (Q)    (E))
(deft         compiler-let (Q)    (E))
(deft             macrolet (Q)    (E))
(deft               labels (Q)    (E))
(deft                 type (Q Q)  (E))

(deft                  setf (Q)    (E))   ;fixes weird interaction with lispm setf 

(defvar *expr-like-special-forms* 
  '(multiple-value-call multiple-value-prog1 progn progv the throw
    ;;some simple additions for lispm
    *catch multiple-value-return progw return-list 
    variable-boundp variable-location variable-makunbound)
  "special forms that can be treated like ordinary functions.
   e.g., they have the same template as expr-template.")

(cl:defun not-expr-like-special-form-p (sym)
  (and #-Series-ANSI(special-form-p sym)
       #+Series-ANSI(special-operator-p sym)
       (not (member sym *expr-like-special-forms*))))
#+symbolics
(eval-when (eval load)
(cl:defun WSLB (list)
  (prog1 (EX list) (push (list (car list)) *renames*)))
(deft                LET-IF (Q E B) (E))
(deft   scl:WITH-STACK-LIST (Q WSLB) (E))
(deft  scl:WITH-STACK-LIST* (Q WSLB) (E)))

;This macro-expands everything in the code making sure that all free
;variables (that are not free in the whole series expression)
;are appropriately changed to gensyms.  It returns the new code plus
; (a) list of pairs of internal var gensyms and external values.
;     Typically, dflow should be inserted from the external values to
;     ports made with these gensyms.
; (b) list of pairs of output gensyms and the actual var names they modify.
; (c) list of vars from the list CHECK-SETQ that are setqed.
; If the state argument is supplied, it contains lists of input and output
; info that is used to initialize things.
;error messages are issued if a series value is used in an improper context.

(cl:defun handle-non-series-stuff (code &optional (state nil) (check-setq nil))
  (cl:let ((free-ins (car state)) (free-outs (cadr state)) (setqed nil))
    (setq code
          (m-&-r code
                 #'(lambda (cd)
                     (if (and check-setq (symbolp cd) *being-setqed*
                              (member cd check-setq))
                       (setq setqed (adjoin cd setqed)))
                     (cl:let ((c (if (frag-p cd) (retify cd) cd)) temp)
                       (when (sym-p c)
                         (if *being-setqed*
                           (if (setq temp (assoc c free-outs)) (setq c (car (cdr temp)))
                               (cl:let ((new (if (setq temp (assoc c free-ins))
						   (car (cdr temp))
						   (new-var 'freeout)))
                                          (v (car (rassoc cd *renames*))))
                                 (push (cons c (cons new v)) free-outs)
                                 (setq c new)))
                           (if (setq temp (assoc c free-ins)) (setq c (car (cdr temp)))
                               (cl:let ((arg (if (setq temp (assoc c free-outs))
						   (car (cdr temp))
						   (new-var 'freein))))
                                 (when (series-var-p c)
                                   (when *not-straight-line-code*
				     (rrs 20 "~%Not straight line code~%" code))
                                   (when (not *in-series-expr*)
                                     (rrs 12 "~%Series var " (car (rassoc c *renames*))
                                          " referenced in nested non-series LAMBDA.")))
                                 (push (cons c (cons arg c)) free-ins)
                                 (setq c arg)))))
                       c))))
    (values code (mapcar #'cdr free-ins) (mapcar #'cdr free-outs)
            setqed (list free-ins free-outs))))

; ---- PHYSICAL REPRESENTATIONS FOR SERIES AND GENERATORS ----

;The following structure is used as the physical representation for a
;series.  It is a structure so that it can print itself and get read
;in.  The only operation on it is to get it to return a a generator
;object.  The only operation on a generator object is NEXT-IN.

;Physical series are of two kinds basic and image.
;A basic series has three parts
;GEN-FN is a fn that generates new values.  When called with no args, must either
;  return a list of the next value, or nil indicating no more values to
;  return.  (If there is an alter function, then each value is actually a
;  list of the fundamental value and any additional information needed by
;  the alter function.  NEXT-IN only returns the fundamental value in any case.)
;DATA-SO-FAR cons of NIL and data generated by GEN-FN so far.  The last cdr is a
;  flag that tells you whether the end has been reached.  If it is T
;  there is still more to get, if it is NIL you are done.  (The NIL car is needed
;  so that new elements can allways be added by side effect.)
;ALTER-FN is a fn that alters elements (or NIL if none).  Must be a function
;  that when called with a new item as its first arg and any additional
;  information computed by the GEN-FN as its other arguments does the alteration.

;Image series compute one series from another without requiring any mutable internal
;state.  The also have four parts.
;BASE-SERIES the series the image series is based on.  The elements of the image are
;  some simple function of the elements of the base.  (The base can be another
;  image series.)
;IMAGE-FN is a function with no changing internal state that will get the next full
;  item of the series given a generator of the base series.  It must behave the
;  same as BASIC-DO-NEXT-IN.
;IMAGE-DATUM Some non-null value that can be used by the IMAGE-FN when deciding
;  what to do.  This often saves having to have the IMAGE-FN be a closure.
;  It is passed as the second argument to the IMAGE-FN.
;ALTER-FN same as for a basic series.

(eval-when (eval load compile)

(defstruct (foundation-series (:conc-name nil))
  (alter-fn nil))

(defstruct (basic-series (:include foundation-series)
			 (:conc-name nil)
			 (:print-function print-series))
  (gen-fn nil) data-so-far)

(defstruct (image-series (:include foundation-series)
			 (:conc-name nil)
			 (:print-function print-series))
  image-fn image-base (image-datum nil))

#+(or cmu CLISP) ; handle multiple definition of ALTER-FN
(defstruct (foundation-series (:conc-name nil))
  (alter-fn nil))

(defmacro make-phys (&key (gen-fn nil) (alter-fn nil) (data-list T))
  `(make-basic-series :gen-fn ,gen-fn :alter-fn ,alter-fn
		      :data-so-far (cons nil ,data-list)))

(cl:defun series-p (x)
  (or (basic-series-p x) (image-series-p x)))

(deftype series (&rest type)
  (declare (ignore type))
  `(satisfies series-p))

(deftype series-element-type (var)
  (declare (ignore var))
  T)

;A generator is a data structure with the following parts.  For
;speed in symbolics lisp, it is implement as a list.
;GEN-BASE is the series the generator is generating the elements of.
;GEN-BASE is one of two things depending on what kind of series the GEN-BASE is.
;  For basic-series, it starts out as the data-so-far and is cdr'ed down as the
;    elemnts are used.  Also additional elements are tagged on to the end as
;    needed.  Sharing is used so that when elements are added here, they are
;    automaticaly added onto the data-so-far of the series itself and to any
;    other generators for this series as well.
;  For image-series, it is a generator for the the IMAGE-BASE.
;  In either of the cases above, the GEN-BASE becomes NIL when the generator is
;    exhausted.
;CURRENT-ALTER-INFO is the list of information that is needed to alter
;  the last element generated.  (If the base has no alter-fn, then this is nil.)

(defstruct (generator (:conc-name nil) (:type list))
  gen-state gen-base (current-alter-info nil))

(cl:defun generator (s)
  (make-generator
    :gen-base s
    :gen-state
     (cond ((image-series-p s) (generator (image-base s)))
	   ((basic-series-p s) (data-so-far s))
	   (T (ers 60 "~%GENERATOR applied to something that is not a series.")))))

(cl:defun generator-check (form)
  (when *in-series-expr*
    (rrs 24 "~%Using GENERATOR blocks optimization " form))
  (cons 'generator0 (cdr form)))

(setf (get 'generator 'series-optimizer) #'generator-check)
(setf (get 'generator 'returns-series) #'no)

(cl:defun generator0 (s)
  (make-generator
    :gen-base s
    :gen-state
     (cond ((image-series-p s) (generator (image-base s)))
	   ((basic-series-p s) (data-so-far s))
	   (T (ers 60 "~%GENERATOR applied to something that is not a series.")))))

;This function interfaces to generators.  No optimization ever happens to
;generators except in the function PRODUCING.  The next element of the generator
;is returned each time DO-NEXT-IN is called.  If there are no more elements, the
;functional argument is funcalled.  It is an error to call DO-NEXT-IN again later.

(defmacro next-in (generator &rest actions)
  `(do-next-in ,generator #'(lambda () ,@ actions)))

(cl:defun do-next-in (g at-end &optional (alter nil alterp))
  (cl:let ((current (basic-do-next-in g)))
    (cond ((null (gen-state g)) (cl:funcall at-end))
	  (alterp
	   (apply (cond ((alter-fn (gen-base g)))
			(T (ers 65 "~%Alter applied to an unalterable form.")))
		  alter (current-alter-info g)))
	  (T current))))

;This returns the next full entry in a generator, or sets the
;gen-state to NIL indicating the generator is exhausted.

(cl:defun basic-do-next-in (g)
  (when (gen-state g)
    (cl:let ((full-current
		 (cond ((image-series-p (gen-base g))
			(prog1 (cl:funcall (image-fn (gen-base g))
					     (gen-state g)
					     (image-datum (gen-base g)))
			       (when (null (gen-state (gen-state g)))
				 (setf (gen-state g) nil))))
		       (T (when (eq (cdr (gen-state g)) T)
			    (setf (cdr (gen-state g))
				  (cl:funcall (gen-fn (gen-base g)))))
			  (pop (gen-state g))
			  (car (gen-state g))))))
      (cond ((alter-fn (gen-base g))
             (setf (current-alter-info g) (cdr full-current))
             (car full-current))
            (T full-current)))))

;The following is an example of an image function.  It selects the
;datum-th part of the full item of the base series as the item of the image series.

(cl:defun image-of-datum-th (g datum)
  (nth datum (basic-do-next-in g)))

(cl:defun values-lists (n series-of-lists &optional (alterers nil))
  (values-list
    (mapcar #'(lambda (i)
		(make-image-series :alter-fn (pop alterers)
				   :image-fn #'image-of-datum-th
				   :image-datum i
				   :image-base series-of-lists))
	    (n-integers n))))

(cl:defun n-integers (n)
  (do ((i (1- n) (1- i))
       (l nil (cons i l)))
      ((minusp i) l)))

(cl:defun print-series (series stream depth)
  (cl:let ((generator (generator series)))
    (write-string "#Z(" stream)
    (do ((first-P T nil)
	 (i (cond (*print-length*) (T -1)) (1- i)))
	(nil)
      (cl:let ((element (next-in generator (return nil))))
        (if (not first-p) (write-char #\space stream))
        (when (zerop i) (write-string "..." stream) (return nil))
        (write element :stream stream
               :level (if *print-level* (- *print-level* depth)))))
    (write-char #\) stream)))

;This is used to allow a fragment to accept a physical series in lieu of
;one computed be another frag.

(cl:defun add-physical-interface (arg)
  (cl:let ((frag (fr arg))
	     (var (var arg))
	     (off-line-spot (off-line-spot arg))
	     (off-line-exit (off-line-exit arg))
	     (series (new-var 'series))
	     (generator (new-var 'generator)))
    (setf (var arg) series)
    (setf (series-var-p arg) nil)
    (setf (off-line-spot arg) nil)
    (setf (off-line-exit arg) nil)
    (push (list var T) (aux frag))
    (push `(setq ,generator (generator ,series)) (prolog frag))
    (push (list generator T) (aux frag))
    (if (not off-line-spot)
	(push `(setq ,var (next-in ,generator (go ,END))) (body frag))
	(setf (body frag)
	      (nsubst-inline
		`((setq ,var (next-in ,generator
				      (go ,(cond (off-line-exit) (T END))))))
		off-line-spot (body frag))))
    generator))

;This turns a series output into a non-series output returning a physical series.
;(Note this assumes that if alterability is being propogated, the corresponding
;input has already been changed using add-physical-interface.  Alter-prop is a
;cons of the new input var (a physical series) and the var holding the generator.)

(cl:defun add-physical-out-interface (ret alter-prop)
  (cl:let* ((frag (fr ret))
	    (off-line-spot (off-line-spot ret))
	    (new-list (new-var 'list))
	    (new-out (new-var 'out)))
    (cl:multiple-value-bind (out-value alterer) (out-value ret alter-prop nil)
      (cl:let* ((new-body-code `((push ,out-value ,new-list)))
		(new-epilog-code
		  `(setq ,new-out (make-phys :data-list (nreverse ,new-list)
                                             :alter-fn ,alterer))))
	(setf (var ret) new-out)
	(setf (series-var-p ret) nil)
	(setf (off-line-spot ret) nil)
	(push (list new-list 'list) (aux frag))
	(push (list new-out 'series) (aux frag))
	(push `(setq ,new-list '()) (prolog frag))
	(if (not off-line-spot)
	    (setf (body frag) (nconc (body frag) new-body-code))
	    (setf (body frag)
		  (nsubst-inline new-body-code off-line-spot (body frag))))
	(push new-epilog-code (epilog frag))
	frag))))


;alter-info has priority

(cl:defun out-value (ret alter-prop flag-off-line?)
  (cl:let* ((var (var ret))
	      (alter-info (cdr (assoc var (alterable (fr ret))))))
    (values (cond (alter-info `(list ,var ,@ (cdr alter-info)))
		  (alter-prop `(do-alter-prop ,var ,(cdr alter-prop)))
		  ((and flag-off-line? (off-line-spot ret)) `(list ,var))
		  (T var))
	    (cond (alter-info
		   (cl:let ((alter (new-var 'alter)))
		     `#'(lambda (,alter ,@(cdr alter-info))
			  ,(subst alter '*alt* (car alter-info)))))
		  (alter-prop `(alter-fn ,(car alter-prop)))))))

(cl:defun do-alter-prop (value gen)
  (if (alter-fn (gen-base gen))
      (cons value (current-alter-info gen))
      value))

(cl:defun nsubst-inline (new-list old list &optional (save-spot nil))
  (cl:let ((tail (member old list)))
    (cond ((not tail) old)
	  (save-spot (rplacd tail (nconc new-list (cdr tail))))
	  (new-list (rplaca tail (car new-list))
		    (rplacd tail (nconc (cdr new-list) (cdr tail))))
	  ((cdr tail) (rplaca tail (cadr tail))
		      (rplacd tail (cddr tail)))
	  (T (setq list (nbutlast list)))))
    list)

;This is used when optimization is not possible.
;It makes one main physical frag that computes the series returned by frag.
;(If there is more than one output, then several subsidiary frags have to be
; created to pick the right values out.)
;It assumes that actual-args must be a list of variables.

(cl:defun frag->physical (frag actual-args &optional (force-precompute? nil))
  (cl:let ((alter-prop-alist
	       (mapcar #'(lambda (a actual)
			   (prog1 (when (series-var-p a)
				    (list* (var a) actual
					   (add-physical-interface a)))
				  (nsubst actual (var a) frag)))
		       (args frag) actual-args)))
    (setf (args frag) nil)
    (if (or force-precompute? (wrappers frag)
	    (some #'(lambda (r) (not (series-var-p r))) (rets frag)))
	(precompute-frag->physical frag alter-prop-alist)
	(series-frag->physical frag alter-prop-alist))))

(cl:defun precompute-frag->physical (frag alter-prop-alist)
  (dolist (r (rets frag))
    (when (series-var-p r)
      (add-physical-out-interface r (cdr (assoc (var r) alter-prop-alist)))))
  (cl:let ((*last-series-loop* nil) (*user-names* nil))
    (declare (special *last-series-loop* *user-names*))
    (codify frag)))

(cl:defun series-frag->physical (frag alter-prop-alist)
  (cl:let* ((out-values nil)
	      (alterers nil)
	      (done-on-line nil)
	      (new-out (new-var 'item))
	      (n (length (rets frag)))
	      (done (new-var 'done))
	      (flag (if (some #'off-line-spot (rets frag)) (new-var 'flag)))
	      (label (if flag (new-var 'l))))
    (dolist (r (reverse (rets frag)))
      (cl:multiple-value-bind (out-value alterer)
	  (out-value r (cdr (assoc (var r) alter-prop-alist)) (not (= n 1)))
	(push out-value out-values)
	(push alterer alterers)))
    (when flag
      (push `(setq ,flag -1) (prolog frag))
      (push (list flag 'fixnum) (aux frag))
      (push label (body frag)))
    (dotimes (i n)
      (cond ((off-line-spot (nth i (rets frag)))
	     (f->p-off-line i frag new-out out-values done flag))
	    ((not done-on-line)
	     (setq done-on-line T)
	     (f->p-on-line frag new-out out-values done flag))))
    (cl:let* ((basic-out (new-var 'series-of-lists))
		(code `(make-phys
			 :alter-fn ,(if (= n 1) (car alterers))
			 :gen-fn #'(lambda ()
				     (cl:let (,new-out)
				       (tagbody ,@ (body frag)
						,@(if (and flag (not done-on-line))
						      `((go ,label)))
						,END ,@(epilog frag)
						(setq ,new-out nil)
						,DONE)
				       ,new-out)))))
      (when (not (= n 1))
	(setq code
	      `(cl:let ((,basic-out ,code))
		 (values
		   ,@(mapcar
		       #'(lambda (i r a)
			   `(make-image-series
			      :alter-fn ,a
			      :image-base ,basic-out
			      :image-datum ,i
			      :image-fn ,(cond ((and (not a) (off-line-spot r))
						'#'car-image-of-non-null-datum-th)
					       ((notany #'off-line-spot (rets frag))
						'#'image-of-datum-th)
					       (T '#'image-of-non-null-datum-th))))
		       (n-integers n) (rets frag) alterers)))))
      (codify-1 (aux frag) `(,@(prolog frag) ,code)))))

(cl:defun image-of-non-null-datum-th (g datum)
  (cl:let (item)
    (loop (setq item (nth datum (basic-do-next-in g)))
	  (if (or (null (gen-state g)) (not (null item))) (return item)))))

(cl:defun car-image-of-non-null-datum-th (g datum)
  (car (image-of-non-null-datum-th g datum)))

(cl:defun f->p-off-line (i frag new-out out-values done flag)
  (cl:let* ((ret (nth i (rets frag)))
	      (off-line-spot (off-line-spot ret))
	      (restart (new-var 'restart))
	      (out-value (if (null (cdr out-values)) (car out-values)
			     `(list ,@(mapcar #'(lambda (r o)
						  (when (eq r ret) o))
					      (rets frag) out-values))))
	      (new-body-code `((setq ,new-out (cons ,out-value T))
			       (setq ,flag ,i)
			       (go ,done)
			       ,restart)))
    (push `(if (= ,flag ,i) (go ,restart)) (body frag))
    (setf (body frag) (nsubst-inline new-body-code off-line-spot (body frag)))))

(cl:defun f->p-on-line (frag new-out out-values done flag)
  (cl:let* ((out-value (if (null (cdr out-values)) (car out-values)
			     `(list ,@(mapcar #'(lambda (r o)
						  (when (not (off-line-spot r)) o))
					      (rets frag) out-values))))
	      (new-body-code `((setq ,new-out (cons ,out-value T))
			       ,@(if flag `((setq ,flag -1)))
			       (go ,done))))
    (setf (body frag) (nconc (body frag) new-body-code))))

);end of eval-when


;                  ---- TURNING AN EXPRESSION INTO A GRAPH ----

;The form below has to be called to set things up right, before
;processing of a series expression can proceed.

;should have some general error catching thing but common lisp has none.

(defmacro starting-series-expr (call body)
  `(cl:let ((*renames* nil)
         (*user-names* nil)
         (*not-straight-line-code* nil)
         (*in-series-expr* ,call))
     ,body))

;assumes opt result cannot be NIL
(defmacro top-starting-series-expr (call opt non-opt)
  `(cond ((catch :series-restriction-violation
	    (starting-series-expr ,call ,opt)))
	 (T ,non-opt)))

(cl:defun ers (id &rest args)  ;Fatal errors.
  (if *testing-errors* (throw :testing-errors id))
  (if *in-series-expr*
    (report-error (list* "~&Error " id " in series expression:~%"
			 *in-series-expr* (copy-list args)))
    (report-error (list* "~&Error " id (copy-list args))))
  (error ""))

(cl:defun rrs (id &rest args) ;Restriction violations.
  (when (not *suppress-series-warnings*)
    (report-error (list* "~&Restriction violation " id
			 " in series expression:~%"
			 (or *in-series-expr* *not-straight-line-code*)
			 (copy-list args)))
    (when (not *testing-errors*)
      (warn "")))
  (throw :series-restriction-violation nil))

(cl:defun wrs (id always-report-p &rest args) ;Warnings.
  (when (or always-report-p (not *suppress-series-warnings*))
    (report-error (list* "~&Warning " id
			 " in series expression:~%"
			 (or *in-series-expr*
			     (and (boundp '*not-straight-line-code*)
				  *not-straight-line-code*))
			 (copy-list args)))
    (when (not *testing-errors*)
      (warn ""))))

(cl:defun report-error (info)
  (setq *last-series-error* info)
  (loop (if (null info) (return nil))
	(if (stringp (car info))
	    (format *error-output* (pop info))
	    (write (pop info) :stream *error-output* :escape T :pretty T
			      :level nil :length nil :case :upcase))))

;  This parses code down to fundamental chunks creating a graph of the
;expression.  Note that macroexpanding and renaming is applied while
;this happens.

(cl:defun graphify (code &optional (return-type '*))
  (cl:let ((*graph* nil))
    (fragify code return-type)
    *graph*))

;Have to be careful not to macroexpand things twice.
;If you did, you could get two copies of some frags on *graph*.
;Note that a type of '* means any number of arguments.

(cl:defun retify (code &optional (type T))
  (if (sym-p code) code ;might have been retified/fragified before.
      (cl:let* ((expansion (my-macroexpand code))
		  (ret (if (symbolp expansion)
			   (cdr (assoc expansion *renames*)))))
        (if (sym-p ret) ret (car (rets (fragify expansion type)))))))

(cl:defun fragify (code type)
  (cl:let* ((expansion (my-macroexpand code))
	      (ret (if (symbolp expansion) (cdr (assoc expansion *renames*))))
	      (types (decode-type-arg type T)))
    (coerce-to-types types
                     (cond ((frag-p expansion) expansion) ;must always make a new frag
                           ((sym-p ret) (annotate code (pass-through-frag (list ret))))
                           ((eq-car expansion 'the)
                            (fragify (caddr expansion) (cadr expansion)))
                           ((eq-car expansion 'values)
 			    ;; It used to map over the cdr of CODE here, which is
 			    ;; obviously not right -- for instance in the case where
 			    ;; (NTH-VALUE 0 x) --> (VALUES x) but it maps over (0 x)
 			    ;; and then thinks there is more than one value.  However
 			    ;; I'm not sure it's right to just blithly map over the
 			    ;; expansion either...
			    (cl:let ((rets (mapcar #'(lambda (form)
							 (car (rets (fragify form T))))
						     (cdr expansion))))
                              (when (and (cdr rets) (some #'series-var-p rets))
                                (rrs 7 "~%VALUES returns multiple series:~%" code))
                              (annotate code (pass-through-frag rets))))
                           (T (annotate code (isolate-non-series
                                              (if (listp types) (length types) 1)
                                              expansion)))))))

(cl:defun decode-type-arg (type &optional (allow-zero nil))
  (cond ((eq type '*) '*)
	((eq-car type 'values)
	 (if (and (not allow-zero) (equal type '(values)))
	     (ers 62
	      "~%The type (VALUES) specified where at least one value required."))
	 (subst T '* (cdr type)))
	((and (not (symbolp type)) (functionp type))
	 (ers 70 "~%Function supplied where type expected."))
	(T (list type))))

(cl:defun pass-through-frag (rets)
  (cl:let ((frag (make-frag)))
    (dolist (ret rets)
      (cl:let* ((series-p (series-var-p ret))
		  (in (new-var 'passin))
		  (in-sym (make-sym :var in :series-var-p series-p))
		  (out-sym (make-sym :var in :series-var-p series-p)))
	(+arg in-sym frag)
	(+ret out-sym frag)
	(+dflow ret in-sym)))
    (+frag frag)))

(cl:defun coerce-to-types (types frag)
  (when (not (eq types '*))
    (cl:let ((n (length types))
	       (current-n (length (rets frag))))
      (cond ((= n current-n))
	    ((< n current-n)
	     (mapc #'(lambda (r) (when (not (free-out r)) (kill-ret r)))
		   (nthcdr n (rets frag))))
	    (T (dolist (v (n-gensyms (- n current-n) "XTRA-"))
		 (+ret (make-sym :var v) frag)
		 (push (list v T) (aux frag))
		 (push `(setq ,v nil) (prolog frag)))))
      (mapc #'coerce-to-type types (rets frag))))
  frag)

;this is also used by PROTECT-FROM-SETQ in an odd way.
(cl:defun coerce-to-type (type ret)
  (if (eq type 'series) (setq type '(series T)))
  (when (not (eq type T))
    (when (and (not (eq-car type 'series)) (series-var-p ret))
      (wrs 30 t "~%Series encountered where not expected."))
    (when (eq-car type 'series)
      (if (not (series-var-p ret))
	  (wrs 31 t "~%Non-series value encountered where series expected."))
      (setq type (cadr type))
      (if (eq type '*) (setq type T)))
    (cl:let ((aux (assoc (var ret) (aux (fr ret)))))
      (if (and aux (not (subtypep (cadr aux) type)))
	  (setf (cadr aux) type)))))

;note that this does implicit mapping when appropriate.  Note also that it
;only maps the absolute minimum necessary.  This is to ensure that things
;will come out the same no matter how they were syntactically expresssed in
;the input.  Also mapping of special forms other than if is not allowed.
;If it were it could lead to all kinds of problems with binding scopes and
;scopes for gos and the like.

(cl:defun isolate-non-series (n code)
  (cl:multiple-value-bind (exp free-ins free-outs)
      (handle-non-series-stuff code)
    (cl:let* ((vars (n-gensyms n "OUT-"))
                (mapped-inputs nil)
                (frag (make-frag :aux (mapcar #'(lambda (v) (list v T)) vars))))
      (dolist (entry free-ins)
        (cl:let ((arg (make-sym :var (car entry))))
           (when (and *series-implicit-map* (series-var-p (cdr entry)))
             (push (car entry) mapped-inputs)
             (setf (series-var-p arg) T))
          (+arg arg frag)
          (+dflow (cdr entry) arg)))
      (dolist (v vars)
        (+ret (make-sym :var v :series-var-p mapped-inputs) frag))
      (if (zerop n) (setf (must-run frag) T))
      (if (null mapped-inputs)
          (setf (prolog frag) (list (make-general-setq vars exp)))
          (cl:multiple-value-bind (prolog-exps body-exp new-aux) 
              (map-exp exp mapped-inputs)
            (when prolog-exps 
              (setf (prolog frag) prolog-exps)
              (dolist (a new-aux)
                (push (list a T) (aux frag))))
            (setf (body frag) (list (make-general-setq vars body-exp)))))
      (dolist (entry free-outs)
        (cl:let ((new (make-sym :var (car entry) 
                                  :series-var-p mapped-inputs))
                   (v (cdr entry)))
	  (when (not (find (car entry) (args frag) :key #'var))
	    (push (list (car entry) T) (aux frag)))
	  (setf (free-out new) v)
	  (+ret new frag)
	  (rplacd (assoc v *renames*) new)))
      (+frag frag))))

;note this can assume that the vars are gensyms that they only appear where
;they are really used. 
;HERE with the way if works, because it will not catch nested lets!

(cl:defun map-exp (exp vars)
  (cl:let ((prolog-exps nil)
             (new-aux nil))
    (labels ((map-exp0 (exp) ;can assume exp contains vars
               (cond ((symbolp exp) exp)
                     ((eq (car exp) 'if) exp)
		     ((not-expr-like-special-form-p (car exp))
                      (ers 99 "~%Implicit mapping cannot be applied to the special form "
			   (car exp)))
                     (T `(,(car exp)
                          ,@(mapcar #'(lambda (x)
                                        (cond ((contains-any vars x) (map-exp0 x))
                                              ((or (symbolp x) (constantp x)
						   (eq-car x 'function)) x)
                                              (T (cl:let ((v (new-var 'M)))
						   (push v new-aux)
						   (push `(setq ,v ,x) prolog-exps)
						   v))))
                                    (cdr exp)))))))
      (cl:let ((body-exp (map-exp0 exp)))
        (values (nreverse prolog-exps) body-exp (nreverse new-aux))))))

(cl:defun n-gensyms (n root)
  (do ((i n (1- i))
       (l nil (cons (gensym root) l)))
      ((zerop i) l)))

(cl:defun make-general-setq (vars value)
  (cond ((= (length vars) 0) value)
	((= (length vars) 1) `(setq ,(car vars) ,value))
	((and (eq-car value 'values)
	      (= (length (cdr value)) (length vars)))
	 `(psetq ,@(mapcan #'list vars (cdr value))))
	(T `(multiple-value-setq ,vars ,value))))

(cl:defun simple-quoted-lambda (form)
  (and (eq-car form 'function) (eq-car (cadr form) 'lambda)
       (every #'variable-p (cadr (cadr form)))))

(defmacro defun (name lambda-list &environment *env* &body body)
  (if (dolist (form body)
        (cond ((and (stringp form) (eq form (car body))))
              ((and (consp form) (eq-car form 'declare))
               (if (assoc 'optimizable-series-function (cdr form)) (return T)))
              (T (return nil))))
    (define-optimizable-series-fn name lambda-list body)
    (progn (undefine-optimizable-series-fn name)
	   `(cl:defun ,name ,lambda-list
	      . ,body))))

#+symbolics(setf (gethash 'defun zwei:*lisp-indentation-offset-hash-table*)
		 '(2 1))
#+Symbolics
(setf (get 'defun 'zwei:definition-function-spec-parser)
      (get 'cl:defun 'zwei:definition-function-spec-parser))

(cl:defun define-optimizable-series-fn (name lambda-list expr-list)
  "Defines a series function, see lambda-series."
  (cl:let ((call (list* 'defun name lambda-list expr-list))
	   (*optimize-series-expressions* T)
	   (*suppress-series-warnings* nil))
    (dolist (v lambda-list)
      (when (and (symbolp v) (not (eq v '&optional))
                 (> (length (string v)) 0) (eql (aref (string v) 0) #\&))
        (ers 71 "~%Unsupported &-keyword " v " in OPTIMIZABLE-SERIES-FN arglist.")))
    (top-starting-series-expr call
      (cl:let ((vars nil) (rev-arglist nil))
	(dolist (a lambda-list)
	  (cond ((not (member '&optional rev-arglist))
		 (push a rev-arglist)
		 (if (not (eq a '&optional)) (push a vars)))
		(T (setq a (iterative-copy-tree a))
		   (setq vars (revappend (vars-of a) vars))
		   (push a rev-arglist))))
	(setq vars (nreverse vars))
	(dolist (v vars)
	  (when (not (variable-p v))
	    (ers 72 "~%Malformed OPTIMIZABLE-SERIES-FUNCTION argument " v ".")))
	(cl:multiple-value-bind
	  (forms type-alist ignore-vars doc off-line-ports outs)
	    (decode-dcls expr-list '(types ignores doc off-line-ports opts))
	  (cl:let* ((series-vars
			(mapcar #'car
				(remove-if-not #'(lambda (e)
						   (or (eq (cdr e) 'series)
						       (eq-car (cdr e) 'series)))
					       type-alist)))
		      (frag (preprocess-body vars series-vars
					     type-alist ignore-vars forms outs))
		      (used-vars (mapcan #'(lambda (v)
					     (if (not (member v ignore-vars))
						 (list v)))
					 vars))
		      (series-p (some #'(lambda (r) (series-var-p r)) (rets frag)))
		      (frag-list (frag->list frag))
		      (dcls (if ignore-vars `((ignore ,@ ignore-vars)))))
	    (check-off-line-ports frag vars off-line-ports)
	    (when (and (not series-p) (notany #'series-var-p (args frag)))
	      (wrs 44 t
		   "~%OPTIMIZABLE-SERIES-FUNCTION neither uses nor returns a series."))
	    `(defS ,name ,(reverse rev-arglist)
	       ,(if (not dcls) doc (cons doc `(declare . ,dcls)))
	       ,(frag->physical frag used-vars)
	       :optimizer
	       (funcall-frag (list->frag ',frag-list) (list ,@ used-vars))
	       :trigger ,(not series-p)))))
      (cl:multiple-value-bind (forms decls doc)
	  (decode-dcls expr-list '(no-complaints doc opts))
	`(cl:defun ,name ,lambda-list
	   ,@(if doc (list doc))
	   ,@(if decls `((declare ,@ decls)))
	   (compiler-let ((*optimize-series-expressions* nil)) ,@ forms))))))

(cl:defun check-off-line-ports (frag vars off-line-ports)
  (do ((vars vars (cdr vars))
       (args (args frag) (cdr args)))
      ((null args))
    (if (off-line-spot (car args))
	(when (not (member (car vars) off-line-ports))
	  (wrs 40 t "~%The input " (car vars) " unexpectedly off-line."))
	(when (member (car vars) off-line-ports)
	  (wrs 41 t "~%The input " (car vars) " unexpectedly on-line."))))
  (do ((i 0 (1+ i))
       (rets (rets frag) (cdr rets)))
      ((null rets))
    (if (off-line-spot (car rets))
	(when (not (member i off-line-ports))
	  (wrs 42 t "~%The " i "th output unexpectedly off-line."))
	(when (member i off-line-ports)
	  (wrs 43 t "~%The " i "th output unexpectedly on-line.")))))

(cl:defun undefine-optimizable-series-fn (name)
  (when (symbolp name)
    (remprop name 'series-optimizer)
    (remprop name 'returns-series))
  name)

(cl:defun vars-of (arg)
  (cond ((member arg lambda-list-keywords) nil)
	((not (consp arg)) (list arg))
	(T (cons (if (consp (car arg)) (cadar arg) (car arg))
		 (copy-list (cddr arg))))))

;Important that this allows extra args and doesn't check.
(cl:defun funcall-frag (frag values)
  (mapc #'(lambda (v a) (+dflow (retify v) a)) values (args frag))
  (+frag frag))

(cl:defun preprocess-body (arglist series-vars type-alist ignore-vars forms outs)
  (cl:let* ((arg-frag-rets
		(mapcar #'(lambda (a)
			    (cl:let* ((ret
					  (make-sym
					    :var (new-var 'arg)
					    :series-var-p
					    (not (null (member a series-vars)))))
					(arg-frag (make-frag :code a)))
			      (+ret ret arg-frag)
			      (push (cons a ret) *renames*)
			      ret))
			arglist))
	      (*graph* nil)
	      (last-form (car (last forms)))
	      (frag (progn (mapc #'(lambda (f) (fragify f '(values)))
				 (butlast forms))
			   (if (not (eq-car last-form 'values))
			       (fragify last-form
					(if (not outs) '*
					    (cons 'values
						  (make-list outs
							     :initial-element T))))
			       (mapc #'(lambda (f) (fragify f '(values T)))
				     (cdr last-form)))
			   (mergify *graph*)))
	      (input-info nil))
    (setf (args frag)  ;get into the right order.  Discard unused args.
          (mapcan #'(lambda (ret a)
                      (cl:let ((arg (car (nxts ret))))
                        (cond ((null arg) ;input never used
                               (cond ((member a ignore-vars) nil)
				     (T #| ;HERE can get false positives.
                                        (wrs 50 t "~%The input " a " never used.")|#
					(list ret)))) ;assume was used anyway.
                              (T ;here probably want to pretend was not declared ignore.
				 (when (member a ignore-vars)
                                   (wrs 51 t "~%The input " a
                                        " declared IGNORE and yet used."))
                                 (push (cons a `(series-element-type ,(var arg))) input-info)
                                 (setf (prv arg) nil)
                                 (dolist (a (cdr (nxts ret)))
                                   ;input used more than once.
                                   (nsubst (var arg) (var a) (fr a)))
                                 (list arg)))))
                  arg-frag-rets arglist))
    (dolist (e type-alist)
      (when (and (member (car e) arglist)
                 (eq-car (cdr e) 'series)
                 (cadr (cdr e))
                 (not (eq (cadr (cdr e)) T)))
        (push (cons (car e) (cadr (cdr e))) input-info)))
    (dolist (v (aux frag)) (propagate-types (cdr v) (aux frag) input-info))
    frag))

;This takes a list of forms that may have documentation and/or
;declarations in the initial forms.  It parses the declarations and
;returns the remaining forms followed by the parsed declarations.  The
;list allowed-dcls specifies what kinds of declarations are allowed.
;Error messages are given if any other kind of declaration is found.
;Each allowed-dcl must be one of the symbols declared special below.

(cl:defun decode-dcls (forms allowed-dcls)
  (cl:let ((doc nil) (ignores nil) (types nil) (props nil)
	     (opts nil) (off-line-ports nil) (no-complaints nil))
      (declare (special doc ignores types props opts off-line-ports no-complaints))
    (loop
      (when (and (member 'doc allowed-dcls)
		 (null doc) (stringp (car forms)) (cdr forms))
	(setq doc (pop forms)))
      (when (not (eq-car (car forms) 'declare)) (return nil))
      (dolist (d (cdr (pop forms)))
	(cond ((and (eq (car d) 'type)
		    (member 'types allowed-dcls))
	       (dolist (v (cddr d)) (push (cons v (cadr d)) types)))
	      ((and (or (member (car d) *short-hand-types*)
			(and (listp (car d)) (member (caar d) *short-hand-types*)))
		    (member 'types allowed-dcls))
	       (dolist (v (cdr d)) (push (cons v (car d)) types)))
	      ((and (eq (car d) 'optimizable-series-function)
		    (member 'opts allowed-dcls))
	       (setq opts (cond ((cadr d)) (T 1))))
	      ((and (eq (car d) 'ignore)
		    (member 'ignores allowed-dcls))
	       (setq ignores (append (cdr d) ignores)))
	      ((and (eq (car d) 'propagate-alterability)
		    (member 'props allowed-dcls))
	       (push (cdr d) props))
	      ((and (eq (car d) 'off-line-port)
		    (member 'off-line-ports allowed-dcls))
	       (setq off-line-ports (append off-line-ports (cdr d))))
	      ((not (member 'no-complaints allowed-dcls))
	       (rrs 1 "~%The declaration " d " blocks optimization."))
	      (T (setq no-complaints (nconc no-complaints (list d)))))))
    (values-list (cons forms (mapcar #'symbol-value allowed-dcls)))))

;                         ---- MERGING A GRAPH ----

;This proceeds in several phases
; (1) check for series/non-series type conflicts.  This operates in
;     one of two different ways depending on the value of
;     *SERIES-IMPLICIT-MAP*.  If this control variable is non-nil then:
;     (a) If a frag that does not process any series at all 
;         (i.e., came from totally non-series stuff in the source) receives
;         a series for any of its inputs, then it was implicitly mapped
;         by isolate-non-series. (Note, if the output is not connected to 
;         anything, it is marked as being forced to run.)
;     (b) If a non-series is supplied where a series is expected, we
;         coerce it into an infinite series of the single value.
;     Whether or not *SERIES-IMPLICIT-MAP* is non-nil we then:
;     (a) if a series is supplied where a non-series is expected,
;         issue a restriction violation warning.
;         It would not be in the spirit of things to create a physical series.
;         And would be very hard to boot.
;     (b) if a non-series is supplied where a series is expected,
;         assume that this non-series item is really a physical series and
;         add a physical interface.  (Note this cannot happen when
;         *SERIES-IMPLICIT-MAP* is non-nil.)
; (2) Do substitutions to get rid of trivial frags representing constant values and
;     references to variables.
; (2.5) get rid of dead code.
; (3) Scan the graph to find places where the expression can be split because it is
;     in disconnected places or there is an isolated dflow touching a non-series
;     or off-line port.  If the graph cannot be split, then it consists solely of
;     dflow connecting on-line ports.  A list structure is created showing all of
;     the split points that will be merged in the next step.
; (4) The structure created above is evaluated doing a sequence of merge steps
;     that reduces the whole expression to a single frag.

(cl:defun mergify (*graph*)
  (reset-marks)
  (do-coercion)
  (do-substitution)
  (kill-dead-code)
  (cl:let ((splits (do-splitting *graph*)))
 (eval splits))) 

;since implicit mapping is a bit tricky, but quite possibly the must useful
;single part of the series macro package, it deserves a few words.  To
;understand what happens, some initial definitions are necessary.  First, a
;compile-time-known series function is one of the predefiend series functions
;or a function DEFUNed with an optimizable-series-function declaration.
;(There may be lots of other functions around manipulating series, but that
;is not relevant to the implicit mapping that is going on here.) A
;compile-time-known series value is a series output of a compile-time-known
;series function or such an output bound to a variable by one of the forms
;below.  A compile-time-known series input is a series input of a
;compile-time-known series function.

;Every non-compile-time-known function that receives a compile-time-known
;series value as an input is mapped.  Note that once a
;non-compile-time-known function is mapped, the result is a
;compile-time-known series function this may cause ;more mapping to occur.
;Special forms are never mapped.  This is flagged as an error if it
;appears that it needs to be done.  Note that non-series functions
;that appear in a context where their value is not used, are flagged
;to indicate that they must be run anyway.  This carries through it
;they are mapped.

;In addition to the above, any non-series value that appears where
;a series is expected is automatically converted into an infinite series
;of that value.  If you side-effects are involved, you might want multiple
;evaluation.  However, you will have to specifically indicate this
;using map-fn or something.  (This may not be the best default in many
;ways, but it is the only way to make things come out the same without
;depending on the exact syntactic form of the input.  For instance
;note that INCF expands into a let in some lisps and this would force
;the let to be in a separate expression even though it does not look
;like it at first glance.)

;As an example of all the above consider the following.
#|
(let* ((x (car (scan '((1) (2) (3)))))
       (y (1+ x))
       (z (collect-sum (* x y))))
  (print (list x y 4))
  (print z)
  (collect (list x (catenate #Z(a) (gensym)))))
|#
;is equivalent to
#|
(let* ((x (#Mcar (scan '((1) (2) (3)))))
       (y (#M1+ x))
       (z (collect-sum (#M* x y))))
  (collect-last (#Mprint (#Mlist x y (series 4))))
  (print z)
  (collect (#Mlist x (catenate #Z(a) (series (gensym))))))
|#

;Note that compile-time-known series functions are never mapped.
;Therefore (collect (collect (scan (scan x)))) is not equivalent to
;(collect (mapping ((y (scan x))) (collect (scan y)))).  You have to
;write the latter if you want it.  Also while series/non-series conflicts
;are less likely to arise, there is no guarantee that the
;restrictions will be satisfied after implicit mapping is applied.

;               (1) CHECK-FOR SERIES/NON-SERIES CONFLICTS.

(cl:defun do-coercion ()
  (reset-marks 1)
  (cl:let ((lambda-arg-frags nil))
    (dofrags (f)
      (dolist (a (args f))
        (cl:let ((ret (prv a)))
          (when (and (series-var-p ret) (not (series-var-p a)))
            (rrs 13 "~%Series to non-series data flow from:~%" (code (fr ret))
                    "~%to:~%" (code (fr a))))
          (when (not (marked-p 1 (fr ret)))
            (push (fr ret) lambda-arg-frags)
            (when (and (not (series-var-p ret)) (series-var-p a))
              (setf (series-var-p ret) T)
              (dolist (aa (nxts ret))
                (when (not (series-var-p aa))
                  (rrs 14 "~%The optimizable series function input "
                       (code (fr ret))
                       " used as a series value by~%" (code (fr a))
                       "~%and as a non-series value by~%"
                       (code (fr aa)))))))
          (when (and (not (series-var-p ret)) (series-var-p a))
            (cond (*series-implicit-map* (series-coerce a))
                  (T (wrs 28 nil "~%Non-series to series data flow from:~%"
                          (code (fr ret)) "~%to:~%" (code (fr a)))
                     (add-physical-interface a))))))
      ;;might have to de-series if a physical interface was required for
      ;;every series input.
      (maybe-de-series f))
    (dolist (f lambda-arg-frags)
      (when (and (series-var-p (car (rets f)))
                 (cdr (nxts (car (rets f)))))
        (add-dummy-source-frag f)))))

(cl:defun series-coerce (a)
  (when (off-line-spot a)
    (nsubst nil (off-line-spot a) (fr a)))
  (setf (series-var-p a) nil))

(cl:defun add-dummy-source-frag (frag)
  (cl:let* ((ret (car (rets frag)))
	      (args (nxts ret))
	      (new-ret (car (rets (pass-through-frag (rets frag))))))
    (dolist (a args)
      (-dflow ret a)
      (+dflow new-ret a))
    (annotate (code frag) (fr new-ret))
    (setq *graph*  ;frag was stuck on wrong end
	  (cons (fr new-ret) (delete (fr new-ret) *graph*)))))

;                     (2) DO SUBSTITUTIONS

;This is VERY conservative.  Note if you substitute variables too freely,
;you can run into troubles with binding scopes and setqs of the variables
;in other places, but just using temporary vars is guaranteed to have the
;right semantics all of the time.  Any decent compiler will then minimize
;the number of variables actually used at run time.

(cl:defun do-substitution (&aux code ret killable)
  (dofrags (f)
    (when (and (= (length (rets f)) 1)
	       (not (off-line-spot (car (rets f))))
	       (null (args f))
	       (null (epilog f))
	       (= 1 (length (setq code (append (prolog f) (body f)))))
	       (eq (var (setq ret (car (rets f)))) (setq-p (setq code (car code))))
	       (or (constantp (setq code (caddr code)))
		   (and (eq-car code 'function) (symbolp (cadr code)))))
      (setq killable (not (null (nxts ret))))
      (dolist (arg (nxts ret))
	(cond ((and (not (off-line-spot arg))
		    (not (contains-p (var arg) (rets (fr arg))))
		    (cond ((or (and (eq-car code 'function) (symbolp (cadr code)))
			       (numberp code) (null code) (eq code T)) T)
			  ((constantp code)
			   (and (null (cdr (nxts (car (rets f)))))
				(not-contained-twice (list (var arg))
						     (list (prolog (fr arg))
							   (body (fr arg))
							   (epilog (fr arg))))))))
	       (nsubst code (var arg) (fr arg))
	       (-dflow ret arg)
	       (-arg arg))
	      (T (setq killable nil))))
      (if killable (-frag f)))))

;                     (2.5) KILL DEAD CODE

(cl:defun kill-dead-code ()
  (setq *graph* (nreverse *graph*))
  (dofrags (f)
    (dolist (r (rets f))
      (if (and (free-out r) (null (nxts r))) (kill-ret r)))
    (when (not (or (rets f) (must-run f)))
      (reap-frag f)))
  (setq *graph* (nreverse *graph*)))

(cl:defun reap-frag (frag)
  (dolist (a (args frag))
    (cl:let ((r (prv a)))
      (-dflow r a)
      (when (null (nxts r)) (kill-ret r))))
  (setq *graph* (delete frag *graph*)))

;                           (3) DO SPLITTING

;Splitting cuts up the graph at all of the correct places, and creates a
;lisp expression which, when evaluated will merge everything together.
;Things area done this way so that all of the splitting will happen
;before any of the merging.  This makes error messages better and allows
;all the right code motion to happen easily.

(cl:defun do-splitting (*graph*)
  (reset-marks 0)
  (non-series-split *graph*))

(defmacro doing-splitting (&body body)
  `(cond ((null (cdr *graph*)) (list 'quote (car *graph*)))
	 (T (reset-marks 1) (prog1 (progn ,@ body) (reset-marks 0)))))

(defmacro doing-splitting1 (&body body)
  `(cond ((null (cdr *graph*)) *graph*)
	 (T (reset-marks 1) (prog1 (progn ,@ body) (reset-marks 0)))))

;  The following breaks the expression up at all the points where there is no series
;data flow between the subexpressions.  Non-series port isolation
;guarantees that this split is possible, cutting only non-series dflows.
;(If there is no data flow, you might not have to cut any data flow.)  If
;*graph* is a complete expression (i.e., one that does not have any series
;inputs or outputs overall), then the subexpressions cannot have external
;series inputs or outputs.
;  Non-series splitting typically breaks the expression up into a large
;number of fragments.  Great care is taken to make sure that these
;fragments will be reassembled without changing their order.  This is
;important so that the user's side-effects will look reasonable.  Careful
;attention has to be paid to the dflow constraints when figuring out where
;to put the series subexpressions.  They are put where the last fn in them
;suggests, within the limits of dflow.
;  Note that the only way the user can write something that has some
;side-effects is to write a side-effect expression that turns into a
;non-series-computation (via isolate-non-series) or to write something in a
;functional argument to a higher-order series function.  the functions here
;make things come out pretty well in the first case; there is not much
;anybody could do about the second case.

(cl:defun non-series-split (*graph*)
  (cl:let ((subexprs (disconnected-split *graph*)))
    (setq subexprs (reorder-frags subexprs))
    (cons 'non-series-merge-list
	  (mapcar #'(lambda (s) (off-line-split s)) subexprs))))

(cl:defun reorder-frags (form)
  (cond ((eq-car form 'dflow) (mapcan #'reorder-frags (cdr form)))
        ((eq-car form 'no-dflow)
         (cl:let ((sublists (mapcar #'reorder-frags (cdr form)))
		    (result nil) min-num min-sublist)
           (setq sublists
                 (mapcar #'(lambda (l) (cons (order-num (car l)) l)) sublists))
           (loop (if (null (cdr sublists))
                   (return (nreconc result (cdr (car sublists)))))
                 (setq min-num (car (car sublists)) min-sublist (car sublists))
                 (dolist (sub (cdr sublists))
                   (when (< (car sub) min-num)
                     (setq min-num (car sub) min-sublist sub)))
                 (push (pop (cdr min-sublist)) result)
                 (if (null (cdr min-sublist))
                   (setq sublists (delete min-sublist sublists))
                   (setf (car min-sublist) (order-num (cadr min-sublist)))))))
        (T (list form))))

(cl:defun order-num (frags)
  (position (car (last frags)) *graph*))

;We have to do non-dflow splitting and non-series-dflow-splitting separately
;in order to get error messages about non-isolated non-series dflow right.
;This breaks the expression up at points where there is no data flow
;between the subexpressions.  Since the size of part1 is minimized it is
;known that part1 must be fully connected.

(cl:defun disconnected-split (*graph*)
  (doing-splitting1
    (cl:multiple-value-bind (part1 part2)
	(split-after (car *graph*) #'(lambda (r a) (declare (ignore r a)) nil))
      (cond ((null part2) (non-series-dflow-split part1))
	    (T (setq part1 (non-series-dflow-split part1))
	       (setq part2 (disconnected-split part2))
	       `(no-dflow ,part1
			  ,@(if (eq-car part2 'no-dflow)
				(cdr part2)
				(list part2))))))))

;This finds internal non-series dflows and splits the graph at that point.
;It may be necessary to cut more than one dflow when splitting.  Therefore,
;no matter how we do things, it will always be possible that either of the
;parts will have more non-series dflow in it.  To see this, note the
;following example:
#|(let ((e (scan x)))
    (values (foo (reverse (collect e)))
	    (collect-last e (car (bar y))))) |#
;  The order of frags on the graph is going to be scan, collect, reverse, foo,
;bar, car, collect-last.  If you start on either the first frag, or the first
;non-series dflow, or the last frag, or the last dflow, there are going to be
;another non-series dflow in each half.  (Note starting from the front, the
;non-series dflow from car to collect-last is going to be pulled into the
;first part.  And in general, starting from the front puts lots of non-series
;dflow in the second part.)
;  The best we can do is construct one part so that it is known that that part
;is connected.  The method used here ensures that the first part is
;connected by minimizing it.
;  Note there is an implicit assumption here that making a cut through
;a bundle of isolated non-series dflows cannot converted a
;non-isolated one into an isolated one.  If this could happen, we
;would fail to detect some problems, and the overall theory would be
;overly strict.

(cl:defun non-series-dflow-split (*graph*)
  (doing-splitting1
    (block top
      (dofrags (f)
	(dolist (ret (rets f))
	  (when (not (series-var-p ret))
	    (dolist (arg (nxts ret))
	      (when (marked-p 1 (fr arg))
		(return-from top (do-non-series-dflow-split ret arg)))))))
      *graph*)))

(cl:defun do-non-series-dflow-split (ret arg)
  (cl:let ((frag1 (fr ret))
	     (frag2 (fr arg)))
    (cl:multiple-value-bind (part1 part2)
	(split-after frag1 #'(lambda (r a)
			       (declare (ignore r))
			       (not (series-var-p a))))
      (when (member frag2 part1)
	(rrs 21 "~%Constraint cycle passes through the non-series output ~
		  at the beginning of the data flow from:~%"
	     (code frag1) "~%to:~%" (code frag2)))
      (setq part1 (non-series-dflow-split part1))
      (setq part2 (disconnected-split part2))
      `(dflow ,@(if (eq-car part1 'dflow) (cdr part1) (list part1))
	      ,@(if (eq-car part2 'dflow) (cdr part2) (list part2))))))

;At this next stage, we split based on off-line ports.  (Note that all
;non-series splitting must be totally complete at this time.)  Several
;other things are important to keep in mind.  First, whenever we split on
;an off-line output that has more than one dflow from it to on-line ports,
;we insert a dummy identity frag so that there will be only one dflow from
;the off-line port to on-line ports (the multiple dflows come from the
;output of the dummy frag).  There are three benefits to this.  First,
;doing this allows us to make the split cutting only one dflow arc.  This
;guarantees that both parts remain connected and therefore we don't have to
;call disconnected-split again.
;  Second, when checking for isolation when doing splitting at the
;same time, we need to have the property that doing a split cannot
;cause a non-isolated arc to become isolated.  If we cut more than one
;series dflow when splitting we could make something else be isolated.
;Consider the program below.
#|(let ((e (split #'plusp (scan x))))
    (collect (#M+ e (f (g e))))) |#
;Note that the offline output of split is isolated, but neither the
;input of f or the output of g is isolated.  If you cut both dflows
;from the split when doing a split, these two ports look isolated in
;the part they are in.
;  Third, the dummy frag helps keep things straight during later
;merging.  The key problem is that if there is more than one
;on-line destination port, then we must make sure that
;they stay on-line, because they may not be isolated.  The dummy frag
;essentially records the requirement that the destinations must keep
;in synchrony.
;  Note that when splitting, things will come out exactly the same no
;matter which part is minimized, because the whole expression is
;connected and there is no non-series dflow.  As a result, there
;cannot be more than one way to split the expression---Every function
;must be forced to one half or the other.
;  By the same argument used with regard to non-series dflow, either
;part can still have off-line ports in it that have not been split on.
;  Note that even if the whole does not have any external series
;ports, the two pieces can.  At least one will be off-line, the other
;can be on-line.  Note that if the splitting is being done based on an
;off-line input, then the output in part one can be used in more than
;one place.  In particular, it can be used by another off-line input
;which is now still in part1.  This forces complex merging cases
;to be handled.

(cl:defun off-line-split (*graph*)
  (doing-splitting
   (block top
     (dofrags (f)
       (dolist (ret (rets f))
         (cl:let ((args nil))
           (dolist (arg (nxts ret))
             (when (marked-p 1 (fr arg))
               (cond ((off-line-spot arg)
                      (setq args (list arg))
                      (return nil))
                     ((off-line-spot ret)
                      (push arg args)))))
           (when args
             (when (and (cdr args) (off-line-spot ret))
               (setq args (list (insert-off-line-dummy-frag ret args))))
             (return-from top (do-off-line-split ret (car args)))))))
     `(on-line-merge ',*graph*))))

(cl:defun insert-off-line-dummy-frag (ret args)
  (cl:let* ((var (new-var 'oo))
	      (dummy-ret (make-sym :var var :series-var-p T))
	      (dummy-arg (make-sym :var var :series-var-p T))
	      (dummy-frag (make-frag :code (code (fr (car (nxts ret)))))))
    (+arg dummy-arg dummy-frag)
    (+ret dummy-ret dummy-frag)
    (dolist (arg args)
      (-dflow ret arg)
      (+dflow dummy-ret arg))
    (+dflow ret dummy-arg)
    (cl:let ((spot (member (fr ret) *graph*)))
      (rplacd spot (cons dummy-frag (cdr spot))))
    (mark 1 dummy-frag) ;so is in currently being considered part.
    dummy-arg))

(cl:defun do-off-line-split (ret arg)
  (cl:let ((frag1 (fr ret))
	     (frag2 (fr arg)))
    (cl:multiple-value-bind (part1 part2)
	(split-after frag1 #'(lambda (r a)
			       (and (eq r ret) (eq a arg))))
      (when (member frag2 part1)
	(if (off-line-spot arg)
	    (rrs 23 "~%Constraint cycle passes through the off-line input ~
		       at the end of the data flow from:~%"
		    (code frag1) "~%to:~%" (code frag2)))
	    (rrs 22 "~%Constraint cycle passes through the off-line output ~
		       at the start of the data flow from:~%"
		    (code frag1) "~%to:~%" (code frag2)))
      `(off-line-merge ,(off-line-split part1) ',ret
		       ,(off-line-split part2) ',arg))))

;This splits the graph by dividing it into two parts (part1 and part2)
;so that to-follow is in part1, there is no data flow from part2 to
;part1 and all of the data flow from part1 to part2 satisfies the
;predicate CROSSABLE.
;  The splitting is done by marker propagation (using the marker 2).
;The algorithm used has the effect of minimizing part1, which among
;other things, guarantees that it is fully connected.

(cl:defun split-after (frag crossable)
  (mark 2 frag)
  (cl:let ((to-follow (list frag)))
    (loop (if (null to-follow) (return nil))
	  (cl:let ((frag (pop to-follow)))
	    (dolist (a (args frag))
	      (cl:let* ((r (prv a)))
	       (when (= (marks (fr r)) 1) ;ie 1 but not 2
		 (push (fr r) to-follow)
		 (mark 2 (fr r)))))
	    (dolist (r (rets frag))
	      (dolist (a (nxts r))
		(when (and (= (marks (fr a)) 1) ;ie 1 but not 2
			   (not (cl:funcall crossable r a)))
		  (push (fr a) to-follow)
		  (mark 2 (fr a))))))))
  (cl:let ((part1 nil) (part2 nil))
    (dofrags (f 1)
      (if (marked-p 2 f) (push f part1) (push f part2)))
    (reset-marks 0)
    (values (nreverse part1) (nreverse part2))))

;                         (4) DO MERGING

;  The merging of frags into a single frag follows the pattern of splitting
;determined above.  At the leaves of the tree of splits are subexpressions where
;some number of frags are connected solely by on-line series data flow.  All these
;frags are combined into a single frag in one step.  As long as every termination
;point is connected to every output point, this is a trivial operation.  If not,
;flags and such have to be inserted to ensure that the result will have the
;property that all of the outputs are produced as soon as ANY input runs
;out of elements.
;  After this is done, things proceed by doing two different kinds of mergings
;based on the two different types of splitting.  One case is particularly
;simple.  Merging frags connect by non-series data flow or no data flow at
;all, is trivial.
;  Merging frags connected by series dflow touching at least one off-line
;port is where the key difficulties lie.  There are three areas of trouble.
;First, things have to be carefully arranged so that termination will work out
;right in situations where not every termination point is connected to every
;output.  Second, if an off-line output is connected to an off-line input,
;one of the frags has to be turned inside out.
;  Third, operations concerning these issues and even the simple cases of
;off-line merging can convert extraneous on-line ports (one not directly
;participating in the merging) into off-line ports.
;  (One issue here is that we must be sure that this port is isolated.
;We know it is, because it cannot be an extraneous port on the frag unless it is
;either a port on the expression as a whole (and therefore touched by no
;dflow) or touched by isolated dflow.)
;  When conversion to off-line happens as part of the internal course of
;events, it indicates that the code is going to get messy, but need not
;concern the user.  (In fact, the code may even be quite efficient, it will
;just look like a real mess.)  Note that if you are just writing a simple
;series expression that neither reads nor writes a series as a whole, there
;will be no externally visible series ports, and you need not worry about
;this issue.
;  However, when you are defining a new series functions, there are external
;series ports.  Given that on-line ports are much more usable than off-line
;ones, it is unfortunate that doing odd things with the termination (for
;example) can make all your ports be off-line.

;  Two cases are always simple.  If an extraneous input or output
;carries a non-series value, then there is never a problem.  If it is
;an input than it must be available from the very start of computation
;and therefore will always be readable no matter how the frags are
;combined.  If the port is an output, then it does not need to be
;available until after everything is done, and the strongly connected
;check insures that it will be eventually computed.
;  Things are also basically simple if an extraneous input or output is
;off-line.  In this situation, a specific marker says exactly where
;connected computation should be put, and this marker will always end
;up in an appropriate place no matter how the fragments are combined.
;The only thing which requires care is making sure that these
;markers stay at top level.

;  One problem case, however, is that it is possible for an off-line
;output to be used by an off-line input.  This can cause a splitting
;to happen that ends up in a situation where an off-line output is used
;both internally and externally.  If so, the output has to be
;preserved the first time it is used so that it can be used again.
;  On the other hand, if an extraneous input or output is on-line,
;significant complexities can arise.  If an extraneous port is
;on-line then it may have to be changed into an off-line port.
;Fortunately, things are arranged so that a graph is never split by
;breaking an on-line to on-line data flow.  However, an on-line port
;can be on one end of a broken data flow.  Nevertheless, most
;instances of extraneous on-line ports come from weird lambda-series bodies.
;Except in simple situations extraneous on-line ports are not
;supported unless they come from complete expressions.

;    Consider the simplest mergings first.

;  Two frags are connected by non-series dflow (or no dflow).
;(When processing complete series expressions it will always be the case that
;both frags are non-series frags.  Further, the way splitting happens
;guarantees that any series ports are direct ports of the expression as a whole.)
;  Merging is trivial as long as at least one of the frags is non-series.  If
;one has series ports, it can be left totally alone.  The other can be placed
;entirely in the prolog (if it is first) or epilog.
;  If both frags are series frags, things are complex.  You must evaluate
;the first one first and completely to get the non-series output(s) (if
;any) that are used by the second.  This will force the first frag to make
;all its outputs normally.  Then you have to evaluate the other one.  To do
;this, one frag or the other has to be severely distorted.  This process
;will make all of the series ports on the modified frag be off-line.
;  The program below converts the first frag into a tight loop that runs in
;the beginning of the body.  (This is essential to preserve the invariant that
;off-line-spots are only in bodies, but it makes a real mess and forces all
;the series inputs off-line.  Also note the way the prolog of the other frag
;has to be moved.)  (Note that the off-line ports created are
;isolated, because they are on the outside of the expression as a whole.)


(cl:defun non-series-merge-list (&rest frags)
  (cl:let ((frag (pop frags)))
    (loop (if (null frags) (return frag))
	  (setq frag (non-series-merge frag (pop frags))))))

(cl:defun non-series-merge (ret-frag arg-frag)
  (handle-dflow ret-frag
    #'(lambda (r a) (declare (ignore r)) (eq (fr a) arg-frag)))
  (when (not (non-series-p ret-frag))
    (if (non-series-p arg-frag)
	(implicit-epilog arg-frag)
	(eval-on-first-cycle ret-frag arg-frag)))
  (merge-frags ret-frag arg-frag))

(cl:defun implicit-epilog (frag)
  (setf (epilog frag) (prolog frag))
  (setf (prolog frag) nil)
  frag)

(cl:defun eval-on-first-cycle (frag arg-frag)
  (cl:let ((b (new-var 'b))
	     (c (new-var 'c))
	     (lab (new-var 's))
	     (flag (new-var 'terminate)))
    (make-ports-off-line frag nil)
    (dolist (a (args frag))
      (when (and (series-var-p a) (null (off-line-exit a)))
	(setf (off-line-exit a) b)))
    (make-inputs-off-line arg-frag nil)
    (nsubst b END frag)
    (push (list flag t) (aux frag))
    (setf (body frag)
	  `((if ,flag (go ,c)) ,@(prolog frag)
	    ,lab ,@(body frag) (go ,lab)
	    ,b ,@(epilog frag) (setq ,flag t) ,@(prolog arg-frag) ,c))
    (setf (prolog frag) nil)
    (setf (prolog arg-frag) nil)
    (setf (epilog frag) nil)))

;  A graph of many frags is connected solely by on-line data flow.
;(Here, even when operating on complete series expressions, it is expected
;that there are extraneous series inputs and outputs, and that
;internally used series ports can be used outside as well.  However, every
;external use must be isolated.)
;  Here things are in general simple, and everything can just be merged
;together in an order compatible with the dflow and everything will be fine
;and all of the extraneous ports will be left alone.
;  However, if there are any termination points that are not connected to
;every output point, we have a problem.  Things have to be altered so
;that these termination points don't prematurely stop things they should not
;stop.  This is done by inserting flags that delay termination until the
;correct time.  This is done as follows.
;  (1) find each termination point and output point.  Test each termination
;point to see whether it is total (i.e., is connected to every output and
;therefore calls for stopping everything.)  Total termination points can act
;by simply branching to END when they trigger.
;  If a termination point is not total, then a flag has to be gensymed
;corresponding to it and the point has to be changed so that it sets the flag
;(which starts with a value of NIL) to T instead of branching when exit
;occures.  For non-total termination points that are series inputs, this
;means that the input will have to become an off-line port that catches
;termination.
;  (2) for each non-total termination point we have to figure out what frags
;it controls.  First, frags the termination point has data flow to are forced
;to stop when it stops.  Second, once EVERY output a frag has data flow to
;has been completed, the frag can stop too.
;  (In addition, we must note that once every frag has stopped, the loop as a
;whole should stop.  If there is at least one total termination point and
;there is at least one output point that is controlled only by total termination
;points, then we don't have to do anything special.  When a total termination
;point stops everything stops and we always have to continue computing as
;long as none of the total termination points have stopped.  However, if the
;above is not the case, we have to add a new termination test that checks to
;see if all of the outputs have completed, and stop everything.)
;  (2a) follow the dflow from each non-total termination point and note that
;the termination point itself, and every frag you reach must stop as soon as
;the termination point does.  This is done by adding FLAG into the list of
;control flags for each frag.  (This list is an implicit OR that specifies
;when to STOP executing the frag)
;  (2b) Start at each output point and get the set of flags that control it.
;Follow the dflow backward from each output point and note what frags feed
;into it.  Once this is done, create a new entry
; (AND (OR . output-flag-set1) (OR . output-flag-set2) ...)
;in the list of control flags.
;  The above can be done in two highly efficient marking sweeps.  The first
;of which also determines whether there are any non-total termination points
;we have to worry about.
;  Finally, we simplify each of the control expressions and do the
;merging inserting the correct tests of flags.  (I could think about
;sorting the frags as much as possible consistent with dflow so that
;adjacent frags have the same expressions, however, this might be bad with
;respect to side-effects.)

(cl:defun on-line-merge (*graph*) ;merge everything, all dflow is on-line.
  (if (null (cdr *graph*)) (car *graph*)
      (cl:let ((frag nil))
	(reset-marks 1)
	(check-termination *graph*)
	(dofrags (f)
	  (handle-dflow f
	    #'(lambda (r a) (declare (ignore r)) (marked-p 1 (fr a))))
	  (if (null frag) (setq frag f) (setq frag (merge-frags frag f))))
	(reset-marks 0)
	(maybe-de-series frag))))

;This is used for the variable renaming part of all kinds of dflow.  Rets
;must be saved either if they have no dflow from them (they are outputs of
;the whole top level expression) or if there is a dflow to a frag that is not
;currently being dealt with.  The functional argument specifies which dflow
;are which.

(cl:defun handle-dflow (source handle-this-dflow)
  (dolist (ret (rets source))
    (cl:let ((ret-killable (not (null (nxts ret)))))
      (dolist (arg (nxts ret))
	(cond ((not (cl:funcall handle-this-dflow ret arg))
	       (setq ret-killable nil))
	      (T (nsubst (var ret) (var arg) (fr arg))
		 (-dflow ret arg)
		 (-arg arg))))
      (if ret-killable (-ret ret)))))

;flag meanings
;1- marks region of interest.
;2- marks places to start output point sweep.
;4- marks places to start termination point sweep.
;4- mark individual output points and termination points.
;this function assumes that on-line-merge will merge frags in the order they
;are on *graph*.

(cl:defun check-termination (*graph*)
  (block nil
    (cl:let ((counter 8.) (all-term-counters 0)
	       (outputs nil) (terminations nil)
	       (problem-terminations nil) all-terminated conditions current-label)
      (dofrags (f 1)
	(when (or (must-run f)
		  (some #'(lambda (r)
			    (or (null (nxts r))
				(some #'(lambda (a) (not (marked-p 1 (fr a))))
				      (nxts r))))
			(rets f)))
	  (push (list counter f) outputs)
	  (mark (+ 2 counter) f)
	  (setq counter (* 2 counter)))
	(when (or (active-terminator-p f)
		  (some #'(lambda (a)
			    (and (series-var-p a)
				 (not (off-line-exit a))
				 (not (marked-p 1 (fr (prv a))))))
			(args f)))
	  (push (list counter f) terminations)
	  (mark (+ 4 counter) f)
	  (setq all-term-counters (+ all-term-counters counter))
	  (setq counter (* 2 counter))))

;;;first sweep to test connection of terms to outputs.
      (dofrags (f 5)                                ; 5 = 1+4
	(cl:let ((current-marks (logandc1 2 (marks f)))) ;strips out 2 bit
	  (dolist (a (all-nxts f))
	    (when (marked-p 1 (fr a))
	      (mark current-marks (fr a))))))
      (dolist (oentry outputs)
	(when (not (marked-p all-term-counters
			     (cadr oentry))) ;99% of time will be marked
	  (dolist (tentry terminations)
	    (when (not (marked-p (car tentry) (cadr oentry)))
	      (pushnew tentry problem-terminations)))))
      (if (null problem-terminations) (return nil))

;;;make the flags and get them initialized
     (dolist (tentry problem-terminations)
       (cl:let ((flag (make-set-flag-rather-than-terminate (cadr tentry))))
	 (dolist (oentry outputs)
	   (when (marked-p (car tentry) (cadr oentry))
	     (push flag (cddr oentry))))))

;;;second sweep to test connection of everything to outputs.
      (cl:let ((*graph* (reverse *graph*)))
	(dofrags (f 3)                                       ; 3 = 1+2
	  (cl:let ((current-marks (logandc1 4 (marks f)))) ;strips out 4 bit
	    (dolist (a (all-prvs f))
	      (when (marked-p 1 (fr a))
		(mark current-marks (fr a)))))))
      (setq all-terminated (make-test (mapcar #'cddr outputs)))
      (when all-terminated
	(push `(if ,all-terminated (go ,END)) (body (car *graph*))))

;;;add conditionalization to each frag
      (setq conditions
	    (mapcar #'(lambda (f)
			(make-test
			  (mapcar #'cddr
				  (remove-if-not #'(lambda (e)
						     (marked-p (car e) f))
						 outputs))))
		    *graph*))
      ;could do some sorting here based on similarity between conditions.
      (dotimes (i (length *graph*))
	(cl:let ((condition (elt conditions i))
	      (frag (elt *graph* i)))
	  (when (not (equal condition all-terminated))
	    (make-outputs-off-line frag)
	    ;inputs are termination points and are already off-line if need be.
	    (when (or (= i 0) (not (equal condition (elt conditions (1- i))))
		      (find (elt *graph* (1- i)) problem-terminations :key #'cadr))
	      (setq current-label (new-var 'skip))
	      (push `(if ,condition (go ,current-label)) (body frag)))
	    (when (or (= i (1- (length *graph*)))
		      (not (equal condition (elt conditions (1+ i))))
		      (find frag problem-terminations :key #'cadr))
	      (setf (body frag) (nconc (body frag) `(,current-label)))))))  )))

(cl:defun make-set-flag-rather-than-terminate (frag)
  (cl:let* ((B (new-var 'bb))
	      (C (new-var 'cc))
	      (flag (new-var 'terminated)))
    (make-ports-off-line frag nil)
    (dolist (a (args frag))
      (when (and (series-var-p a) (not (off-line-exit a)))
	(setf (off-line-exit a) B)))
    (nsubst B END (body frag))
    (push `(,flag T) (aux frag))
    (push `(setq ,flag nil) (prolog frag))
    (setf (body frag) (nconc (body frag) `((go ,C) ,B (setq ,flag T) ,C)))
    flag))

;the challenge here is making as simple a test as possible

(cl:defun make-test (and-of-ors)
  (if (null and-of-ors) T
    (cl:let ((top-level-or nil) (residual-and-of-ors nil))
      (dolist (f (car and-of-ors))
	(when (every #'(lambda (or) (member f or)) (cdr and-of-ors))
	  (push f top-level-or)
	  (setq and-of-ors (mapcar #'(lambda (or) (remove f or)) and-of-ors))))
      (when (member nil and-of-ors) (setq and-of-ors nil))
      (dolist (or and-of-ors)
	(when (notany #'(lambda (other-or)
			  (and (not (eq or other-or)) (subsetp other-or or)))
		      and-of-ors)
	  (push or residual-and-of-ors)))
      (setq residual-and-of-ors
	    (mapcar #'(lambda (or)
			(if (cdr or) `(or . ,or) (car or)))
		    residual-and-of-ors))
      (when residual-and-of-ors
	(push `(and . ,(nreverse residual-and-of-ors)) top-level-or))
      (cond ((null top-level-or) nil)
	    ((null (cdr top-level-or)) (car top-level-or))
	    (T `(or . ,(nreverse top-level-or)))))))

;  Two frags are connected by dflow touching at least one off-line port.
;(Even in complete expressions, there can be extraneous series
;ports.  (e.g., going to other subexpressions created in other splits.)
;However, any dflow touching these ports must be isolated.)
;Note that if the output port is on-line there may be other dflow starting on
;it other than the one in question.
;  The first difficulty in this case involves termination.  With regard to the
;second frag, there is no problem.  If the second frag is the first to stop,
;then it must have produced all its outputs.  If the first frag is the first to
;stop, then it must have produced all its outputs which either means that the
;second must also stop, or the second will catch the termination of the first.
;  Further there is no trouble with the first frag as long as either (1) the
;second frag has no termination points other than the one in question (i.e.,
;has no series inputs without off-line-exits other than possibly the
;one in question and cannot by itself terminate) or (2) the first frag does
;not have any output points other than the one in question (i.e., has no
;other outputs, and does not have the must-run flag set) and this output is
;not used anywhere other than by the input in question.  In case (1) running
;the second frag forces the complete running of the first frag.  In case (2),
;it does not matter if the first frag is run completely or not.
;  If neither of the cases above applies, we have to do some hard work.  We
;know that the destination frag is a termination point and either, (a) the
;source frag has a non-series output or has the must-run flag set or (b)
;there is data flow from series outputs of the source frag to more than one
;place (i.e., either fan out from one, or dflow from two different ones).
;  In case (a) things are simple, we just have to change the destination
;frag so that it always reads all of the elements of the input in question.
;This can be done by catching the termination of the frag caused by other
;things, and using a flag to force execution to continue until the input
;runs out.  This transformation causes all the other series ports to become
;off-line.
;  Case (b) is more complex, the source frag might not be able to terminate
;at all, and even if it can, it might not terminate soon enough.  We must
;look at all of the destinations of dflow from it (not just the one we are
;looking at now) and see which ones of them are termination points.  What
;we want is for the source to terminate exactly when all of the
;destinations terminate (if ever).  If at least one of the destinations is
;not a termination point, then we can proceed exactly as in case (a).  If
;none of them are, then we can still proceed the same, but we have to add a
;test to the first frag that causes termination as soon as all of the
;destinations have stopped.  This requires flags to be set in the destinations.
;  (Note that we could probably use simpler frags and things if we figured
;out all the places where we were going to have to do this before merging
;the on-line subexpressions in the first place.  However, this would make the
;code more complex and is not worth doing given that it is rather unlikely
;for series expressions to have more than one output in any case.  Note that
;the prior version of this macro package just outlawed every problematical
;case.  Doing things with more efficiency is a possible future research
;direction.)

;  The second difficulty involves actually doing the merging.
;  A- The ret is off-line and the arg is on-line
;There are two basic ways in which this can be handled.
; A1- The most straightforward way is to insert the arg frag into the
;off-line-spot in the ret-frag.  This is very simple and allows on-line inputs
;and outputs of the ret-frag to remain unchanged.  However, on-line inputs and
;outputs of the arg-frag are forced to become off-line.
; A2- The ret-frag is turned inside out and converted into an enumerator, which
;has on-line data flow to the arg-frag.  This requires the use of a flag
;variable, and the making off-line of any on-line inputs or outputs of the
;ret-frag.  However, it allows any extraneous inputs and outputs of the arg-frag
;to remain unchanged.
; If either of the two frags has no extraneous on-line ports, then the
;appropriate combination method above is used and everything works out great.
;If they both have extraneous on-line ports, then which every one has fewer of
;these ports has them changed to off-line ports and the appropriate process
;above is then applied.
;  In either case, special care has to be taken to insure that the off-line
;output will still exist if it is used some place other than in the arg-frag.
;(It is possible that it will exist, but will get changed to on-line.  This does
;not cause confusion since the input it is connected to must be
;off-line--otherwise there would be only one dflow from the output.)

;  B- The ret is on-line and the arg is off-line.  This case is closely
;analogous to the one above.  Again, there are two basic ways to proceed.
; B1- The most straightforward way is to insert the ret frag into the
;off-line-spot in the arg-frag.  This has the feature that it is very simple and
;allows all on-line inputs and outputs of the arg-frag to remain unchanged.
;However, on-line inputs and outputs of the ret-frag are forced to become
;off-line.
; B2- The arg-frag is turned inside out and converted into a reducer which
;receives on-line data flow from the ret-frag.  This requires the use of a flag
;variable, and it forces off-line any extraneous on-line inputs or outputs of
;the arg-frag.  However, it allows any extraneous inputs and outputs of the
;ret-frag to remain unchanged.
; If either of the two frags has no extraneous ports, then the appropriate
;combination method above is used and everything works out great.  If the both
;have extraneous ports then whichever has fewer has them changed to off-line and
;things proceed as above.
;  C- the ret and arg are both off-line.  Here it is not possible to
;simultaneously substitute the frags into each other.  However, it is possible
;to combine them after A2 is applied to the ret-frag or B2 is applied to the
;arg-frag.  Again this presents two options and it is possible to preserve
;either the extraneous ports of the ret-frag or the arg-frag, but not both.
;  Note we have to be prepared for the general case more often than you might
;expect, because the combination process can cause ports to become off-line.

(cl:defun some-other-termination (arg)
  (or (active-terminator-p (fr arg))
      (plusp (count-if #'(lambda (a)
			   (and (not (eq a arg))
				(series-var-p a)
				(not (off-line-exit a))))
		       (args (fr arg))))))

(cl:defun off-line-merge (ret-frag ret arg-frag arg)
  (when (and (some-other-termination arg)
	     (or (> (length (rets ret-frag)) 1)
		 (must-run ret-frag)
		 (> (length (nxts ret)) 1)))
    (cl:let ((destinations nil))
      (dolist (r (rets ret-frag) nil)
	(when (series-var-p r)
	  (setq destinations (append destinations (nxts ret)))))
      (if (or (must-run ret-frag)
	      (not (every #'series-var-p (rets ret-frag)))
	      (not (every #'some-other-termination destinations)))
	  (make-read-arg-completely arg)
	(cl:let ((cnt (new-var 'cnt)))
	  (push `(,cnt T) (aux ret-frag))
	  (push `(setq ,cnt ,(length destinations)) (prolog ret-frag))
	  (push `(if (zerop ,cnt) (go ,end)) (body ret-frag))
	  (dolist (a destinations)
	    (make-read-arg-completely a cnt))))))
  (handle-dflow (fr ret) #'(lambda (r a) (declare (ignore r)) (eq (fr a) arg-frag)))
  (cl:let* ((ret-rating (count-on-line ret-frag))
	      (arg-rating (count-on-line arg-frag)))
    (cond ((not (off-line-spot arg))
	   (if (> arg-rating ret-rating)
	       (convert-to-enumerator ret nil)
	       (substitute-in-output ret arg)))
	  ((not (off-line-spot ret))
	   (if (and (> ret-rating arg-rating) (null (off-line-exit arg)))
	       (convert-to-reducer arg)
	       (substitute-in-input ret arg)))
	  (T (cond ((and (> ret-rating arg-rating) (null (off-line-exit arg)))
		    (convert-to-reducer arg)
		    (substitute-in-output ret arg))
		   (T (convert-to-enumerator ret (off-line-exit arg))
		      (substitute-in-input ret arg))))))
  (maybe-de-series (merge-frags ret-frag arg-frag)))

(eval-when (eval load compile)

(cl:defun count-on-line (frag)
  (+ (length (find-on-line (args frag))) (length (find-on-line (rets frag)))))

(cl:defun find-on-line (syms)
  (do ((s syms (cdr s)) (r nil))
      ((null s) (nreverse r))
    (when (and (series-var-p (car s)) (null (off-line-spot (car s))))
      (push (car s) r))))

(cl:defun make-read-arg-completely (arg &optional (cnt nil))
  (cl:let* ((frag (fr arg))
	      (B (new-var 'bbb))
	      (C (new-var 'ccc))
	      (flag (new-var 'ready-to-terminate)))
    (make-ports-off-line frag nil)
    (dolist (a (args frag))
      (when (and (not (eq a arg)) (not (off-line-exit a)))
        (setf (off-line-exit a) B)))
    (nsubst B END (body frag))
    (push `(,flag T) (aux frag))
    (push `(setq ,flag nil) (prolog frag))
    (setf (body frag)
          (nsubst-inline (if (not (off-line-exit arg))
                           `((go ,C) ,B ,@(if cnt `((if (null ,flag) (decf ,cnt))))
                             (setq ,flag T) 
                             ,C ,(off-line-spot arg) (if ,flag (go ,C)))
                           (cl:let ((CF (new-var 'CF))
				      (CD (new-var 'CD))
				      (exit (off-line-exit arg)))
                             (setf (off-line-exit arg) CF)
                             `((go ,C) ,B  (if ,flag (go ,end)) (setq ,flag T)
                               ,C  ,(off-line-spot arg) (go ,CD)
                               ,CF (if ,flag (go ,end)) (setq ,flag T) (go ,exit)
                               ,CD (if ,flag (go ,C)))))
                         (off-line-spot arg) (body frag)))))

(cl:defun substitute-in-output (ret arg)
  (cl:let ((ret-frag (fr ret)) (arg-frag (fr arg)))
    (make-ports-off-line arg-frag (off-line-exit arg))
    (setf (body ret-frag)
	  (nsubst-inline (body arg-frag) (off-line-spot ret) (body ret-frag)
			 (nxts ret)))
    (setf (body arg-frag) nil)))

(cl:defun substitute-in-input (ret arg)
  (cl:let ((ret-frag (fr ret)) (arg-frag (fr arg)))
    (make-ports-off-line ret-frag (off-line-exit arg))
    (when (off-line-exit arg)
      (dolist (a (args (fr ret)))
	(if (and (series-var-p a) (not (off-line-exit a)))
	    (setf (off-line-exit a) (off-line-exit arg))))
      (nsubst (off-line-exit arg) END (body ret-frag)))
    (setf (body arg-frag)
	  (nsubst-inline (body ret-frag) (off-line-spot arg) (body arg-frag)))
    (setf (body ret-frag) nil)))

(cl:defun make-ports-off-line (frag off-line-exit)
  (make-inputs-off-line frag off-line-exit)
  (make-outputs-off-line frag))

(cl:defun make-outputs-off-line (frag)
  (dolist (out (find-on-line (rets frag)))
    (when (or (null (nxts out))
	      (some #'(lambda (in)
			(not (marked-p 1 (fr in)))) ;needed by check-termination
		    (nxts out)))
      (cl:let ((-X- (new-var '-x-)))
	(setf (off-line-spot out) -X-)
	(setf (body frag) `(,@(body frag) ,-X-))))))

(cl:defun make-inputs-off-line (frag off-line-exit)
  (dolist (in (find-on-line (args frag)))
    (when (not (marked-p 1 (fr (prv in)))) ;needed by check-termination
      (cl:let ((-X- (new-var '-xx-)))
	(setf (off-line-spot in) -X-)
	(setf (off-line-exit in) off-line-exit)
	(setf (body frag) `(,-X- ,@(body frag)))))))

(cl:defun convert-to-enumerator (ret off-line-exit)
  (cl:let ((frag (fr ret)))
    (make-ports-off-line frag off-line-exit)
    (cl:let* ((tail (member (off-line-spot ret) (body frag)))
		(head (ldiff (body frag) tail))
		(flag (new-var 'flg))
		(E (new-var 'e)))
      (setf (off-line-spot ret) nil)
      (push (list flag '(member T nil)) (aux frag))
      (push `(setq ,flag nil) (prolog frag))
      (setf (body frag)
	    `((when (null ,flag) (setq ,flag T) (go ,E))
	      ,@(cdr tail)
	      ,E ,@ head)))
    frag))

(cl:defun convert-to-reducer (arg)
  (cl:let ((frag (fr arg)))
    (make-outputs-off-line frag)
    (cl:let* ((tail (member (off-line-spot arg) (body frag)))
		(head (ldiff (body frag) tail))
		(flag (new-var 'fl))
		(M (new-var 'm))
		(N (new-var 'n)))
      (push (list flag '(member T nil)) (aux frag))
      (push `(setq ,flag nil) (prolog frag))
      (setf (body frag)
	    `((if (null ,flag) (go ,M))
	  ,N ,@(cdr tail)
	  ,M ,@ head
	      (when (null ,flag) (setq ,flag T) (go ,N)))))
    frag))  ) ;end of eval-when

;                        TURNING A FRAG INTO CODE

;this takes a non-series frag and makes it into a garden variety chunk of code.
;It assumes that it will never be called on a frag with any inputs.

(eval-when (eval load compile)

(cl:defun codify (frag)
  (dolist (r (rets frag))
    (if (series-var-p r) (rrs 10 "~%Series value returned by~%" (code frag))))
  (maybe-de-series frag)
  (cl:let ((rets (mapcan #'(lambda (r)
			       (if (not (free-out r)) (list (var r))))
			   (rets frag)))
	     (aux (aux frag))
	     (code (prolog frag)))
    (when (wrappers frag)
      (if (cdr code) (setq code (cons 'progn code)) (setq code (car code)))
      (dolist (wrp (wrappers frag))
	(setq code (cl:funcall (eval wrp) code)))
      (setq code (list code)))
    (cl:let ((last-form (car (last code))))
      (if (and rets (null (cdr rets)))
	  (cond ((and (eq-car last-form 'setq)
		      (eq-car (cdr last-form) (car rets)))
		 (setq code (delete last-form code))
		 (when (not (contains-p (car rets) code))
		   (setq aux (delete (car rets) aux :key #'car)))
		 (setq rets (caddr last-form)))
		(T (setq rets (car rets))))
	  (setq rets `(values ,@ rets))))
    (setq code (nconc code (list rets)))
    (setq code (codify-1 aux code))
    (use-user-names aux code)
    (setf (cadr code) (sort (cadr code) #'aux-ordering))
    (setq *last-series-loop* code)))

(cl:defun codify-1 (aux code)
  (multiple-value-setq (aux code) (clean-code aux code))
  (when aux
    (cl:let ((dcls (clean-dcls aux)))
      (if dcls (push `(declare ,@ dcls) code))))
 `(cl:let ,(mapcar #'aux-init aux) ,@ code))

(cl:defun aux-ordering (a b)
  (when (consp a) (setq a (car a)))
  (when (consp b) (setq b (car b)))
  (string-lessp (string a) (string b)))

;this tries to get a correct init in situations where NIL won't do.
;it assumes that like maclisp, all that could really matter is whether
;something is a fixnum, or float.

;; This function converts a type to a "canonical" type.  Mainly meant
;; to handle things that have been deftype'd.  We want to convert that
;; deftype'd thing to the underlying Lisp type.  
#+cmu
(cl:defun canonical-type (type)
  (kernel:type-specifier (c::specifier-type (if (and (not (atom type))
						     (eq 'quote (first type)))
						(cdr type)
						type))))
#+CLISP
(cl:defun canonical-type (type)
  (lisp:type-expand type))

#-(or cmu CLISP)
(cl:defun canonical-type (type)
  type)

;; toy@rtp.ericsson.se:
;; Actually, to be correct, we need to be more careful about how we
;; init things because CLtL2 says it's wrong and CMU Lisp complains
;; and fails if we don't init things correctly.  In particular, we
;; need to handle the case of arrays, strings, and "(member t)" that
;; is used in a few places.  I think all cases that occur in the test
;; suite are handled here.


(cl:defun aux-init (aux)
  (cl:flet ((eq-or-eq-car (thing item)
	      (or (eq thing item)
		  (eq-car thing item))))
    (cl:let ((var-name (car aux))
	     (var-type (canonical-type (cadr aux))))
      ;; (format t "var-name, var-type = ~a ~a~%" var-name var-type)
      (cond ((subtypep var-type 'complex)
	     ;; (complex) or (complex float-type)
	     (cond ((atom var-type)
		    ;; Plain old complex.  (Don't want #C(0 0) because
		    ;; complex canonicalization converts that to plain
		    ;; 0.)
		    (list var-name #C(0.0f0 0.0f0)))
		   (t
		    ;; Create a complex zero with the correct component type.
		    (list var-name (complex (coerce 0 (cadadr aux)))))))
	    ((subtypep var-type 'number)
	     ;; Initialize NUMBER's to 0 of the appropriate type.
	     (list var-name (coerce 0 var-type)))
	    ;; Although a STRING can be (VECTOR CHARACTER) and a
	    ;; SIMPLE-STRING can be a (SIMPLE-ARRAY CHARACTER (*)), we
	    ;; handle them here because the syntax of the declarations
	    ;; is different.
	    ((or (eq-or-eq-car var-type 'string)
		 (eq-or-eq-car var-type 'simple-string)
		 (eq-or-eq-car var-type 'base-string)
		 (eq-or-eq-car var-type 'simple-base-string))
	     ;; Handle (string) or (string len)
	     ;; (format t "string = ~A~%" var-type)
	     (cl:let ((len (if (and (consp var-type)
				    (= 2 (length var-type)))
			       (second var-type)
			       0)))
	       (list var-name (list 'make-sequence
				    `',var-type
				    (if (eq len '*) 0 len)))))
	    ;; Although a BIT-VECTOR could be handled via the VECTOR
	    ;; entry below, we do it here because the syntax of a
	    ;; BIT-VECTOR declaration is different from VECTOR.  The
	    ;; same holds for SIMPLE-BIT-VECTOR.
	    ((or (eq-or-eq-car var-type 'bit-vector)
		 (eq-or-eq-car var-type 'simple-bit-vector))
	     ;; (bit-vector) or (bit-vector len) or (simple-bit-vector)
	     ;; or (simple-bit-vector len)
	     ;; (format t "got a bit-vector:  ~A~%" var-type)
	     (cl:let ((len (if (and (consp var-type)
				    (= 2 (length var-type)))
			       (second var-type)
			       0)))
	       (list var-name (list 'make-sequence
				    `',var-type
				    (if (eq len '*) 0 len)))))
	    ((subtypep var-type 'simple-array)
	     ;; (simple-array) or (simple-array el-type) or
	     ;; (simple-array el-type dim)
	     (cl:let ((len (if (and (consp var-type)
				    (= 3 (length var-type)))
			       (first (third var-type))
			       0)))
	       (list var-name (list 'make-sequence
				    `',var-type
				    (if (eq len '*) 0 len)))))
	    ((subtypep var-type 'vector)
	     ;; (vector) or (vector el-type) or (vector el-type len)
	     (cl:let ((len (if (and (consp var-type)
				    (= 3 (length var-type)))
			       (third var-type)
			       0)))
	       (list var-name (list 'make-sequence
				    `',var-type
				    (if (eq len '*) 0 len)))))
	    ((subtypep var-type 'cons)
	     (list var-name '(cons nil nil)))
	    (T var-name)))))

(cl:defun clean-dcls (aux)
  (dolist (v aux) (propagate-types (cdr v) aux))
  (mapcar #'(lambda (v)
	      ;; Sometimes the desired type is quoted.  Remove the
	      ;; quote.  (Is this right?)
	      (if (and (listp (cadr v))
		       (eq 'quote (caadr v)))
		  `(type ,(cadadr v) ,(car v))
		  `(type ,(cadr v) ,(car v))))
	  (remove-if #'(lambda (v) (eq (cadr v) T)) aux)))

(cl:defun propagate-types (expr aux &optional (input-info nil))
  (do ((tt expr (cdr tt)))
      ((not (consp tt)) nil)
    (do () ((not (eq-car (car tt) 'series-element-type)))
      (when (cdr (assoc (cadar tt) input-info))
	(setf (car tt) (cdr (assoc (cadar tt) input-info)))
	(return nil))
      (setf (car tt) (cond ((cadr (assoc (cadar tt) aux))) (T T))))
    (when (consp (car tt)) (propagate-types (car tt) aux))))

(cl:defun use-user-names (aux loop)
  (cl:let ((alist nil))
    (dolist (v-info aux)
      (cl:let* ((v (car v-info))
		  (u (cdr (assoc v *user-names*))))
        (if (and u (not (contains-p u loop)) (not (rassoc u alist)))
          (push (cons v u) alist))))
    (if alist (nsublis alist loop))))

;This takes a series frag all of whose inputs and outputs are non-series
;things and makes it into a non-series frag.

(cl:defun maybe-de-series (frag)
  (when (and (non-series-p frag) (or (body frag) (epilog frag)))
    (when (not (active-terminator-p frag))
      (wrs 29 nil "~%Non-terminating series expression:~%" (code frag)))
    (cl:let* ((lab (new-var 'll))
		(loop `(tagbody ,lab ,@(body frag) (go ,lab) ,END)))
      (setf (prolog frag) (append (prolog frag) (list loop) (epilog frag)))
      (setf (body frag) nil)
      (setf (epilog frag) nil)
      (clean-labs frag (cdr loop))))
  frag)

;This gets rid of duplicate labs in a row.

(cl:defun clean-labs (frag stmtns)
  (cl:let ((alist nil))
    (do ((l stmtns (cdr l))) ((not (consp (cdr l))))
      L (when (and (car l) (symbolp (car l))
		   (cadr l) (symbolp (cadr l)))
	  (push (cons (pop (cdr l)) (car l)) alist)
	  (go L)))
    (nsublis alist frag)))

(cl:defun clean-code (aux code)
  (cl:let* ((suspicious (not-contained-twice (mapcar #'car aux) code))
	      (dead-aux (clean-code1 suspicious code)))
    (values (remove-if #'(lambda (v) (member (car v) dead-aux)) aux) code)))

(cl:defun not-contained-twice (items thing)
  (cl:let ((found-once nil) (found-twice nil))
    (labels ((look-at (tree)
	       (cond ((symbolp tree)
		      (cl:let ((found (car (member tree items))))
			(when found
			  (if (member found found-once)
			      (pushnew found found-twice)
			      (push found found-once)))))
		     (T (do ((tt tree (cdr tt)))
			    ((not (consp tt)) nil)
			  (look-at (car tt)))))))
      (look-at thing))
    (set-difference items found-twice)))

(cl:defun clean-code1 (suspicious code)
  (cl:let ((dead nil))
    (labels ((clean-code2 (prev-parent parent code &aux var)
	       (tagbody
		 R (when (setq var (car (member (setq-p code) suspicious)))
		     (push var dead)
		     (rplaca parent (setq code (caddr code)))
		     (when (or (symbolp code) (constantp code))
		       (cond ((consp (cdr parent))
			      (rplaca parent (cadr parent))
			      (rplacd parent (cddr parent))
			      (setq code (car parent))
			      (go R)) ;do would skip the next element
			     (prev-parent (pop (cdr prev-parent)))))))
	       (when (consp code)
		 (clean-code2 nil code (car code))
		 (do ((tt code (cdr tt)))
		     ((not (and (consp tt) (consp (cdr tt)))) nil)
		   (clean-code2 tt (cdr tt) (cadr tt))))))
      (clean-code2 nil nil code) ;depends on code not being setq at top.
      dead)))

(cl:defun setq-p (thing)
  (if (and (eq-car thing 'setq) (= (length thing) 3)) (cadr thing)))
); end of eval-when


;                          ---- GATHERERS ----

;The following functions support gatherers. No optimization ever applies to
;gatherers except in PRODUCING and GATHERING.  A gatherer is a function of
;two arguments.  If the second argument is NIL, the first argument is added
;into the accumulator of the gatherer.  If the second argument is not NIL, the
;accumulated result is returned.  It is an error to call the gatherer again
;after the accumulated result has been returned.

(cl:defun next-out (gatherer item)
  (cl:funcall gatherer item nil)
  nil)

(cl:defun result-of (gatherer)
  (cl:funcall gatherer nil t))

(defmacro gatherer (collector &environment *env*)
  (when (not (eq-car collector 'function))
    (cl:let ((x (new-var 'gather)))
      (setq collector
	    `#'(lambda (,x)
		 (cl:funcall ,collector (cl:funcall #'scan (collect ,x)))))))
  (cl:let ((frag (frag-for-collector (cadr collector) *env*)))
    (when (wrappers frag)
      (cl:let ((x (new-var 'gather)))
	(setq frag (frag-for-collector
		     `(lambda (,x) (funcall ,collector (scan (collect ,x))))
		     *env*))))
    (gathererify frag)))

(cl:defun gather-sanitize (collector)
  (cl:let ((x (new-var 'gather)))
    `#'(lambda (,x) (funcall ,collector (scan (collect ,x))))))

(defmacro gathering (var-collector-pairs &environment *env* &body body)
  (cl:let* ((frags (mapcar #'(lambda (p) (frag-for-collector (cadr p) *env*))
			     var-collector-pairs))
	      (wrappers (mapcan #'(lambda (f)
				    (prog1 (wrappers f) (setf (wrappers f) nil)))
				frags))
	      (stuff (mapcar #'gathererify frags))
	      (fns (mapcar #'(lambda (p s) (list (car p) (nth 4 s)))
			   var-collector-pairs stuff))
	      (returns (mapcar #'(lambda (p) `(result-of ,(car p)))
			       var-collector-pairs)))
    (setq returns (if (= (length returns) 1) (car returns) `(values ,@ returns)))
    (setq body `(cl:let ,fns ,@ body ,returns))
    (dolist (s (reverse stuff))
      (setq body (list 'cl:let (nth 1 s) (nth 2 s) (nth 3 s) body)))
    (dolist (wrp wrappers)
      (setq body (cl:funcall (eval wrp) body)))
    body))

(cl:defun frag-for-collector (collector *env*)
  (cl:let ((frag
	       (top-starting-series-expr collector
		 (progn
		   (when (not (eq-car collector 'lambda))
		     (cl:let ((x (new-var 'gatherer)))
		       (setq collector `(lambda (,x) (,collector ,x)))))
		   (cl:multiple-value-bind (forms type-alist ignore-vars outs)
		       (decode-dcls (cddr collector) '(types ignores opts))
		     (cl:let* ((series-vars
				   (mapcar #'car
					   (remove-if-not
					     #'(lambda (e)
						 (or (eq (cdr e) 'series)
						     (eq-car (cdr e) 'series)))
					     type-alist))))
		       (preprocess-body (cadr collector) series-vars
					type-alist ignore-vars forms outs))))
					 nil)))
    (when (not (and (frag-p frag)
                    (= 1 (length (args frag)))
                    (series-var-p (car (args frag)))
                    (= 1 (length (rets frag)))
                    (not (series-var-p (car (rets frag))))))
      (ers 61 "~%Input to GATHERER fails to be one-input one-output collector."))
    frag))

;this assumes the frag is a one-in one-out collector and
;that if there are wrappers, they are only relevant to the epilog.

(cl:defun gathererify (frag)
  (when (off-line-spot (car (args frag)))
    (convert-to-reducer (car (args frag))))
  (cl:let ((code `(tagbody ,@(body frag)))
	     (ecode `(progn ,@(epilog frag))))
    (dolist (wrp (wrappers frag))
      (setq ecode (cl:funcall (eval wrp) ecode)))
    (codify-1 (aux frag)
	      `(,@(prolog frag)
		#'(lambda (,(var (car (args frag))) result-p)
		    (cond ((null result-p) ,code)
			  (T ,ecode ,(var (car (rets frag))))))))))

;                  ---- SERIES FUNCTION LIBRARY ----

;The body runs when optimization is not happening.
;The optimizer runs when optimization is happening.
;The trigger says whether or not a series expression is beginning.
;  it forces NAME to be a macro instead of a function.
;The discriminator says whether or not series are being returned.

(defmacro defS (name arglist doc body &key optimizer trigger discriminator)
  (cl:let* ((body-code body)
	      (dcl (if (consp doc) (prog1 (cdr doc) (setq doc (car doc)))))
	      (opt-code (or optimizer body))
	      (body-fn (cond ((symbolp body-code) body-code)
			     (trigger (gentemp (string name)))))
	      (opt-fn (gentemp (string name)))
	      (desc-fn (cond (discriminator (gentemp (string name)))
			     (trigger 'no)
			     (t 'yes)))
	      (opt-arglist	;makes up for extra level of evaluation.
		(mapcar #'(lambda (a)
			    (if (and (listp a) (listp (cdr a)))
				(list* (car a) `(copy-tree ',(cadr a)) (cddr a))
				a))
			arglist)))
    `(eval-when (eval load compile)
       ,@(if trigger ;This must be first---it's a macro and body can refer to it
	     `((defmacro ,name (&whole call &rest stuff &environment *env*)
		 #+symbolics (declare (zl:arglist ,@(copy-list arglist)))
		 ,@(if doc (list doc))
		 (if (and *optimize-series-expressions* ,trigger)
		     (process-top call)
		     (cons ',body-fn stuff)))))
       ,@(cond ((and (not trigger) (symbolp body-code))
		`((defmacro ,name (&rest stuff) (cons ',body-code  stuff))))
	       ((not (symbolp body-code))
		`((cl:defun ,(if trigger body-fn name) ,arglist
		    ,@(if doc (list doc))
		    ,@(if dcl (list dcl))
		    (compiler-let ((*optimize-series-expressions* nil)) ,body-code)))))
       ,@(if discriminator
	     `((cl:defun ,desc-fn (call) ,discriminator)))
       (setf (get ',name 'returns-series) (function ,desc-fn))
       (cl:defun ,opt-fn ,opt-arglist
	 ,@(if dcl (list dcl))
	 (compiler-let ((*optimize-series-expressions* T)) ,opt-code))
       (setf (get ',name 'series-optimizer) (function ,opt-fn))
       ',name)))

(eval-when (eval load compile)

(cl:defun eq-car (thing item)
  (and (consp thing) (eq (car thing) item)))

(cl:defun contains-p (item thing)
  (do ((tt thing (cdr tt)))
      ((not (consp tt)) (eq tt item))
    (if (contains-p item (car tt)) (return T))))

(cl:defun contains-any (items thing)
  (do ((tt thing (cdr tt)))
      ((not (consp tt)) (member tt items))
    (if (contains-any items (car tt)) (return T))))
) ;end of eval-when

(cl:defun process-top (call)
  (when (and *series-expression-cache*
	     (not (hash-table-p *series-expression-cache*)))
    (setq *series-expression-cache* (make-hash-table :test #'eq)))
  (cl:let ((cached-value (and *series-expression-cache*
                           (gethash call *series-expression-cache*))))
    (cond (cached-value)
	  (T (setq cached-value
		   (top-starting-series-expr call
		      (codify (mergify (graphify call)))
		      `(compiler-let ((*optimize-series-expressions* nil)) ,call)))
	     (when *series-expression-cache*
	       (setf (gethash call *series-expression-cache*) cached-value))
	     cached-value))))

;this forms are useful for making code that comes out one way in the
;body and another way in the optimizer

(defmacro opt-non-opt (f1 f2)
  (if *optimize-series-expressions* f1 f2))

(defmacro non-optq (x) `(opt-non-opt ,x (list 'quote ,x)))

(defmacro optq (x) `(opt-non-opt ',x ,x))

(defmacro fragL (&rest stuff)
  #+symbolics (declare (scl:arglist args rets aux alt prolog body epilog wraprs))
  (if *optimize-series-expressions*
    `(funcall-literal-frag
      (list ,(if (not (contains-p '*type* stuff))
               `',stuff
               `(subst *type* '*type* ',stuff))
            ,@(mapcar #'car (car stuff))))
    (cl:let ((literal-frag
		 (list* (car stuff)
			(cadr stuff)
			(mapcar #'(lambda (data)
				    (if (or (eq (cadr data) '*type*)
					    (eq-car (cadr data)
						    'series-element-type))
					(list (car data) T)
					data))
				(caddr stuff))
			(cdddr stuff))))
      (frag->physical (literal-frag literal-frag)
                      (mapcar #'car (car stuff))))))

(cl:defun funcall-literal-frag (frag-and-values)
  (funcall-frag (literal-frag (car frag-and-values)) (cdr frag-and-values)))

;the next few things are optimizers that hang on standard symbols.

(cl:defun setq-opt (var exp)
  (my-multi-setq (list var) exp `(setq ,var ,exp)))

(setf (get 'setq 'series-optimizer) #'setq-opt)
(setf (get 'setq 'returns-series) #'no) ;here should be better than this

(cl:defun multiple-value-setq-opt (vars exp)
  (my-multi-setq vars exp `(multiple-value-setq ,vars ,exp)))

(setf (get 'multiple-value-setq 'series-optimizer) #'multiple-value-setq-opt)
(setf (get 'multiple-value-setq 'returns-series) #'no)  ;here should be better than this

;Note the cludging we have to do when the first var is a let-series var.
;This is necessary in case this value is going to have to be a return value as well.
;We really should have done something better about specifing free variable outputs
;so that this mess would not be necessary.

(cl:defun my-multi-setq (vars value form)
  (cl:let* ((type (if (null (cdr vars)) t
			`(values ,@(make-list (length vars) :initial-element T))))
	      (frag (fragify value type)))
    (dolist (out (rets frag))
      (cl:let* ((v (pop vars))
		  (entry (assoc v *renames*)))
        (cond (entry
               (rplacd entry out)
               (setf (free-out out) v)
               (when (eq out (car (rets frag)))
                 (cl:let* ((v (new-var 'copy))
			     (ret (make-sym :var v :series-var-p (series-var-p out)))
			     (assignment `((setq ,v ,(var out)))))
                   (setf (fr ret) frag)
                   (push ret (rets frag))
                   (push (list v T) (aux frag))
                   (cond ((off-line-spot out)
                          (setf (off-line-spot ret) (new-var '-C-))
                          (setf (body frag)
                                (nsubst-inline `(,@ assignment
                                                    ,(off-line-spot ret)
                                                    ,(off-line-spot out))
                                               (off-line-spot out) (body frag))))
                         ((series-var-p out)
                          (setf (body frag) (append (body frag) assignment)))
                         ((or (body frag) (epilog frag))
                          (setf (epilog frag) (append (epilog frag) assignment)))
                         (T (setf (prolog frag)
                                  (append (prolog frag) assignment)))))))
              ((series-var-p out)
               (rrs 11 "~%series value assigned to free variable~%" form))
              (T (if (or (body frag) (epilog frag))
                   (setf (epilog frag)
                         (append (epilog frag) `((setq ,v ,(var out)))))
                   (setf (prolog frag)
                         (append (prolog frag) `((setq ,v ,(var out))))))
                 (when (not (eq out (car (rets frag))))
                   (kill-ret out))))))
    frag))

(defS funcall (function &rest expr-list) "" cl:funcall
 :optimizer
  (cond ((and (eq-car function 'function) (symbolp (cadr function))
	      (get (cadr function) 'series-optimizer))
	 (cons (cadr function) expr-list))
	((not (simple-quoted-lambda function))
	 (list* 'cl:funcall function expr-list))
	((not (= (length expr-list) (length (cadr (cadr function)))))
	 (ers 67 "~%Wrong number of args to funcall:~%" (cons function expr-list)))
	(T `(let ,(mapcar #'list (cadr (cadr function)) expr-list)
	      ,@(cddr (cadr function)))))
 :trigger
  (cl:let* ((function (my-macroexpand (cadr call)))
	      (expr-list (cddr call)))
    (or (and (eq-car function 'function) (symbolp (cadr function))
	     (get (cadr function) 'series-optimizer))
	(and (simple-quoted-lambda function)
	     (some #'produces-optimizable-series expr-list))))
 :discriminator
  (cl:let* ((function (my-macroexpand (cadr call))))
    (or (and (eq-car function 'function) (symbolp (cadr function))
	     (get (cadr function) 'series-optimizer))
	(and (simple-quoted-lambda function)
	     (produces-optimizable-series (car (last (cddr (cadr function)))))))))

(cl:defun produces-optimizable-series (original-code)
  (cl:let ((flag T) pred (code original-code))
    (loop
      (if (not (and flag (consp code) (symbolp (car code)))) (return nil))
      (if (eq (car code) 'values)
        (return (some #'produces-optimizable-series (cdr code))))
      (if (eq (car code) 'the)
        (return (produces-optimizable-series (caddr code))))
      (setq pred (get (car code) 'returns-series))
      (if pred (return (cl:funcall pred code)))
      (if (not-expr-like-special-form-p (car code)) (return nil))
      (if (not (macro-function (car code))) 
        (return (some #'produces-optimizable-series (cdr code))))
      (if (eq code original-code) (setq code (iterative-copy-tree code)))
      (multiple-value-setq (code flag) (macroexpand-1 code *env*)))))

(defS multiple-value-bind (vars values &rest body) "" cl:multiple-value-bind
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars)
      (decode-dcls body '(types ignores))
    (cl:let* ((bindings (process-let-series-pair (list vars values)
							type-alist T))
		(*renames* (revappend bindings *renames*)))
      (process-let-series-body ignore-vars forms bindings)))
 :trigger (produces-optimizable-series (caddr call))
 :discriminator (produces-optimizable-series (car (last call))))

#+symbolics(setf (gethash 'multiple-value-bind
			  zwei:*lisp-indentation-offset-hash-table*)
		 '(1 3 2 1))

(setf (get 'cl:multiple-value-bind 'series-optimizer)
      (get 'multiple-value-bind 'series-optimizer))
(setf (get 'cl:multiple-value-bind 'returns-series)
      (get 'multiple-value-bind 'returns-series))

(defS let (pairs &rest body) "" cl:let
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars)
      (decode-dcls body '(types ignores))
    (cl:let* ((bindings (mapcan #'(lambda (p)
				      (process-let-series-pair p type-alist nil))
				  pairs))
		(*renames* (revappend bindings *renames*)))
      (process-let-series-body ignore-vars forms bindings)))
 :trigger
  (dolist (pair (cadr call) nil)
    (if (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
	(return T)))
 :discriminator (produces-optimizable-series (car (last call))))

#+symbolics(setf (gethash 'let zwei:*lisp-indentation-offset-hash-table*)
		 '(1 1))

(setf (get 'cl:let 'series-optimizer) (get 'let 'series-optimizer))
(setf (get 'cl:let 'returns-series) (get 'let 'returns-series))

(defS let* (pairs &rest body) "" cl:let*
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars)
      (decode-dcls body '(types ignores))
    (cl:let* ((old-top *renames*)
		(*renames* *renames*))
      (dolist (p pairs)
	(setq *renames*
	      (nconc (process-let-series-pair p type-alist nil) *renames*)))
      (process-let-series-body ignore-vars forms (ldiff *renames* old-top))))
 :trigger
  (dolist (pair (cadr call) nil)
    (if (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
	(return T)))
 :discriminator (produces-optimizable-series (car (last call))))

#+symbolics(setf (gethash 'let* zwei:*lisp-indentation-offset-hash-table*)
		 '(1 1))

(setf (get 'cl:let* 'series-optimizer) (get 'let* 'series-optimizer))
(setf (get 'cl:let* 'returns-series) (get 'let* 'returns-series))

(cl:defun process-let-series-pair (p type-alist allow-multiple-vars)
  (setq p (normalize-pair p allow-multiple-vars))
  (cl:let* ((vars (car p))
	      (types (mapcar #'(lambda (v) (or (cdr (assoc v type-alist)) T))
			     vars))
	      (rets
		(if (= (length vars) 1)
		    (list (retify (cadr p) (car types)))
		    (rets (fragify (cadr p) `(values ,@ types))))))
    (mapcar #'(lambda (v r)
                (push (cons (var r) v) *user-names*)
                (setf (free-out r) v)
                (cons v r))
            vars rets)))

(cl:defun normalize-pair (p allow-multiple-vars)
  (cond ((variable-p p) (list (list p) nil))
	((and (consp p) (variable-p (car p)) (= (length p) 2))
	 (list (list (car p)) (cadr p)))
	((and (consp p) (variable-p (car p)) (= (length p) 1))
	 (list (list (car p)) nil))
	((and allow-multiple-vars (consp p) (consp (car p))
	      (every #'variable-p (car p))
	      (= (length p) 2)) p)
	(T (ers 66 "~%Malformed binding pair " p "."))))

(cl:defun variable-p (thing)
  (and thing (symbolp thing) (not (eq thing T)) (not (keywordp thing))))

(cl:defun process-let-series-body (ignore-vars forms alist)
  (cl:let* ((initial-alist (mapcar #'(lambda (e) (cons (car e) (cdr e))) alist))
	      (frag (process-let-forms forms)))
    (mapc #'(lambda (old new)
	      (cond ((and (eq (cdr old) (cdr new))   ;not setqed.
			  (null (nxts (cdr new))))   ;current value not used.
		     (if (not (member (car old) ignore-vars))
			 nil #| ;HERE can get false positives.
			 (wrs 52 t "~%The variable "
			      (car old) " is unused in:~%" *call*)|#))
		    ((member (car old) ignore-vars)
		     (wrs 53 t "~%The variable " (car old)
			  " is declared IGNORE and yet used in:~%" *call*))))
	  initial-alist alist)
    frag))

(cl:defun process-let-forms (forms)
  (mapc #'(lambda (f) (fragify f '(values))) (butlast forms))
  (fragify (car (last forms)) '*)) ;forces NIL if no forms.

;Next we have the definitions of the basic higher order functions.

(eval-when (eval load compile) (proclaim '(special *state*)))

(defS map-fn (type function &rest args)
    "Maps FUNCTION over the input series."
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1)
	   (fragL ((function) (args)) ((items T))
		  ((items T) (list-of-generators list)) ()
		  ((setq list-of-generators
			 (mapcar #'(lambda (s) (generator s)) args)))
		  ((setq items (apply function (list-of-next #'(lambda () (go end))
					      list-of-generators)))) () ()))
	  (T (values-lists n (apply #'map-fn T
				    #'(lambda (&rest vals)
					(multiple-value-list
					  (apply function vals)))
				    args)))))
 :optimizer
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	      (params nil)
	      (frag (make-frag))
	      (in-vars (n-gensyms (length args) "M-"))
	      (out-vars (n-gensyms (length types) "ITEMS-"))
	      (*state* nil))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p T) frag))
    (setf (aux frag) (mapcar #'list out-vars types))
    (multiple-value-setq (function params)
      (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p T) frag))
    (setf (body frag) (handle-fn-call frag out-vars function in-vars t))
    (funcall-frag frag params)))

(cl:defun list-of-next (at-end list-of-generators)
 (mapcar #'(lambda (g) (do-next-in g at-end)) list-of-generators))

(defS encapsulated (encapsulating-fn scanner-or-collector)
    "Specifies an encapsulating form to be used with a scanner or collector."
  encapsulated-macro
 :optimizer
  (progn
    (when (not (eq-car encapsulating-fn 'function))
      (ers 68 "~%First ENCAPSULATING arg " encapsulating-fn
	   " is not quoted function."))
    (cond ((and (or (eq-car scanner-or-collector 'scan-fn)
		    (eq-car scanner-or-collector 'scan-fn-inclusive))
		(= (length scanner-or-collector) 5))
	    (apply #'scan-fn-opt encapsulating-fn
		  (eq-car scanner-or-collector 'scan-fn-inclusive)
		  (cdr scanner-or-collector)))
	  ((eq-car scanner-or-collector 'collect-fn)
	   (apply #'collect-fn-opt encapsulating-fn (cdr scanner-or-collector)))
	  (T (ers 69 "~%Malformed second arg to ENCAPSULATING arg "
		  scanner-or-collector "."))))

 :trigger T
 :discriminator (or (eq-car (caddr call) 'scan-fn)
		    (eq-car (caddr call) 'scan-fn-inclusive)))

(defmacro encapsulated-macro (encapsulating-fn scanner-or-collector)
  (when (not (eq-car encapsulating-fn 'function))
    (ers 68 "~%First ENCAPSULATING arg " encapsulating-fn
         " is not quoted function."))
  (cond ((and (or (eq-car scanner-or-collector 'scan-fn)
                  (eq-car scanner-or-collector 'scan-fn-inclusive))
              (= (length scanner-or-collector) 5))
         (cl:let ((body `(basic-collect-list
			     (scan-multi-out->scan-list-out
			       #',(car scanner-or-collector)
			       ,@(cdr scanner-or-collector)))))
           `(cl:let ((data ,(cl:funcall (eval encapsulating-fn) body)))
              (values-lists (length (car data)) (scan data)))))
        ((eq-car scanner-or-collector 'collect-fn)
         (cl:funcall (eval encapsulating-fn) scanner-or-collector))
        (T (ers 69 "~%Malformed second arg to ENCAPSULATING arg "
                scanner-or-collector "."))))

(cl:defun scan-multi-out->scan-list-out (fn type init step test)
  (compiler-let ((*optimize-series-expressions* nil))
    (cl:let ((n (length (decode-type-arg type))))
      (flet ((new-init () (forceL n (multiple-value-list (cl:funcall init))))
	     (new-step (state) (forceL n (multiple-value-list (apply step state))))
	     (new-test (state) (apply test state)))
	(cl:funcall fn T #'new-init #'new-step #'new-test)))))

;needed because collect is a macro
(cl:defun basic-collect-list (items)
  (compiler-let ((*optimize-series-expressions* nil))
    (fragL ((items T)) ((result)) ((result list)) ()
	   ((setq result nil))
	   ((setq result (cons items result)))
	   ((setq result (nreverse result))) ())))

(defS collect-fn (type inits function &rest args)
   "Computes a cumulative value by applying FUNCTION to the elements of ITEMS."
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1) (apply #'basic-collect-fn inits function args))
	  (T (values-list
	       (apply #'basic-collect-fn
		      #'(lambda ()
			  (forceL n (multiple-value-list (cl:funcall inits))))
		      #'(lambda (state &rest args)
			  (forceL n (multiple-value-list
				      (apply function (nconc state args)))))
		      args)))))
 :optimizer
  (apply #'collect-fn-opt nil type inits function args)
 :trigger T)

(cl:defun collect-fn-opt (wrap-fn type inits function &rest args)
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	      (params nil)
	      (frag (make-frag))
	      (in-vars (n-gensyms (length args) "ITEMS-"))
	      (out-vars (n-gensyms (length types) "C-"))
	      (*state* nil))
    (if wrap-fn (push wrap-fn (wrappers frag)))
    (dolist (var out-vars)
      (+ret (make-sym :var var) frag))
    (setf (aux frag) (mapcar #'list out-vars types))
    (multiple-value-setq (inits params) (handle-fn-arg frag inits params))
    (multiple-value-setq (function params) (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p T)
	    frag)) ;must be before other possible args
    (setf (prolog frag) (handle-fn-call frag out-vars inits nil))
    (setf (body frag)
	  (handle-fn-call frag out-vars function (append out-vars in-vars) t))
    (funcall-frag frag params)))

;needed because collect-fn is macro
(cl:defun basic-collect-fn (inits function &rest args)
  (compiler-let ((*optimize-series-expressions* nil))
    (fragL ((inits) (function) (args)) ((result))
           ((result t) (list-of-generators list)) ()
           ((setq result (cl:funcall inits))
            (setq list-of-generators (mapcar #'(lambda (s) (generator s)) args)))
           ((cl:let ((vals (list-of-next #'(lambda () (go end))
					   list-of-generators)))
              (setq result (apply function result vals)))) () ())))

;hint to users: to avoid inits, add an extra init that acts like a flag.

(defS collecting-fn (type inits function &rest args)
  "Computes cumulative values by applying FUNCTION to the elements of ITEMS."
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1)
           (fragL ((inits) (function) (args)) ((result T))
                  ((result T) (list-of-generators list)) ()
                  ((setq result (cl:funcall inits))
                   (setq list-of-generators
                         (mapcar #'(lambda (s) (generator s)) args)))
                  ((cl:let ((vals (list-of-next #'(lambda () (go end))
						  list-of-generators)))
                     (setq result (apply function result vals)))) () ()))
          (T (values-lists n
                           (apply #'collecting-fn T
                                  #'(lambda ()
                                      (forceL n (multiple-value-list (cl:funcall inits))))
                                  #'(lambda (state &rest args)
                                      (forceL n (multiple-value-list
                                                 (apply function (append state args)))))
                                  args)))))
  :optimizer
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	      (params nil)
	      (frag (make-frag))
	      (in-vars (n-gensyms (length args) "ITEMS-"))
	      (out-vars (n-gensyms (length types) "C-"))
	      (*state* nil))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p T) frag))
    (setf (aux frag) (mapcar #'list out-vars types))
    (multiple-value-setq (inits params) (handle-fn-arg frag inits params))
    (multiple-value-setq (function params) (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p T) frag))
    (setf (prolog frag) (handle-fn-call frag out-vars inits nil))
    (setf (body frag)
          (handle-fn-call frag out-vars function (append out-vars in-vars) t))
    (funcall-frag frag params)))

(defS scan-fn (type init step &optional (test nil test-p))
  "Enumerates a series"
  (cl:let ((n (length (decode-type-arg type))))
    (if (not test-p) (setq test #'never))
    (cond ((= n 1)
           (fragL ((init) (step) (test)) ((prior-state T))
                  ((state T) (prior-state T)) ()
                  ((setq state (cl:funcall init)))
                  ((if (cl:funcall test state) (go END))
                   (prog1 (setq prior-state state)
                     (setq state (cl:funcall step state)))) () ()))
          (T (cl:let ((data (scan-multi-out->scan-list-out
				#'scan-fn type init step test)))
               (values-lists n data)))))
  :optimizer
  (if test-p (scan-fn-opt nil nil type init step test)
      (scan-fn-opt nil nil type init step)))

(defS scan-fn-inclusive (type init step test)
  "Enumerates a series"
  (cl:let ((n (length (decode-type-arg type))))
    (cond ((= n 1)
           (fragL ((init) (step) (test)) ((prior-state T))
                  ((state T) (prior-state T) (done T)) ()
                  ((setq state (cl:funcall init)) (setq done nil))
                  ((if done (go END))
                   (setq done (cl:funcall test state))
                   (prog1 (setq prior-state state)
                     (if (not done) (setq state (cl:funcall step state)))))
                  () ()))
          (T (cl:let ((data (scan-multi-out->scan-list-out
				#'scan-fn-inclusive type init step test)))
               (values-lists n data)))))
  :optimizer
  (scan-fn-opt nil T type init step test))

(cl:defun scan-fn-opt (wrap-fn inclusive-p type init step
				 &optional (test nil test-p))
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	      (params nil)
	      (frag (make-frag))
	      (state-vars (n-gensyms (length types) "STATE-"))
	      (out-vars (n-gensyms (length types) "ITEMS-"))
	      (*state* nil))
    (if wrap-fn (push wrap-fn (wrappers frag)))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p T) frag))
    (setf (aux frag) (append (mapcar #'list state-vars types)
			     (mapcar #'list out-vars types)))
    (multiple-value-setq (init params) (handle-fn-arg frag init params))
    (multiple-value-setq (step params) (handle-fn-arg frag step params))
    (if test-p (multiple-value-setq (test params)
		 (handle-fn-arg frag test params)))
    (setq params (mapcar #'retify params))
    (setf (prolog frag) (handle-fn-call frag state-vars init nil))
    (cl:let ((output-expr `(setq ,@(mapcan #'list out-vars state-vars)))
	       (step-code (car (handle-fn-call frag state-vars step state-vars))))
      (if (not inclusive-p)
	  (setf (body frag)
		`(,@(if test-p
			`((if ,(car (handle-fn-call frag nil test state-vars t))
			      (go ,END))))
		  ,output-expr ,step-code))
		(cl:let ((done (new-var 'd)))
	    (push (list done '(member T nil)) (aux frag))
	    (push `(setq ,done nil) (prolog frag))
	    (setf (body frag)
		  `((if ,done (go ,END))
		    ,(car (handle-fn-call frag (list done) test state-vars t))
		    ,output-expr
		    (if (not ,done) ,step-code))))))
    (funcall-frag frag params)))

;Helping functions

(cl:defun never (&rest stuff) (declare (ignore stuff)) nil)

(cl:defun forceL (n list)
  (if (= n (length list)) list
      (cl:let* ((new (make-list n :initial-element nil))
		  (ptr new))
	(do ((i (min n (length list)) (1- i))) ((zerop i))
	  (setf (car ptr) (pop list))
	  (pop ptr))
	new)))

(cl:defun must-be-quoted (type)
  (cond ((eq-car type 'quote) (cadr type))
	((member type '(t nil)) type)
	(T (rrs 2 "~%Non-quoted type " type "."))))

;If function is not a simple quoted function, then a non-series input is
;added to frag, and a parameter is added to params so that the function
;will get processed right.

(cl:defun handle-fn-arg (frag function params)
  (when (not (and (eq-car function 'function)
		  (or (symbolp (cadr function))
		      (and (eq-car (cadr function) 'lambda)
			   (every #'(lambda (a)
				      (and (symbolp a)
					   (or (zerop (length (string a)))
					       (not (char= (char (string a) 0)
							   #\&)))))
				  (cadr (cadr function)))))))
    (cl:let ((fn-var (new-var 'function)))
      (+arg (make-sym :var fn-var) frag)
      (setq params (nconc params (list function)))
      (setq function fn-var)))
  (values function params))

;This makes code for `(multiple-value-setq ,out-vars (funcall ,fn ,@ in-vars)).
;It always returns a list of a single statement.  Also,
;any free references to series::let vars are made
;non-series inputs of frag and hooked up to the right
;things.  (Note macro expansion has to be done in a
;nested context so that nested series expressions will be ok.)
;(Note also that this has to bypass what usually happens when macroexpanding
;function quoted things.  Things should be sructured differently so that this
;is not necessary.)

(cl:defun handle-fn-call (frag out-vars fn in-vars &optional (last? nil))
  (cl:let ((*in-series-expr* nil) (*not-straight-line-code* nil)
	     (*user-names* nil) (*renames* *renames*) (fn-quoted? nil))
    (when (eq-car fn 'function)
      (setq fn-quoted? t)
      (setq fn (cadr fn)))
    (cl:multiple-value-bind (fn free-ins free-outs setqed state)
	(handle-non-series-stuff fn *state*)
      (declare (ignore setqed))
      (setq *state* state)
      (when last?
        (dolist (entry free-ins)
          (cl:let ((arg (make-sym :var (car entry))))
            (+arg arg frag)
            (+dflow (cdr entry) arg)))
        (dolist (entry free-outs)
          (cl:let ((new (make-sym :var (car entry)))
		     (v (cdr entry)))
            (when (not (find (car entry) (args frag) :key #'var))
              (push (list (car entry) T) (aux frag)))
            (setf (free-out new) v)
            (+ret new frag)
            (rplacd (assoc v *renames*) new))))
      (setq fn (if (not fn-quoted?) `(cl:funcall ,fn) `(,fn)))
      (cond ((null out-vars) `((,@ fn ,@ in-vars)))
            ((= (length out-vars) 1)
             `((setq ,(car out-vars) (,@ fn ,@ in-vars))))
            (T `((multiple-value-setq ,out-vars (,@ fn ,@ in-vars))))))))

;various easy ways of doing mapping

;put on #M
(cl:defun abbreviated-map-fn-reader (stream subchar arg)
    (declare (ignore stream subchar))
  (case arg
    ((nil) '\#M) ;the macros actually do the work.
    (1 '\#1M)
    (2 '\#2M)
    (3 '\#3M)
    (4 '\#4M)
    (5 '\#5M)
    (otherwise
      (error "The numeric argument to #M must be between 1 and 5 inclusive"))))

; (set-dispatch-macro-character #\# #\M (function abbreviated-map-fn-reader))

(defmacro \#M (fn &rest args) (mapit T fn args))
(defmacro \#1M (fn &rest args) (mapit T fn args))
(defmacro \#2M (fn &rest args) (mapit '(values T T) fn args))
(defmacro \#3M (fn &rest args) (mapit '(values T T T) fn args))
(defmacro \#4M (fn &rest args) (mapit '(values T T T T) fn args))
(defmacro \#5M (fn &rest args) (mapit '(values T T T T T) fn args))

(cl:defun mapit (type fn args)
  (if (not (symbolp fn))
    `(map-fn ',type (function ,fn) ,@ args)
    (cl:let ((vars (do ((a args (cdr a))
			  (l nil (cons (gensym "V-") l)))
			 ((null a) (return l)))))
      `(map-fn ',type (function (lambda ,vars (,fn ,@ vars))) ,@ args))))

(defS iterate (var-value-list &rest body)
    "Applies BODY to each element of the series"
  iterate-mac
 :optimizer
  `(iterate-mac ,var-value-list ,@ body)
 :trigger T)

(defmacro iterate-mac (var-value-list &rest body)
    "Applies BODY to each element of the series"
  `(collect-ignore (mapping ,var-value-list ,@ body)))

(defS collect-ignore (items)
    "Reads input and returns NIL."
  (fragL ((items T)) ((item)) ((item null)) () ((setq item nil)) () () ())
 :trigger T)

#+symbolics(setf (gethash 'iterate zwei:*lisp-indentation-offset-hash-table*)
		 '(1 1))

(defS mapping (var-value-list &rest body)
    "Applies body to each element of the series"
  mapping-mac
 :optimizer
  (cl:let* ((bindings (mapcan #'(lambda (p) (process-let-series-pair p nil T))
				var-value-list))
	      (*renames* (revappend bindings *renames*)))
    (process-let-series-body nil
       `((map-fn T #'(lambda ,(mapcar #'car bindings) ,@ body)
	       ,@(mapcar #'car bindings)))
       bindings)))

#+symbolics(setf (gethash 'mapping zwei:*lisp-indentation-offset-hash-table*)
		 '(1 1))

;only used when optimization not possible.
(defmacro mapping-mac (var-value-list &body body)
  (setq var-value-list (mapcar #'(lambda (p) (normalize-pair p t)) var-value-list))
  (cond ((every #'(lambda (p) (null (cdar p))) var-value-list)
	 `(map-fn T
		  #'(lambda ,(mapcar #'caar var-value-list) ,@ body)
		  ,@(mapcar #'cadr var-value-list)))
	((null (cdr var-value-list))
	 `(multiple-value-bind ,@(car var-value-list)
	    (map-fn T #'(lambda ,(copy-list (caar var-value-list)) ,@ body)
		    ,@(copy-list (caar var-value-list)))))
	(T `(apply #'map-fn T
		   #'(lambda ,(apply #'append (mapcar #'car var-value-list))
		       ,@ body)
		   (nconc ,@(mapcar #'(lambda (p)
					(if (null (cdar p)) `(list ,(cadr p))
					    `(forceL ,(length (car p))
						     (multiple-value-list
						       ,(cadr p)))))
				    var-value-list))))))

;This allows you to specify more or less arbitrary transducers.

(defS producing (output-list input-list &rest body) "" non-opt-producing
 :optimizer
   (optimize-producing output-list input-list body)
 :trigger
   (dolist (pair (caddr call) nil)
     (if (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
	 (return T)))
 :discriminator
   (dolist (pair (cadr call) nil)
     (if (not (consp pair)) (return T))))

#+symbolics(setf (gethash 'producing zwei:*lisp-indentation-offset-hash-table*)
		 '(2 1))

(defmacro terminate-producing ()
    "Causes the containing call on producing to terminate."
  `(go ,END))

(cl:defun validate-producing (output-list input-list body)
  (if (not (and (every #'(lambda (f) (eq-car f 'declare)) (butlast body))
		(eq-car (car (last body)) 'loop)
		(eq-car (cadr (car (last body))) 'tagbody)))
      (ers 73 "~%PRODUCING body not of the form ({DECL}* (LOOP (TAGBODY ...)))~%"
	   (list* 'producing output-list input-list body)))
  (if (null output-list)
      (ers 74 "~%PRODUCING fails to have any outputs~%"
	   (list* 'producing output-list input-list body)))
  (mapc #'(lambda (p) (normalize-pair p nil)) output-list)
  (mapc #'(lambda (p) (normalize-pair p nil)) input-list)
  (cl:let ((visited-inputs nil) (visited-outputs nil))
    (dolist (f (cdadar (last body)))
      (cond ((and (eq-car f 'setq)
		  (eq-car (caddr f) 'next-in))
	     (if (not (and (null (cdddr f))
			   (cadr (caddr f)) (symbolp (cadr (caddr f)))
			   (find (cadr (caddr f))  input-list
				 :key #'(lambda (e) (when (consp e) (car e))))
			   (not (member (cadr (caddr f)) visited-inputs))
			   (cddr (caddr f))))
		 (ers 75 "~%Malformed NEXT-IN call: " (caddr f)))
	     (push (cadr (caddr f)) visited-inputs))
	    ((eq-car f 'next-out)
	     (if (not (and (null (cdddr f))
			   (member (cadr f) output-list)
			   (not (member (cadr f) visited-outputs))))
		 (ers 76 "~%Malformed NEXT-OUT call: " f))
	     (push (cadr f) visited-outputs))))
    (values visited-inputs visited-outputs)))

(cl:defun optimize-producing (output-list input-list body)
  (cl:let ((series-ins (validate-producing output-list input-list body)))
    (cl:multiple-value-bind (bod type-alist propagations)
	(decode-dcls body '(types props))
      (cl:let* ((forms (cdadar bod))
		  (frag (make-frag))
		  (input-alist nil)
		  (series-output-alist nil)
		  (*renames* *renames*)
		  (new-renames *renames*))
        (dolist (p (append (remove-if-not #'consp output-list) input-list))
          (setq p (normalize-pair p nil))
          (cl:let* ((value (retify (cadr p)
				     (or (cdr (assoc (caar p) type-alist)) T)))
		      (arg (make-sym :var (gensym (root (caar p)))
				     :series-var-p
				     (not (null (member (caar p) series-ins))))))
            (+arg arg frag)
            (+dflow value arg)
            (setf (free-out value) (caar p))
            (push (cons (caar p) (var arg)) new-renames)
            (push (cons (caar p) arg) input-alist)))
        (setq *renames* new-renames) ;note let-like binding semantics
        (dolist (p output-list)
          (if (consp p)
            (+ret (make-sym :var (var (cdr (assoc (car p) input-alist)))) frag)
            (cl:let ((ret (make-sym :var (gensym (root p)) :series-var-p T))
		       (type (cdr (assoc p type-alist))))
              (if (eq-car type 'series) (setq type (cadr type)) (setq type T))
              (+ret ret frag)
              (push (list (var ret) type) (aux frag))
              (push (cons p (var ret)) *renames*)
              (push (cons p ret) series-output-alist))))
        (cl:let* ((label-alist nil) (new-forms nil))
          (dolist (f forms)
            (cond ((not (symbolp f)) (push f new-forms))
                  (T (cl:let ((new (gensym (root f))))
                       (push (cons `(go ,f) `(go ,new)) label-alist)
                       (push new new-forms)))))
          (when label-alist
            (setq forms (sublis label-alist (nreverse new-forms) :test #'equal))))
        
        (cl:let ((*in-series-expr* nil)
		   (*not-straight-line-code* nil)
		   (body (basic-prod-form->frag-body
			   forms input-alist series-output-alist)))
          (cl:multiple-value-bind (bod free-ins free-outs setqed)
	      (handle-non-series-stuff `(progn ,@ body) nil
				       (mapcar #'(lambda (s) (var (cdr s)))
					       input-alist))
            (setf (body frag) (cdr bod))
            (dolist (entry free-ins)
              (cl:let ((arg (make-sym :var (car entry))))
                (+arg arg frag)
                (+dflow (cdr entry) arg)))
            (dolist (entry free-outs)
              (cl:let ((new (make-sym :var (car entry)))
			 (v (cdr entry)))
                (when (not (find (car entry) (args frag) :key #'var))
                  (push (list (car entry) T) (aux frag)))
                (setf (free-out new) v)
                (+ret new frag)
                (rplacd (assoc v *renames*) new)))
            (dolist (v setqed)
              (protect-from-setq
               (cdr (find-if #'(lambda (s)
                                 (eq v (var (cdr s))))
                             input-alist))
               (or (cdr (assoc (car (rassoc v *renames*))
                               type-alist))
                   T))))
          ;bit of a cludge the way the following bashes things into the old style of stuff.
          (dolist (pair propagations)
            (cl:let ((input (cdr (assoc (car pair) input-alist)))
		       (output (cdr (assoc (cadr pair) series-output-alist))))
              (when (and input output)
                (setf (var output) (var input)))))
          (+frag frag))))))

(cl:defun basic-prod-form->frag-body (forms input-alist series-output-alist)
  (setq forms  ;this is dangerous, should be done more safely.
        (subst `(go ,end) `(terminate-producing) forms :test #'equal))
  (cl:let ((revbody nil)
	     (state :prolog)
	     (epilog-start
	       (do ((l (cons nil (reverse forms)) (cdr l)))
		   ((or (null (cdr l)) (not (eq-car (cadr l) 'next-out)))
		    (car l)))))
    (dolist (f forms)
      (cond ((and (eq-car f 'setq) (eq-car (caddr f) 'next-in))
             (cl:let* ((source (cadr (caddr f)))
			 (arg (cdr (assoc source input-alist)))
			 (actions (cddr (caddr f)))
			 (destination (cadr f))
			 (E (new-var 'ee))
			 (D (new-var 'dd))
			 (-X- (new-var '-xxx-)))
               (setf (series-var-p arg) T)
               (cond ((and (eq state :prolog)
                           (equal actions '((terminate-producing)))))
                     ((equal actions '((terminate-producing)))
                      (setf (off-line-spot arg) -X-) (push -X- revbody))
                     (T (setq state :middle)
                        (setf (off-line-spot arg) -X-)
                        (setf (off-line-exit arg) E)
                        (setq revbody
                              (append (reverse `(,-X- (go ,D) ,E ,@ actions ,D))
                                      revbody))))
               (push `(setq ,destination ,(var arg)) revbody)))
            ((eq-car f 'next-out)
             (setq state (if (or (eq state :epilog) (eq f epilog-start))
                           :epilog :middle))
             (cl:let* ((ret (cdr (assoc (cadr f) series-output-alist)))
			 (-X- (new-var '-xxxx-)))
               (setf (series-var-p ret) T)
               (push `(setq ,(var ret) ,(caddr f)) revbody)
               (when (not (eq state :epilog))
                 (setf (off-line-spot ret) -X-)
                 (push -X- revbody))))
            (T (setq state :middle)
               (push f revbody))))
    (nreverse revbody)))

(cl:defun protect-from-setq (in type)
  (cl:let ((frag (fr in))
	     (var (var in))
	     (new (new-var 'in)))
    (push (list var type) (aux (fr in)))
    (coerce-to-type type in) ;why am I doing this?
    (cond ((not (series-var-p in))
	   (push `(setq ,var ,new) (prolog frag)))
	  ((not (off-line-spot in))
	   (push `(setq ,var ,new) (body frag)))
	  (T (nsubst-inline `((setq ,var ,new)) (off-line-spot in) (body frag) T)))
    (setf (var in) new)))

;This turns a producing form into a frag that fits the requirements of a frag well
;enough that we can call FRAG->PHYSICAL on it.  The key to this is that we only
;keep the series inputs and outputs, and we know exactly where they can be used.

(defmacro non-opt-producing (output-list input-list &body body)
  (cl:let ((series-inputs (validate-producing output-list input-list body)))
    (cl:multiple-value-bind (bod props) (decode-dcls body '(props no-complaints))
      (setq input-list
            (mapcar #'(lambda (p) (if (consp p) p (list p nil))) input-list))
      (cl:let* ((forms (cdadar bod))
		  (frag (make-frag :code `(producing ,output-list ,input-list
					    ,@ body)))
		  (input-alist nil)
		  (series-output-alist nil))
        (dolist (in series-inputs)
          (cl:let* ((v (new-var 'inpt))
		      (arg (make-sym :var v :series-var-p T)))
            (+arg arg frag)
            (push (cons in arg) input-alist)))
        (dolist (out output-list)
          (cl:let* ((prop-input (dolist (p props nil)
				    (if (eq (cadr p) out) (return (car p)))))
		      (v (cond (prop-input
				(var (cdr (assoc prop-input input-alist))))
			       ((consp out) (car out))
			       (T (new-var 'out))))
		      (ret (make-sym :var v :series-var-p (not (consp out)))))
            (+ret ret frag)
            (unless prop-input
              (push (list v t) (aux frag)))
            (if (not (consp out))
              (push (cons out ret) series-output-alist)
              (push `(setq ,(car out) ,(cadr out)) (prolog frag)))))
        (setf (body frag)
              (basic-prod-form->frag-body forms input-alist series-output-alist))
        `(cl:let ,input-list
           ,(frag->physical frag series-inputs (some #'consp output-list)))))))

;The alter form found probably refers to vars which are not OLD itself.
;For this to be OK, we must be sure that these variables must never be
;renamed.  To ensure that, we must ensure that none of these variables are
;ever inputs.  (Aux variables are not renamed and return variables are not
;renamed as long as they are not also inputs.)  This requires care on the
;part of all standard functions that are alterable (i.e. scan) and
;particularly in to-alter which would be much easier to write if it just
;passed the inputs through.

;This is ok because outputs never get renamed.  Also
;the input old to the frag most likely never gets used, but this
;makes sure that the dflow is logically correct.

(defS alter (destinations items)
    "Alters the values in DESTINATIONS to be ITEMS."
  (fragL ((destinations) (items T)) ((result))
	 ((gen list) (result null)) ()
	 ((setq gen (generator destinations)) (setq result nil))
	 ((do-next-in gen #'(lambda () (go END)) items)) () ())
 :optimizer
  (cl:let ((ret (retify destinations)))
    (when (not (series-var-p ret))
      (rrs 5 "~%Alter applied to a series that is not known at compile time:~%"
	   *call*))
    (cl:let* ((form (find-alter-form ret))
		(frag (literal-frag '(((old T) (items T)) ((result)) ((result null))
				      () ((setq result nil)) () () ()))))
      (when (null form)
	(ers 65 "~%Alter applied to an unalterable series:~%" *call*)) 
      (setf (body frag) (list (subst (var (cadr (args frag))) '*alt* form)))
      (funcall-frag frag (list ret items))))
  :trigger T)

(defS to-alter (series alter-function &rest other-inputs)
  "Specifies how to alter SERIES."
  (progn (setq other-inputs (copy-list other-inputs))
         (cl:let ((series (generator series))
		    (others (mapcar #'generator other-inputs)))
           (make-phys
            :gen-fn
            #'(lambda ()
                (block nil
                  (cons (cons (next-in series (return nil))
                              (mapcar #'(lambda (x) (next-in x (return nil)))
                                      others))
                        t)))
            :alter-fn
            #'(lambda (alter &rest other-info)
                (apply alter-function alter other-info)))))
  :optimizer
  (cl:let* ((params (list series))
	      (frag (make-frag))
	      (input-vars (n-gensyms (length other-inputs) "STATE-IN-"))
	      (state-vars (n-gensyms (length other-inputs) "STATE-"))
	      (var (new-var 'items)))
    (+ret (make-sym :var var :series-var-p T) frag)
    (+arg (make-sym :var var :series-var-p T) frag)
    (multiple-value-setq (alter-function params)
                         (handle-fn-arg frag alter-function params))
    (mapc #'(lambda (in-var state-var)
	      (+arg (make-sym :var in-var :series-var-p T) frag)
	      (push (list state-var t) (aux frag))
	      (push `(setq ,state-var ,in-var) (body frag)))
          input-vars state-vars)
    (setq params (append params other-inputs))
    (setf (alterable frag)
          `((,var (cl:funcall ,alter-function *alt* ,@ state-vars) ,@ state-vars)))
    (funcall-frag frag params)))

(cl:defun find-alter-form (ret)
  (cl:let* ((v (var ret))
	      (form (cadr (assoc v (alterable (fr ret))))))
    (if form form
	(dolist (a (args (fr ret)))
	  (when (or (eq v (var a))
		    (equal (prolog (fr ret)) `((setq ,v ,(var a)))))
	    (return (find-alter-form (prv a))))))))

(defS series (expr &rest expr-list)
  "Creates an infinite series of the results of the expressions."
  (cond ((null expr-list)
         (fragL ((expr)) ((expr T)) () () () () () ()))
        (T (cl:let ((full-expr-list
			(opt-non-opt `(list ,expr ,@ expr-list)
				     (cons expr (copy-list expr-list)))))
             (fragL ((full-expr-list)) ((items T)) ((items (or null t)) (lst list)) ()
                    ((setq lst (copy-list full-expr-list))
                     (setq lst (nconc lst lst)))
                    ((setq items (car lst)) (setq lst (cdr lst))) () ())))))

;put on #Z
(cl:defun series-reader (stream subchar arg)
    (declare (ignore subchar arg))
  `(literal-series ',(read stream T nil T)))

(defS literal-series (seq) ""
  (make-phys :data-list seq)
  :optimizer
  (+frag
   (cl:let ((frag (literal-frag
		      '(() ((elements T)) ((listptr list) (elements T)) ()
			() ((if (endp listptr) (go END))
			    (setq elements (car listptr))
			    (setq listptr (cdr listptr))) () ()))))
     (push `(setq ,(caar (aux frag)) ,seq) (prolog frag))
     frag)))

(defS make-series (item &rest item-list)
    "Creates a series of the items."
  (make-phys :data-list (cons item (copy-list item-list)))
  :optimizer
    (macroexpand `(scan (list ,item ,@ (copy-list item-list)))))


#-old-scan-range
(progn
;; This deftype is used to appease the compiler (CMUCL) about an
;; unknown type when compiling scan-range.  If someone has a better
;; way of getting scan-range to generate the correct initial values
;; with this hack, I'd love to have it.
(deftype *type* () t)

;; This version of scan-range fixes a long-standing bug that occurs
;; when :type is not 'number.  Say :type is 'single-float.  Then the
;; looping variables are declared to be single-float's, but the
;; variables end up being initialized with fixnums.  This loses.
;; However, you need a reasonably smart compiler to do constant
;; folding so that you don't do the coercion every time through the
;; loop.
(defS scan-range (&key (from 0) (by 1)
		       (upto nil) (below nil)
		       (downto nil) (above nil)
		       (length nil) (type 'number))
    "Creates a series of numbers by counting from :FROM by :BY."
  (cl:let ((*type* (opt-non-opt (if (eq-car type 'quote) (cadr type) 'number)
				  type)))
    (if (> (length (delete nil (list upto below downto above length))) 1)
	(ers 77 "~%Too many keywords specified in a call on SCAN-RANGE."))
    (cond (upto
	   (fragL ((from) (upto) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (coerce (- from by) '*type*)))
		  ((setq numbers (+ numbers (coerce by '*type*)))
		   (if (> numbers upto) (go END))) () ()))
	  (below
	   (fragL ((from) (below) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (coerce (- from by) '*type*)))
		  ((setq numbers (+ numbers (coerce by '*type*)))
		   (if (not (< numbers below)) (go END))) () ()))
	  (downto
	   (fragL ((from) (downto) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (coerce (- from by) '*type*)))
		  ((setq numbers (+ numbers (coerce by '*type*)))
		   (if (< numbers downto) (go END))) () ()))
	  (above
	   (fragL ((from) (above) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (coerce (- from by) '*type*)))
		  ((setq numbers (+ numbers (coerce by '*type*)))
		   (if (not (> numbers above)) (go END))) () ()))
	  #+cmu
	  (length
	   (fragL ((from) (length) (by)) ((numbers T))
		  ((numbers *type*) (counter fixnum)) ()
		  ((setq numbers (- from by)) (setq counter length))
		  ((setq numbers (+ numbers by))
		   (if (not (plusp counter)) (go END))
		   (decf counter)) () ()))
	  #-cmu
	  (length
	   (fragL ((from) (length) (by)) ((numbers T))
		  ((numbers *type*) (counter fixnum)) ()
		  ((setq numbers (coerce (- from by) '*type*))
		   (setq counter length))
		  ((setq numbers (+ numbers (coerce by '*type*)))
		   (if (not (plusp counter)) (go END))
		   (decf counter)) () ()))
	  (T (fragL ((from) (by)) ((numbers T)) ((numbers *type*)) ()
		    ((setq numbers (coerce (- from by) '*type*)))
		    ((setq numbers (+ numbers (coerce by '*type*))))
		    () ())))))
)

#+old-scan-range
(defS scan-range (&key (from 0) (by 1)
		       (upto nil) (below nil)
		       (downto nil) (above nil)
		       (length nil) (type 'number))
    "Creates a series of numbers by counting from :FROM by :BY."
  (cl:let ((*type* (opt-non-opt (if (eq-car type 'quote) (cadr type) 'number)
				  type)))
    (if (> (length (delete nil (list upto below downto above length))) 1)
	(ers 77 "~%Too many keywords specified in a call on SCAN-RANGE."))
    (cond (upto
	   (fragL ((from) (upto) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (- from by)))
		  ((setq numbers (+ numbers by))
		   (if (> numbers upto) (go END))) () ()))
	  (below
	   (fragL ((from) (below) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (- from by)))
		  ((setq numbers (+ numbers by))
		   (if (not (< numbers below)) (go END))) () ()))
	  (downto
	   (fragL ((from) (downto) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (- from by)))
		  ((setq numbers (+ numbers by))
		   (if (< numbers downto) (go END))) () ()))
	  (above
	   (fragL ((from) (above) (by)) ((numbers T)) ((numbers *type*)) ()
		  ((setq numbers (- from by)))
		  ((setq numbers (+ numbers by))
		   (if (not (> numbers above)) (go END))) () ()))
	  (length
	   (fragL ((from) (length) (by)) ((numbers T))
		  ((numbers *type*) (counter fixnum)) ()
		  ((setq numbers (- from by)) (setq counter length))
		  ((setq numbers (+ numbers by))
		   (if (not (plusp counter)) (go END))
		   (decf counter)) () ()))
	  (T (fragL ((from) (by)) ((numbers T)) ((numbers *type*)) ()
		    ((setq numbers (- from by)))
		    ((setq numbers (+ numbers by))) () ())))))

(defS scan (seq-type &optional (seq nil seq-p))
    "Enumerates a series of the values in a sequence"
  (cl:let (type limit *type*)
    (when (not seq-p) ;it is actually seq-type that is optional
      (setq seq seq-type)
      (setq seq-type (optq 'list)))
    (multiple-value-setq (type limit *type*) (decode-seq-type (non-optq seq-type)))
    (cond ((member type '(list bag))
	   (fragL ((seq)) ((elements T))
		  ((elements *type*) (listptr list) (parent list))
		  ((elements (setf (car parent) *alt*) parent))
		  ((setq listptr seq))
		  ((if (endp listptr) (go END))
		   (setq parent listptr)
		   (setq elements (car listptr))
		   (setq listptr (cdr listptr))) () ()))
	  (limit
	   (fragL ((seq) (limit)) ((elements T))
		  ((elements *type*) (temp T) (index fixnum))
		  ((elements (setf (aref temp index) *alt*) temp index))
		  ((setq index -1) (setq temp seq))
		  ((incf index)
		   (if (not (< index limit)) (go END))
		   (setq elements (aref seq index))) () ()))
	  ((not (eq type 'sequence)) ;some kind of vector
	   (fragL ((seq)) ((elements T))
		  ((elements *type*) (limit fixnum) (temp T) (index fixnum))
		  ((elements (setf (aref temp index) *alt*) temp index))
		  ((setq index -1) (setq limit (length seq)) (setq temp seq))
		  ((incf index)
		   (if (not (< index limit)) (go END))
		   (setq elements (aref seq index))) () ()))
	  (T (fragL ((seq-type) (seq)) ((elements T)) ;dummy type input avoids warn
		    ((elements *type*) (limit fixnum) (temp T) (index fixnum))
		    ((elements (setf (elt temp index) *alt*) temp index))
		    ((setq index -1) (setq limit (length seq)) (setq temp seq))
		    ((incf index)
		     (if (not (< index limit)) (go END))
		     (setq elements (elt seq index))) () ())))))

(defS scan-multiple (type sequence &rest sequences)
    "Scans multiple sequences in parallel"
  (cl:let ((types (mapcar #'cadr
			    (decode-multiple-types-arg (list 'quote type)
						       (1+ (length sequences))))))
    (apply #'cotruncate
	   (scan (car types) sequence)
	   (mapcar #'scan* (cdr types) sequences)))
 :optimizer
  (cl:let ((types (decode-multiple-types-arg type (1+ (length sequences)))))
    `(cotruncate (scan ,(car types) ,sequence)
		 ,@(mapcar #'(lambda (type seq) `(scan* ,type ,seq))
			   (cdr types) sequences))))

(cl:defun decode-multiple-types-arg (type n)
  (cond ((or (not (eq-car type 'quote))
	     (not (eq-car (cadr type) 'values)))
	 (make-list n :initial-element type))
	(T (if (not (= (length (cdadr type)) n))
	       (ers 78 "~%SCAN-MULTIPLE: type and number of sequences conflict."))
	 (mapcar #'(lambda (x) `(quote ,x)) (cdadr type)))))

(defS scan* (seq-type seq)
    "Enumerates a series of the values in SEQ without checking for the end."
  (cl:multiple-value-bind (type limit *type*)
      (decode-seq-type (non-optq seq-type))
      (declare (ignore limit))
    (cond ((member type '(list bag))
	   (fragL ((seq)) ((elements T))
		  ((elements *type*) (listptr list) (parent list))
		  ((elements (setf (car parent) *alt*) parent))
		  ((setq listptr seq))
		  ((setq parent listptr)
		   (setq elements (car listptr))
		   (setq listptr (cdr listptr))) () ()))
	  ((not (eq type 'sequence)) ;some kind of vector
	   (fragL ((seq)) ((elements T))
		  ((elements *type*) (temp T) (index fixnum))
		  ((elements (setf (aref temp index) *alt*) temp index))
		  ((setq index -1) (setq temp seq))
		  ((incf index)
		   (setq elements (aref seq index))) () ()))
	  (T (fragL ((seq-type) (seq)) ((elements T)) ;dummy type input avoids warn
		    ((elements *type*) (temp T) (index fixnum))
		    ((elements (setf (elt temp index) *alt*) temp index))
		    ((setq index -1) (setq temp seq))
		    ((incf index)
		     (setq elements (elt seq index))) () ())))))

;;; DECODE-SEQ-TYPE
;;;
;;; DECODE-SEQ-TYPE tries to canonicalize the given type into the
;;; underlying type.  It returns three values: the sequence type
;;; (string, vector, simple-array, sequence), the length of the
;;; sequence (or NIL if not known) and the element type of the
;;; sequence (or T if not known).
(cl:defun decode-seq-type (type)
  (cond ((not (and (eq-car type 'quote)
		   (setq type (cadr type))))
	 (values 'sequence nil T))
	((and (symbolp type)
	      (string= (string type) "BAG"))
	 (values 'bag nil T))
	((and (consp type)
	      (symbolp (car type))
	      (string= (string (car type)) "BAG"))
	 (values 'bag nil (cadr type)))
	(t
	 ;; Hmm, should we use subtypep to handle these?  Might be easier.
	 (cond ((eq type 'list)
		(values 'list nil T))
	       ((eq-car type 'list)
		(values 'list nil (cadr type)))
	       ((eq type 'sequence)
		(values 'sequence nil T))
	       ;; A STRING is canonicalized to (VECTOR CHARACTER)
	       ((eq type 'string)
		(values 'vector nil 'character))
	       ((eq-car type 'string)
		(values 'vector (if (numberp (cadr type)) (cadr type)) 'character))
	       ;; But SIMPLE-STRING's are really (SIMPLE-ARRAY CHARACTER (*))
	       ((eq type 'simple-string)
		(values 'simple-array nil 'character))
	       ((eq-car type 'simple-string)
		(values 'simple-array
			(if (numberp (cadr type)) (cadr type))
			'character))
	       ;; A BIT-VECTOR is (vector bit)
	       ((eq type 'bit-vector)
		(values 'vector nil 'bit))
	       ((eq-car type 'bit-vector)
		(values 'vector (if (numberp (cadr type)) (cadr type)) 'bit))
	       ;; But a SIMPLE-BIT-VECTOR is really a (SIMPLE-ARRAY BIT (*))
	       ((eq type 'simple-bit-vector)
		(values 'simple-array nil 'bit))
	       ((eq-car type 'simple-bit-vector)
		(values 'simple-array
			(if (numberp (cadr type)) (cadr type))
			'bit))
	       ;; A VECTOR is just a VECTOR
	       ((eq type 'vector)
		(values 'vector nil T))
	       ((eq-car type 'vector)
		(values 'vector (if (numberp (caddr type)) (caddr type))
			(if (not (eq (cadr type) '*)) (cadr type) T)))
	       ;; And a SIMPLE-VECTOR is just a SIMPLE-VECTOR
	       ((eq type 'simple-vector)
		(values 'simple-vector nil T))
	       ((eq-car type 'simple-vector)
		(values 'simple-vector (if (numberp (cadr type)) (cadr type)) T))
	       ;; A SIMPLE-ARRAY is a SIMPLE-ARRAY.  This assumes you
	       ;; specified a one-dimensional simple-array.  Results
	       ;; are undefined if it's not a one-dimensional array.
	       ((eq type 'simple-array)
		(values 'simple-array nil T))
	       ((eq-car type 'simple-array)
		(values 'simple-array
			(if (not (eq (caaddr type) '*)) (caaddr type))
			(if (not (eq (cadr type) '*))
			    (cadr type)
			    T)))
	       ;; We don't need to handle anything else like arrays
	       ;; because series only handles 1-dimensional
	       ;; sequences.

	       ;; Everything else is a sequence
	       (T
		(values 'sequence nil T))))))

(defS scan-sublists (lst)
    "Creates a series of the sublists in a list."
  (fragL ((lst)) ((sublists T)) ((sublists list) (lstptr list)) ()
	 ((setq lstptr lst))
	 ((if (endp lstptr) (go END))
	  (setq sublists lstptr)
	  (setq lstptr (cdr lstptr))) () ()))

(defS scan-alist (alist &optional (test #'eq))
    "Creates two series containing the keys and values in an alist."
  (fragL ((alist) (test)) ((keys T) (values T))
	 ((alistptr list) (keys t) (values t) (parent list))
	 ((keys (setf (car parent) *alt*) parent)
	  (values (setf (cdr parent) *alt*) parent))
	 ((setq alistptr alist))
	 (L (if (null alistptr) (go END))
	    (setq parent (car alistptr))
	    (setq alistptr (cdr alistptr))
	    (if (or (null parent)
		    (not (eq parent (assoc (car parent) alist :test test))))
		(go L))
	    (setq keys (car parent))
	    (setq values (cdr parent))) () ()))

(defS scan-plist (plist)
    "Creates two series containing the indicators and values in a plist."
  (fragL ((plist)) ((indicators T) (values T))
	 ((indicators t) (values t) (plistptr list) (parent list))
	 ((indicators (setf (car parent) *alt*) parent)
	  (values (setf (cadr parent) *alt*) parent))
	 ((setq plistptr plist))
	 (L (if (null plistptr) (go END))
	    (setq parent plistptr)
	    (setq indicators (car plistptr))
	    (setq plistptr (cdr plistptr))
	    (setq values (car plistptr))
	    (setq plistptr (cdr plistptr))
	    (do ((ptr plist (cddr ptr)))
		((eq (car ptr) indicators)
		 (if (not (eq ptr parent)) (go L))))) () ()))

(defS scan-lists-of-lists (tree &optional (test #'atom test-p))
    "Creates a series of the nodes in a tree."
  (if test-p
      (fragL ((tree) (test)) ((nodes T)) ((nodes T) (state list)) ()
	     ((setq state (list tree)))
	     ((if (null state) (go END))
	      (setq nodes (car state))
	      (setq state (cdr state))
	      (when (not (or (atom nodes) (cl:funcall test nodes)))
		(do ((ns nodes (cdr ns))
		     (r nil (cons (car ns) r)))
		    ((not (consp ns))
		     (setq state (nreconc r state)))))) () ())
      (fragL ((tree)) ((nodes T)) ((nodes T) (state list)) ()
	     ((setq state (list tree)))
	     ((if (null state) (go END))
	      (setq nodes (car state))
	      (setq state (cdr state))
	      (when (not (atom nodes))
		(do ((ns nodes (cdr ns))
		     (r nil (cons (car ns) r)))
		    ((not (consp ns))
		     (setq state (nreconc r state)))))) () ())))

(defS scan-lists-of-lists-fringe (tree &optional (test #'atom test-p))
    "Creates a series of the leaves of a tree."
  (if test-p
      (fragL ((tree) (test)) ((leaves T))
	     ((leaves T) (parent list) (state list))
	     ((leaves (setf (car parent) *alt*) parent))
	     ((setq state (list (list tree))))
	     (L (if (null state) (go END))
		(setq leaves (car state))
		(setq state (cdr state))
		(setq parent leaves)
		(setq leaves (car leaves))
		(when (not (or (atom leaves) (cl:funcall test leaves)))
		  (do ((ns leaves (cdr ns))
		       (r nil (cons ns r)))
		      ((not (consp ns)) (setq state (nreconc r state))))
		  (go L))) () ())
      (fragL ((tree)) ((leaves T))
	     ((leaves T) (parent list) (state list))
	     ((leaves (setf (car parent) *alt*) parent))
	     ((setq state (list (list tree))))
	     (L (if (null state) (go END))
		(setq leaves (car state))
		(setq state (cdr state))
		(setq parent leaves)
		(setq leaves (car leaves))
		(when (not (atom leaves))
		  (do ((ns leaves (cdr ns))
		       (r nil (cons ns r)))
		      ((not (consp ns)) (setq state (nreconc r state))))
		  (go L))) () ())))

#-symbolics
(defS scan-symbols (&optional (package nil))
    "Creates a series of the symbols in PACKAGE."
  (fragL ((package)) ((symbols T)) ((symbols symbol) (lst list)) ()
	 ((setq lst nil)
	  (do-symbols (s (or package *package*)) (push s lst)))
	 ((if (null lst) (go END))
	  (setq symbols (car lst))
	  (setq lst (cdr lst))) () ()))

#+symbolics ;see do-symbols
(defS scan-symbols (&optional (package nil))
    "Creates a series of the symbols in PACKAGE."
  (fragL ((package)) ((symbols T)) ((index T) (state T) (symbols symbol)) ()
	 ((multiple-value-setq (index symbols state)
	    (si:loop-initialize-mapatoms-state (or package *package*) nil)))
	 ((if (multiple-value-setq (nil index symbols state)
		(si:loop-test-and-step-mapatoms index symbols state))
	      (go END))) () ()))

(defS scan-file (name &optional (reader #'read))
    "Creates a series of the forms in the file named NAME."
  (fragL ((name) (reader)) ((items T)) ((items T) (lst list)) ()
	 ((setq lst nil)
	  (with-open-file (f name :direction :input)
	    (cl:let ((done (list nil)))
	      (loop		 
		(cl:let ((item (cl:funcall reader f nil done)))
		  (when (eq item done)
		    (setq lst (nreverse lst))
		    (return nil))
		  (push item lst))))))
	 ((if (null lst) (go END))
	  (setq items (car lst))
	  (setq lst (cdr lst))) () ())
 :optimizer
  (funcall-literal-frag
    (cl:let ((file (new-var 'file)))
      `((((reader)) ((items T)) ((items t) (done t)) ()
	 ((setq done (list nil)))
	 ((if (eq (setq items (cl:funcall reader ,file nil done)) done)
	      (go END))) ()
	 (#'(lambda (code)
	      (list 'with-open-file
		    '(,file ,name :direction :input)
		    code)))) ,reader))))

(defS scan-stream (name &optional (reader #'read))
    "Creates a series of the forms in the stream NAME."
  (fragL ((name) (reader)) ((items T)) ((items T) (lst list)) ()
	 ((setq lst nil)
	  (cl:let ((done (list nil)))
	    (loop		 
		(cl:let ((item (cl:funcall reader name nil done)))
		  (when (eq item done)
		    (setq lst (nreverse lst))
		    (return nil))
		  (push item lst)))))
	 ((if (null lst) (go END))
	  (setq items (car lst))
	  (setq lst (cdr lst))) () ())
 :optimizer
  (funcall-literal-frag
   `((((reader)) ((items T)) ((items t) (done t)) ()
      ((setq done (list nil)))
      ((if (eq (setq items (cl:funcall reader ,name nil done)) done)
	   (go END))) ()
      ())
     ,reader
     )))

(defS scan-hash (table)
    "Creates two series containing the keys and values in a hash table."
  #-CLISP
  (fragL ((table)) ((keys T) (values T)) ((keys T) (values T) (lst list)) ()
	 ((setq lst nil)
	  (maphash #'(lambda (key val) (push (cons key val) lst)) table))
	 ((if (null lst) (go END))
	  (setq keys (caar lst))
	  (setq values (cdar lst))
	  (setq lst (cdr lst))) () ())
  #+CLISP
  (fragL ((table)) ((keys T) (values T)) ((state T) (nextp T) (keys T) (values T)) ()
         ((setq state (sys::hash-table-iterator table)))
         ((multiple-value-setq (nextp keys values) (sys::hash-table-iterate state))
          (unless nextp (go END)))
	 () ())
#+symbolics :optimizer #+symbolics
  (funcall-literal-frag
    `((((table)) ((keys T) (values T)) ((state T) (keys T) (values T)) ()
       ((setq state nil))
       ((if (not (multiple-value-setq (state keys values)
		   (si:send table :next-element state)))
	    (go END))) ()
       (#'(lambda (c) `(si:inhibit-gc-flips ,c)))) ,table))
  )

(defS previous (items &optional (default nil) (amount 1))
    "Shifts ITEMS to the right by AMOUNT inserting DEFAULT."
  (cond ((eql amount 1)
	 (fragL ((items T) (default)) ((shifted-items T))
		((shifted-items (series-element-type items))
		 (state (series-element-type items))) ()
		((setq state default))
		((setq shifted-items state) (setq state items)) () ()))
	(T (fragL ((items T) (default) (amount)) ((shifted-items T))
		  ((shifted-items (series-element-type items)) (ring list)) ()
		  ((setq ring (make-list (1+ amount) :initial-element default))
		   (nconc ring ring))
		  ((setf (car ring) items) (setq ring (cdr ring))
		   (setq shifted-items (car ring))) () ()))))

(defS latch (items &key (after nil) (before nil) (pre nil pre-p) (post nil post-p))
    "Modifies a series before or after a latch point."
  (progn (when (and after before)
	   (ers 79 "~%:AFTER and :BEFORE both specified in a call on LATCH."))
	 (if (not (or before after)) (setq after 1))
	 (if (null pre-p) (setq post-p T))
	 (cond (after
		(fragL ((items T) (after) (pre) (pre-p) (post) (post-p))
		       ((masked-items T)) ((masked-items T) (state fixnum)) ()
		       ((setq state after))
		       ((cond ((plusp state) (if items (decf state))
			       (setq masked-items (if pre-p pre items)))
			      (T (setq masked-items (if post-p post items)))))()()))
	       (T (fragL ((items T) (before) (pre) (pre-p) (post) (post-p))
			 ((masked-items T))
			 ((masked-items T) (state fixnum)) ()
			 ((setq state before))
			 ((cond ((and (plusp state)
				      (or (null items)
					  (not (zerop (setq state (1- state))))))
				 (setq masked-items (if pre-p pre items)))
				(T (setq masked-items
					 (if post-p post items))))) () ())))))

(cl:defun promote-series (series)
  (cond ((not (alter-fn series)) series)
	(T (setq series (if (image-series-p series)
			    (copy-image-series series)
			    (copy-basic-series series)))
	   (setf (alter-fn series) nil)
	   series)))

(defS cotruncate (&rest items-list)
    "Truncates the inputs so that they are all no longer than the shortest one."
  (values-lists (length items-list)
		(apply #'map-fn t #'list (mapcar #'promote-series items-list))
		(mapcar #'alter-fn items-list))
 :optimizer
  (cl:let* ((args (copy-list items-list))
	      (vars (n-gensyms (length args) "COTRUNC-"))
	      (ports (mapcar #'(lambda (v) (list v t)) vars)))
    (funcall-frag
      (literal-frag `(,ports ,(copy-list ports) nil nil nil nil nil nil))
      args)))

(defS until (bools items-1 &rest items-i)
   "Returns ITEMS-I up to, but not including, the first non-null element of BOOLS."
  (if (null items-i) (until1 bools items-1)
      (apply #'cotruncate
	     (mapcar #'(lambda (i) (until1 bools i)) (cons items-1 items-i))))
 :optimizer
  (cl:let ((extra-ins (mapcar #'(lambda (x) (declare (ignore x))
					  (list (gensym "ITEMS") T))
				items-i)))
    (funcall-literal-frag
      (list* `(((bools T) (items T) ,@ extra-ins)
	       ((items T) ,@(copy-tree extra-ins))
	       () ()
	       () ((if bools (go END))) () ())
	     bools items-1 items-i))))

(defS until1 (bools items)
    "Returns ITEMS up to, but not including, the first non-null element of BOOLS."
  (fragL ((bools T) (items T)) ((items T)) () ()
	 () ((if bools (go END))) () ()))

(defS until-if (pred items-1 &rest items-i)
"Returns ITEMS-i up to, but not including, the first element which satisfies PRED."
  (if (null items-i) (until-if1 pred items-1 items-1)
      (apply #'cotruncate
	     (mapcar #'(lambda (i) (until-if1 pred items-1 i))
		     (cons items-1 items-i))))
 :optimizer
  (cl:let* ((params nil)
	      (frag (make-frag))
	      (item-vars (n-gensyms (1+ (length items-i)) "ITEMS-"))
	      (*state* nil))
    (multiple-value-setq (pred params) (handle-fn-arg frag pred params))
    (setq params (mapcar #'retify (nconc params (cons items-1 items-i))))
    (dolist (var item-vars)
      (+arg (make-sym :var var :series-var-p T) frag)
      (+ret (make-sym :var var :series-var-p T) frag))
    (setf (body frag)
	  `((if ,(car (handle-fn-call frag nil pred (list (car item-vars)) t))
		(go ,END))))
    (funcall-frag frag params)))

(defS until-if1 (pred items other-items)
  "Returns ITEMS up to, but not including, the first element which satisfies PRED."
  (fragL ((pred) (items T) (other-items T)) ((other-items T)) () ()
	 () ((if (cl:funcall pred items) (go END))) () ()))

(defS positions (bools)
    "Returns a series of the positions of non-null elements in bools."
  (fragL ((bools T -X-)) ((index T)) ((index fixnum)) ()
	 ((setq index -1))
	 (L -X- (incf index) (if (not bools) (go L))) () ()))

(defS mask (monotonic-indices)
    "Creates a series containing T in the indicated positions."
  (fragL ((monotonic-indices T -X- D)) ((bools T))
	 ((bools (member T nil)) (index fixnum)) ()
	 ((setq index 0 bools T))
	 (  (if (not bools) (go F))
	  -X- (go F) D (setq index -1)
	  F (setq bools (and (not (minusp index))
			     (= (prog1 index (incf index))
				monotonic-indices)))) () ()))

(defS choose (bools &optional (items nil items-p))
    "Chooses the elements of ITEMS corresponding to non-null elements of BOOLS."
  (cond (items-p
	 (fragL ((bools T) (items T)) ((items T -X-)) () ()
		() ((if (not bools) (go F)) -X- F) () ()))
	(T (fragL ((bools T -X-)) ((bools T)) () ()
		  () (L -X- (if (not bools) (go L))) () ()))))

(defS choose-if (pred items)
    "Chooses the elements of ITEMS for which PRED is non-null."
  (fragL ((pred) (items T -X-)) ((items T)) () ()
	 () (L -X- (if (not (cl:funcall pred items)) (go L))) () ()))

(defS expand (bools items &optional (default nil))
    "Spreads the elements of ITEMS out into the indicated positions."
  (fragL ((bools T) (items T -X-) (default)) ((expanded T))
	 ((expanded (series-element-type items))) () ()
	 ((when (not bools) (setq expanded default) (go F))
	  -X- (setq expanded items)
	  F) () ()))

(defS spread (gaps items &optional (default nil))
    "Spreads the elements of ITEMS by inserting copies of DEFAULT."
  (fragL ((gaps T) (items T) (default)) ((expanded T -X-))
	 ((expanded (series-element-type items)) (count fixnum)) () ()
	 ((setq count gaps)
	  L (setq expanded (if (zerop count) items default))
	    -X-
	    (when (plusp count) (decf count) (go L))) () ()))

(defS subseries (items start &optional (below nil below-p))
    "Returns the elements of items from START up to, but not including, BELOW."
  (cond (below-p
	 (fragL ((items T -X-) (start) (below)) ((items T)) ((index fixnum)) ()
		((setq index -1))
		(LP -X-
		    (incf index)
		    (if (not (< index below)) (go END))
		    (if (< index start) (go LP))) () ()))
	(T (fragL ((items T -X-) (start)) ((items T)) ((index fixnum)) ()
		  ((setq index (- -1 start)))
		  (LP -X-
		      (incf index)
		      (if (minusp index) (go LP))) () ()))))

(defS mingle (items1 items2 comparator)
    "Merges two series into one."
  (fragL ((items1 T -X1- F1) (items2 T -X2- F2) (comparator)) ((items T))
	 ((items (or (series-element-type items1) (series-element-type items2)))
	  (need1 fixnum) (need2 fixnum)) ()
	 ((setq need1 1 need2 1))
	 ((if (not (plusp need1)) (go F1))
	  (setq need1 -1)
	  -X1-
	  (setq need1 0)
	  F1 (if (not (plusp need2)) (go F2))
	  (setq need2 -1)
	  -X2-
	  (setq need2 0)
	  F2 (cond ((and (minusp need1) (minusp need2)) (go END))
		   ((minusp need1) (setq items items2) (setq need2 1))
		   ((minusp need2) (setq items items1) (setq need1 1))
		   ((not (cl:funcall comparator items2 items1))
		    (setq items items1) (setq need1 1))
		   (T (setq items items2) (setq need2 1)))) () ()))

(defS catenate (items1 items2 &rest more-items)
    "Concatenates two or more series end to end."
  (if more-items
      (catenate2 items1 (apply #'catenate items2 more-items))
      (catenate2 items1 items2))
 :optimizer
  (if more-items
      `(catenate2 ,items1 (catenate ,items2 ,@ more-items))
      `(catenate2 ,items1 ,items2)))

(defS catenate2 (items1 items2) ""
  (fragL ((items1 T -X- F) (items2 T -Y-)) ((items T))
	 ((items T) (flag (member T nil))) ()
	 ((setq flag nil))
	 (  (if flag (go B))
	  -X- (setq items items1) (go D)
	  F (setq flag T)
	  B -Y- (setq items items2) D) () ()))

(defS split (items bools &rest more-bools)
    "Divides a series into multiple outputs based on BOOLS."
  (cl:let* ((pos-lists
		(apply #'map-fn t
		       #'(lambda (item &rest bools)
			   (cons (apply #'pos bools) item))
		       (promote-series items)
		       (list* bools (copy-list more-bools)))))
    (values-list
      (mapcar #'(lambda (i)
		  (make-image-series :alter-fn (alter-fn items)
				     :image-fn #'image-of-with-datum
				     :image-datum i
				     :image-base pos-lists))
	    (n-integers (+ 2 (length more-bools))))))
 :optimizer
  (do-split items (cons bools (copy-list more-bools)) T))

(cl:defun pos (&rest bools)
  (do ((bs bools (cdr bs))
       (i 0 (1+ i)))
      ((null bs) i)
    (if (car bs) (return i))))

(defS split-if (items pred &rest more-pred)
    "Divides a series into multiple outputs based on PRED."
  (cl:let* ((preds (list* pred (copy-list more-pred)))
	      (pos-lists
		(map-fn t #'(lambda (item)
			      (cons (apply #'pos-if item preds) item))
			(promote-series items))))
    (values-list
      (mapcar #'(lambda (i)
		  (make-image-series :alter-fn (alter-fn items)
				     :image-fn #'image-of-with-datum
				     :image-datum i
				     :image-base pos-lists))
	    (n-integers (+ 2 (length more-pred))))))
 :optimizer
  (do-split items (cons pred (copy-list more-pred)) nil))

(cl:defun pos-if (item &rest fns)
  (do ((fs fns (cdr fs))
       (i 0 (1+ i)))
      ((null fs) i)
    (if (cl:funcall (car fs) item) (return i))))

(cl:defun image-of-with-datum (g datum)
  (cl:let (item)
    (loop (setq item (basic-do-next-in g))
	  (if (or (null (gen-state g))
		  (eql (car item) datum))
	      (return (cdr item))))))

(cl:defun do-split (items stuff bools-p)
  (cl:let ((frag (make-frag))
	     (ivar (new-var 'splititems))
	     (D (new-var 'dne)))
    (+arg (make-sym :var ivar :series-var-p T) frag)
    (dotimes (i (length stuff) i)
      (cl:let ((var (new-var 'h))
		 (-X- (new-var '-z-))
		 (S (new-var 'ss)))
	(+arg (make-sym :var var :series-var-p bools-p) frag)
	(+ret (make-sym :var ivar :series-var-p T :off-line-spot -X-) frag)
	(setf (body frag)
	      `(,@(body frag)
		  (if (not ,(if bools-p var `(cl:funcall ,var ,ivar))) (go ,S))
		  ,-X-
		  (go ,D)
	       ,S ))))
    (cl:let ((-X- (new-var '-Y-)))
      (+ret (make-sym :var ivar :series-var-p T :off-line-spot -X-) frag)
      (setf (body frag)
	    `(,@(body frag)
	       ,-X- ,D)))
    (funcall-frag frag (cons items stuff))))

(defS chunk (m n &optional (items nil items-p))
    "Moves a window of width M over ITEMS by step N."
  (progn
    (when (not items-p)        ;it is actually n that is optional
      (setq items n)
      (setq n 1))
    (cond ((not (and (integerp m) (plusp m)))
	   (ers 63 "~%M argument " m " to CHUNK fails to be a positive integer."))
	  ((not (and (integerp n) (plusp n)))
	   (ers 64 "~%N argument " n " to CHUNK fails to be a positive integer."))
	  (T (values-list
	       (mapcar #'(lambda (i)
			   (every-nth m n
				      (if (zerop i) items (previous items nil i))))
		       (nreverse (n-integers m)))))))
 :optimizer
 (progn
   (when (not items-p)        ;it is actually n that is optional
     (setq items n)
     (setq n 1))
   (cond ((not (and (integerp m) (plusp m)))
          (rrs 3 "~%M argument " m " to CHUNK fails to be a positive integer."))
         ((not (and (integerp n) (plusp n)))
          (rrs 4 "~%N argument " n " to CHUNK fails to be a positive integer."))
         (T (cl:let* ((vars (n-gensyms m "CHUNK-"))
			(outs (mapcar #'(lambda (v) (list v t)) vars))
			(auxes (mapcar #'(lambda (v)
					   `(,v ,(copy-list
						   '(series-element-type in))))
				       vars))
			(setqs (mapcar #'(lambda (u v) (list 'setq u v))
				       vars (cdr vars))))
              (funcall-frag
               (literal-frag
                `(((in T -X-)) ,outs ((count fixnum) ,@ auxes) ()
                  ((setq count ,(1- m)))
                  (L -X- ,@ setqs (setq ,(car (last vars)) in)
                     (cond ((plusp count) (decf count) (go L))
                           (T (setq count ,(1- n))))) () ()))
               (list items)))))))

(defS every-nth (m n items)
    "Returns a series of every Nth element of ITEMS, after skipping M elements."
  (fragL ((m) (n) (items T -X-)) ((items T)) ((count fixnum)) ()
	 ((setq count (1- m)))
	 (L -X- (cond ((plusp count) (decf count) (go L))
		      (T (setq count (1- n))))) () ()))

;seq-type must be a subtype of SEQUENCE or BAG.

(defS collect (seq-type &optional (items nil items-p))
    "Collects the elements of a series into a sequence."
  (cl:let (*type* limit el-type)
    (when (not items-p) ;it is actually seq-type that is optional
      (setq items seq-type)
      (setq seq-type (optq 'list)))
    (multiple-value-setq (*type* limit el-type)
      (decode-seq-type (non-optq seq-type)))
    (cond ((eq *type* 'list)
	   (fragL ((items T)) ((lst)) ((lst list)) ()
		  ((setq lst nil))
		  ((setq lst (cons items lst)))
		  ((setq lst (nreverse lst))) ()))
	  ((eq *type* 'bag)
	   (fragL ((items T)) ((lst)) ((lst list)) ()
		  ((setq lst nil))
		  ((setq lst (cons items lst))) () ()))
	  (limit
	   ;; It's good to have the type exactly right so CMUCL can
	   ;; optimize better.
	   (setq *type* (if (consp (cadr seq-type))
			    (cadr seq-type)
			    seq-type))
	   (fragL ((seq-type) (items T) (limit)) ((seq))
		  ((seq *type*) (index fixnum)) ()
		  (
		   ;; For some reason seq isn't initialized when
		   ;; *optimize-series-expressions* is nil and this
		   ;; errors out in CMUCL.  This makes sure seq is
		   ;; initialized to something.
		   (setq seq (if seq
				 seq
				 (make-sequence seq-type limit)))
		   (setq index 0))
		  ((setf (aref seq index) items) (incf index)) () ()))
	  ((not (eq *type* 'sequence)) ;some kind of vector with no length
	   ;; It's good to have the type exactly right so CMUCL can
	   ;; optimize better.
	   (setq *type* (if (eq *type* 'simple-array)
			    (list *type* el-type '(*))
			    (list *type* el-type)))
	   (fragL ((seq-type) (items T)) ((seq))
		  ;;((seq (or null simple-array)) (lst list)) ()
		  ((seq *type*) (lst list)) ()
		  ((setq lst nil))
		  ((setq lst (cons items lst)))
		  ((cl:let ((num (length lst)))
		     (setq seq (make-sequence seq-type num))
		     (do ((i (1- num) (1- i))) ((minusp i))
		       (setf (aref seq i) (pop lst))))) ()))
	  (T (fragL ((seq-type) (items T)) ((seq))
		    ((seq (or null T)) (limit (or null fixnum)) (lst list)) ()
		    ((setq lst nil)
		     (multiple-value-bind (x y)
		         (decode-seq-type (list 'quote seq-type))
			 (declare (ignore x))
		       (setq limit y)))	; y is not restricted to fixnum!
		    ((setq lst (cons items lst)))
		    ((cl:let ((num (length lst)))
		       (setq seq (make-sequence seq-type (or limit num)))
		       (do ((i (1- num) (1- i))) ((minusp i))
			 (setf (elt seq i) (pop lst))))) ()))))
  :trigger T)

(defS collect-append (seq-type &optional (items nil items-p))
    "Appends the elements of ITEMS together into a single list."
  (progn
    (when (not items-p) ;it is actually seq-type that is optional
      (setq items seq-type)
      (setq seq-type (optq 'list)))
    (cond ((equal seq-type (optq 'list))
	   (fragL ((items T)) ((lst)) ((lst list) (list-end list)) ()
		  ((setq list-end nil) (setq lst nil))
		  ((when items
		     (cl:let ((copy (copy-list items)))
		       (if list-end (setf (cdr (last list-end)) copy))
		       (setq list-end copy)
		       (if (null lst) (setq lst copy))))) () ()))
	  (T (fragL ((seq-type) (items T)) ((seq)) ((seq T)) ()
		    ((setq seq nil))
		    ((setq seq (cons items seq)))
		    ((setq seq (apply #'concatenate seq-type (nreverse seq))))
		    ()))))
  :trigger T)

(defS collect-nconc (items)
    "Nconcs the elements of ITEMS together into a single list."
  (fragL ((items T)) ((lst)) ((lst list) (list-end list)) ()
	 ((setq list-end nil) (setq lst nil))
	 ((when items
	    (if list-end (setf (cdr (last list-end)) items))
	    (setq list-end items)
	    (if (null lst) (setq lst items)))) () ())
 :trigger T)

(defS collect-hash (keys values &rest option-plist)
   "Combines a series of keys and a series of values together into a hash table."
  (fragL ((keys T) (values T) (option-plist)) ((table)) ((table T)) ()
	 ((setq table (apply #'make-hash-table option-plist)))
	 ((setf (gethash keys table) values)) () ())
 :optimizer
  (funcall-literal-frag
    (list '(((keys T) (values T) (table)) ((table)) () ()
	    () ((setf (gethash keys table) values)) () ())
	  keys values `(make-hash-table ,@ option-plist)))
 :trigger T)

(defS collect-file (name items &optional (printer #'print))
    "Prints the elements of ITEMS into a file."
  (fragL ((name) (items T) (printer)) ((out)) ((out (or null T)) (lst list)) ()
	 ((setq lst nil) (setq out T))
	 ((setq lst (cons items lst)))
	 ((setq lst (nreverse lst))
	  (with-open-file (f name :direction :output)
	    (dolist (item lst)
	      (cl:funcall printer item f)))) ())
 :optimizer
  (funcall-literal-frag
    (cl:let ((file (new-var 'outfile)))
      `((((items T) (printer)) ((out)) ((out (or null T))) ()
	 ((setq out T)) ((cl:funcall printer items ,file)) ()
	 (#'(lambda (c)
	      (list 'with-open-file '(,file ,name :direction :output) c))))
	,items ,printer)))
 :trigger T)

(defS collect-alist (keys values)
    "Combines a series of keys and a series of values together into an alist."
  (fragL ((keys T) (values T)) ((alist)) ((alist list)) ()
	 ((setq alist nil))
	 ((setq alist (cons (cons keys values) alist)))
	 () ())
 :trigger T)

(defS collect-plist (indicators values)
   "Combines a series of indicators and a series of values together into a plist."
  (fragL ((indicators T) (values T)) ((plist)) ((plist list)) ()
	 ((setq plist nil))
	 ((setq plist (list* indicators values plist)))
	 () ())
 :trigger T)

(defS collect-last (items &optional (default nil))
    "Returns the last element of ITEMS."
  (fragL ((items T) (default)) ((item)) ((item t)) ()
	 ((setq item default)) ((setq item items)) () ())
 :trigger T)

(defS collect-first (items &optional (default nil))
    "Returns the first element of ITEMS."
  (fragL ((items T) (default)) ((item)) ((item t)) ()
	 ((setq item default))
	 ((setq item items) (go END)) () ())
 :trigger T)

(defS collect-nth (n items &optional (default nil))
    "Returns the nth element of ITEMS."
  (fragL ((n) (items T) (default)) ((item))
	 ((counter fixnum) (item t)) ()
	 ((setq item default) (setq counter n))
	 ((when (zerop counter) (setq item items) (go END))
	  (decf counter)) () ())
 :trigger T)

(defS collect-and (bools)
    "Computes the AND of the elements of BOOLS."
  (fragL ((bools T)) ((bool)) ((bool T)) ()
	 ((setq bool T)) ((if (null (setq bool bools)) (go END))) () ())
 :trigger T)

(defS collect-or (bools)
    "Computes the OR of the elements of BOOLS."
  (fragL ((bools T)) ((bool)) ((bool T)) ()
	 ((setq bool nil)) ((if (setq bool bools) (go END))) () ())
 :trigger T)

(defS collect-length (items) "Returns the number of elements in ITEMS."
  (fragL ((items T)) ((number)) ((number fixnum)) ()
	 ((setq number 0)) ((incf number)) () ())
 :trigger T)

(defS collect-sum (numbers &optional (type 'number))
    "Computes the sum of the elements in NUMBERS."
  (fragL ((numbers T) (type)) ((sum)) ((sum T)) ()
	 ((setq sum (coerce 0 type)))
	 ((setq sum (+ sum numbers))) () ())
 :optimizer
  (funcall-literal-frag
    `((((numbers T)) ((sum)) ((sum ,(must-be-quoted type))) ()
       ((setq sum ,(coerce 0 (must-be-quoted type))))
       ((setq sum (+ sum numbers))) () ())
      ,numbers))
 :trigger T)

(defS collect-max (numbers &optional (items numbers items-p) (default nil))
    "Returns the ITEM corresponding to the maximum NUMBER."
  (if items-p
      (fragL ((numbers T) (items T) (default)) ((result))
	     ((number T) (result (series-element-type items))) ()
	     ((setq number nil))
	     ((if (or (null number) (< number numbers))
		  (setq number numbers result items)))
	     ((if (null number) (setq result default))) ())
      (fragL ((numbers T) (default)) ((number))
	     ((number T)) ()
	     ((setq number nil))
	     ((if (or (null number) (< number numbers)) (setq number numbers)))
	     ((if (null number) (setq number default))) ()))
 :trigger T)

(defS collect-min (numbers &optional (items numbers items-p) (default nil))
    "Returns the ITEM corresponding to the minimum NUMBER."
  (if items-p
      (fragL ((numbers T) (items T) (default)) ((result))
	     ((number T) (result (series-element-type items))) ()
	     ((setq number nil))
	     ((if (or (null number) (> number numbers))
		  (setq number numbers result items)))
	     ((if (null number) (setq result default))) ())
      (fragL ((numbers T) (default)) ((number))
	     ((number T)) ()
	     ((setq number nil))
	     ((if (or (null number) (> number numbers)) (setq number numbers)))
	     ((if (null number) (setq number default))) ()))
 :trigger T)

;A note on types.  Things are set up so that every aux variable (except the
;variable that is necessary when a non-series input is not a constant) is given a
;type declaration whereas inputs are never given types.  This ensures that
;everything is given a type definition and only once.  The only exception is
;that a user can use a type decl in a series::let which will then override the
;default type.  You can also wrap a (THE TYPE ...) around any series function call
;to override the types of the output.
;  Some types are given in the form (series-element-type var) where var
;is a series input.  A final pass substitutes this type if it
;can be found.  Note that this is a purely one-way propagation of information
;starting on the inputs.
;  The final pass also discards any type declarations which are T.

;------------------------------------------------------------

;some things added since the last documentation
; #nM for returning multiple values
; Note #M does odd stuff with the keywords for keyword arguments, but
;   there is nothing we can do about this, because the documentation is very 
;   clear on what #M does.  If your are using implicit mapping,
;   #M is unnecessary anyway.
; *series-implicit-map*, note the detailed rules for when mapping
;   happens, which are much like OSS was, but more conservative.
;   We never map a function unless we MUST---i.e., only when one of
;   its actual arguments is a series.  We never map a special form except IF.
;   The virtue of this is that it is applicable on a single function
;   by single function basis, and gets the same results no matter what
;   the input looks like syntactically.  (Note you might not get portable
;   resuls. if some standard macro expands into code with an IF in one
;   implementation and without in another.)  (Note the forced evaluation
;   of non-series functions that are not last in a let etc. is already done.)

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

;-------------------------------------------------------------------------

#+nil ;; Don't think I really want to do this
(eval-when (load)
  (in-package "SERIES")
  (install :pkg "COMMON-LISP-USER")
  (in-package "COMMON-LISP-USER"))
