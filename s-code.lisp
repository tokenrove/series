;-*- Mode: lisp; syntax:ANSI-COMMON-LISP; Package: (SERIES :use "COMMON-LISP" :colon-mode :external) -*-

;;;; The standard version of this program is available from
;;;;
;;;; http://series.sourceforge.net/
;;;;
;;;; If you obtained this file from somewhere else, or copied the
;;;; files a long time ago, you might consider copying them from the
;;;; above web site now to obtain the latest version.
;;;; NO PATCHES TO OTHER BUT THE LATEST VERSION WILL BE ACCEPTED.
;;;;
;;;; $Id: s-code.lisp,v 1.107 2008/10/27 14:24:53 rtoy Exp $
;;;;
;;;; This is Richard C. Waters' Series package.
;;;; This started from his November 26, 1991 version.
;;;;
;;;; $Log: s-code.lisp,v $
;;;; Revision 1.107  2008/10/27 14:24:53  rtoy
;;;; Support SCL.  Just add scl conditionalizations where we have cmucl
;;;; ones, and convert uppercase symbols and symbol-names to use
;;;; symbol-name and uninterned symbols.  This is to support scl's default
;;;; "modern" mode.
;;;;
;;;; Changes from Stelian Ionescu.
;;;;
;;;; Revision 1.106  2007/08/08 15:07:45  rtoy
;;;; Change default test for SCAN-ALIST to EQL instead of EQ.
;;;;
;;;; Revision 1.105  2007/08/08 13:36:56  rtoy
;;;; s-code.lisp:
;;;; o Update docstrings
;;;;
;;;; s-doc.txt:
;;;; o Add documentation for COLLECT-PRODUCT, COLLECT-STREAM
;;;; o Correct the documentation for COLLECT-MAX and COLLECT-MIN.
;;;;
;;;; Revision 1.104  2007/08/08 03:42:43  rtoy
;;;; s-code.lisp:
;;;; o Update docstrings with more descriptive strings.
;;;;
;;;; s-doc.txt:
;;;; o Document SCAN-STREAM.
;;;;
;;;; Revision 1.103  2007/07/31 21:14:11  rtoy
;;;; Make the #Z reader signal an error if we are trying to create an
;;;; infinite literal series.  Series doesn't support that.
;;;;
;;;; Revision 1.102  2007/07/10 17:45:46  rtoy
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
;;;; Revision 1.101  2007/02/06 21:10:38  rtoy
;;;; Get rid of a warning message.  Don't know why the warning is done at
;;;; all.
;;;;
;;;; Revision 1.100  2005/12/13 14:40:30  rtoy
;;;; Lispworks wants an eval-when around coerce-maybe-fold.  From Chris
;;;; Dean, 2005/12/09.
;;;;
;;;; Revision 1.99  2005/11/15 15:07:57  rtoy
;;;; ANSI CL says a declaration cannot also be the name of a type, so
;;;; remove the declaration for SERIES.
;;;;
;;;; Revision 1.98  2005/01/27 04:19:33  rtoy
;;;; Fix for bug 434120.
;;;;
;;;; s-code.lisp:
;;;; o scan* should initialize the index to -1 instead of 0, to keep in
;;;;   step with scan.
;;;;
;;;; s-test.lisp:
;;;; o Add test from the bug report.
;;;;
;;;; Revision 1.97  2005/01/26 18:37:34  rtoy
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
;;;; Revision 1.96  2004/12/15 17:18:53  rtoy
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
;;;; Revision 1.95  2003/06/08 12:52:40  rtoy
;;;; From Alexey Dejneka:
;;;;
;;;; o Add support for SBCL
;;;; o Fix a missing initialization of temp.
;;;;
;;;; Revision 1.94  2003/01/21 20:12:40  rtoy
;;;; Add support for CMUCL 18e which no longer has
;;;; pcl::walk-form-macroexpand.  It's walker::macroexpand-all.
;;;;
;;;; Revision 1.93  2002/12/12 04:27:41  rtoy
;;;; Add support for a macrolet code-walker for Clisp.
;;;;
;;;; Revision 1.92  2002/12/11 04:03:26  rtoy
;;;; o Update /allowed-generic-opts/ to include SYSTEM::READ-ONLY for CLISP
;;;;   2.29.
;;;; o Modify COMPUTE-SERIES-MACFORM-1 and COMPUTE-SERIES-MACFORM-2 so that
;;;;   CMUCL doesn't try to dump functions directly to a FASL file.  Fixes
;;;;   bug 498418: cmucl doesn't like dumping functions.
;;;;
;;;; Revision 1.91  2002/12/10 19:36:32  rtoy
;;;; Previous patch failed some tests.  Let's try this
;;;; again. PROMOTE-SERIES returns an extra arg telling us what it did.  We
;;;; use that to decide if we want the car or not of the item.
;;;;
;;;; Revision 1.90  2002/12/10 17:55:46  rtoy
;;;; Bug [ 516952 ] only optimized split-if works
;;;;
;;;; A gross hack to fix this has been applied.  The wrong things were
;;;; passed to pos-if in some situations.
;;;;
;;;; Revision 1.89  2002/06/03 17:53:14  rtoy
;;;; From Joe Marshall:
;;;;
;;;;     I found a bug in `scan-fn-opt' that caused an unbound variable
;;;;     when the initialization thunk in scan-fn refers to a lexical
;;;;     variable, and there is a test function.
;;;;
;;;;     The existing code calls `handle-fn-call' to invoke the thunks
;;;;     for scanning.  handle-fn-call keeps track of free variable references.
;;;;     When calling it the last time, you pass in T as the last argument.
;;;;
;;;;     In the case where there was a test expression, however, the
;;;;     order of calling handle-fn-call changes making the *second* to last
;;;;     call have the T argument, rather than the last.  By re-ordering the
;;;;     way scan-fn-opt expands the thunks, this is fixed.
;;;;
;;;; Revision 1.88  2002/03/29 23:53:38  rtoy
;;;; Should not macroexpand declarations?  I think this is right.  I think
;;;; I did it right, but needs more testing.
;;;;
;;;; Revision 1.87  2001/12/23 16:54:44  rtoy
;;;; Make series support Allegro "modern" lisp with its case-sensitive
;;;; reader.  Mostly just making every that needs to be lower case actually
;;;; lower case.  The tests still work.
;;;;
;;;; Revision 1.86  2001/08/31 15:51:54  rtoy
;;;; Some changes from Joe Marshall for Allegro which apparently doesn't
;;;; fold constants in coerce.  These changes only apply to Allegro.
;;;;
;;;; Revision 1.85  2001/04/10 17:22:33  rtoy
;;;; o Change series printer to output items one at a time instead of
;;;;   gathering up everything before printing.
;;;; o Add more detailed doc strings for some functions.
;;;;
;;;; Revision 1.84  2001/04/09 22:18:47  rtoy
;;;; o Random re-indents so I can read the code better
;;;; o The latest versions of CMUCL's PCL have a better code walker that
;;;;   allows us to do a macroexpand, ala lispworks.
;;;; o For CMUCL, use its list pretty-printer for printing out series.
;;;;   It looks much better.  But may be a problem if the series is
;;;;   infinite.  We won't get any output at all.  (I think the original
;;;;   would produce output and just never stop.)
;;;;
;;;; Revision 1.83  2001/04/09 19:52:34  rtoy
;;;; Stupid typo commenting (too much) stuff out.
;;;;
;;;; Revision 1.82  2001/04/07 20:14:31  rtoy
;;;; o remove-aux-if was inadvertently defined twice (should have been
;;;;   remove-aux-if-not)
;;;; o remove-aux-if and remove-aux-if-not don't appear to be used
;;;;   anywhere, so comment them out for now.  Remember to remove them
;;;;   later.
;;;;
;;;; Revision 1.81  2001/04/07 15:35:51  rtoy
;;;; scan-stream didn't work when not optimized, due to a typo.  The core
;;;; of scan-stream should now be identical to the core of scan-file.
;;;;
;;;; Revision 1.80  2000/10/10 21:03:12  rtoy
;;;; Oops.  It's cl:let, not just plain let.
;;;;
;;;; Revision 1.79  2000/10/10 15:02:27  rtoy
;;;; Fix up the lifting code to handle all variables except #:SEQ.
;;;; (There's some code in collect that initializes a SEQ var with (if SEQ
;;;; SEQ <do something else>), which I can't lift up because then SEQ would
;;;; be undefined.)
;;;;
;;;; Change the default for *lift-out-vars-p* to be T.
;;;;
;;;; Revision 1.78  2000/10/07 20:02:10  rtoy
;;;; Comment and clean up code added in previous update.
;;;;
;;;; Revision 1.77  2000/10/06 23:03:01  rtoy
;;;; First cut at trying to lift some variable initializations into the
;;;; enclosing LET.
;;;;
;;;; Basically, we look for something like
;;;;
;;;; (let (out-1 out-2)
;;;;   (setq out-1 <init-1>)
;;;;   (setq out-2 <init-2>)
;;;;   <stuff>)
;;;;
;;;; and try to convert that to
;;;;
;;;; (let ((out-1 <init-1>) (out-2 <init-2>))
;;;;   <stuff>)
;;;;
;;;; right after series has completed all of the macroexpansions it wants.
;;;;
;;;; Because this may be buggy, you can enable this feature by setting
;;;; *lift-out-vars-p* to T.  It defaults to NIL.
;;;;
;;;; Note:  this can cause CMUCL sometimes to produce a compile warning
;;;; that constant folding failed.  (Often caused by trying to compute
;;;; array-total-size of a known constant list.)
;;;;
;;;; Revision 1.76  2000/10/01 23:07:28  rtoy
;;;; o Add some comments.
;;;; o Add a template for LOCALLY.  (MCL works now!!!!)
;;;; o Move the OPTIF macro before it's first use in EOPTIF-Q. Seems that
;;;;   this is required according to the CLHS.  (Noticed by Rainer Joswig.)
;;;;
;;;; Thanks to Rainer for testing this on MCL.  MCL passes all of the
;;;; tests!
;;;;
;;;; Revision 1.75  2000/09/30 21:44:41  rtoy
;;;; Bug #115738:
;;;;
;;;; Use remove-if-not instead of delete-if-not in delete-aux-if-not.  This
;;;; was causing CLISP to fail test 530.
;;;;
;;;; (I'm not sure about this.  It seems there's some shared list structure
;;;; with CLISP that doesn't happen in CMUCL.  However, I think it's safe
;;;; to cons up a new list instead of destructively modifying the
;;;; original.)
;;;;
;;;; Revision 1.74  2000/09/05 15:54:09  rtoy
;;;; Fix bug 113625:  scan doesn't scan constants very well.
;;;;
;;;; Solution: If it's a symbol, take the value of the symbol.  (Not sure
;;;; this is quite correct, but it works and the other tests pass without
;;;; problems.)
;;;;
;;;; Revision 1.73  2000/06/26 18:11:26  rtoy
;;;; Fix for bug #108331: collect 'vector sometimes returns results in
;;;; reverse order.  Example is (collect 'vector (scan '(1 2 3))).
;;;;
;;;; Revision 1.72  2000/06/26 15:28:19  rtoy
;;;; DECODE-SEQ-TYPE was getting BASE-STRING and STRING mashed together,
;;;; and didn't even handle BASE-STRING.  They are slightly different:
;;;; BASE-STRING is composed of BASE-CHAR's and STRING is composed of
;;;; CHARACTER's.
;;;;
;;;; Revision 1.71  2000/03/28 10:23:49  matomira
;;;; polycall et all are now tail recursive.
;;;; LETIFICATION WORKS COMPLETELY!!
;;;;
;;;; Revision 1.86  2000/03/28 10:19:04  matomira
;;;; polycall et al. are now tail recursive.
;;;; LETIFICATION WORKS COMPLETELY!
;;;;
;;;; Revision 1.85  2000/03/27 17:21:14  matomira
;;;; Fixed eval-on-first-cycle for letification.
;;;; Improved clean-code so it does not miss completely unused variables.
;;;;
;;;; Revision 1.83  2000/03/25 21:44:26  matomira
;;;; Avoided gratuitous consig in values-lists.
;;;;
;;;; Revision 1.80  2000/03/23 23:01:56  matomira
;;;;   NEW FEATURES:
;;;;   ------------
;;;;    - (collect 'set
;;;;      Collects a series into a list removing any duplicates in the most efficient way possible.
;;;;    - (collect 'ordered-set
;;;;      Collects a series into a list removing any duplicates but keeping the original series order.
;;;;    - SCAN now allows to drop the type specifier for any source expression
;;;;      [:cltl2-series reactivates the old 'list assumption]
;;;;    - SCAN now can scan multidimensional arrays in row-major order.
;;;;
;;;;   IMPROVEMENTS:
;;;;   ------------
;;;;    - Better code generation
;;;;      . Some fixnum declarations were further constrained.
;;;;      . Optimized scanning of constant sequences.
;;;;      . Somewhat optimized scanning of "empty" vectors, ie,
;;;;        declared to be of constant 0 length, like in
;;;;        (collect (scan '(vector t 0) <gimme-a-huge-array-to-throw-away>)
;;;;        now gives you NIL generating/executing less instructions.
;;;;        [<gimme-a-huge-array-to-throw-away> is still executed if not constantp,
;;;;         though]
;;;;      . Variables of type NULL are replaced by constant NILs.
;;;;
;;;;   BUG FIXES:
;;;;   ---------
;;;;    - Some incorrect fixnum declarations were relaxed.
;;;;    - Improved some declarations to avoid spurious range warnings regarding
;;;;      dead code by not-so-smart compilers.
;;;;
;;;; Revision 1.79  2000/03/21 17:18:56  matomira
;;;; Reinstated plain generation support.
;;;;
;;;; Revision 1.78  2000/03/21 15:26:12  matomira
;;;; Fixed letified merge-frags bug.
;;;; Adapted handle-dflow and non-series-merge for letification.
;;;; Spawned list->frag1 from list->frag.
;;;; define-optimizable-series-function uses list->frag1 to support letification.
;;;; Still can't handle all initial bindings because off-line handling seems to
;;;; move prologs into TAGBODYs.
;;;;
;;;; Revision 1.75  2000/03/18 20:12:52  matomira
;;;; Improved code generated by compute-series-macform-2 when trigger is t.
;;;;
;;;; Revision 1.74  2000/03/18 19:14:45  matomira
;;;; Improved merging when letified.
;;;; Last version with series library definitions not requiring letification.
;;;;
;;;; Revision 1.73  2000/03/18 18:05:24  matomira
;;;; Full letification works.
;;;;
;;;; Revision 1.72  2000/03/17 19:24:23  matomira
;;;; MERGE-FRAGS no longer depends on frag component order.
;;;; purity component of frag is now just a symbol.
;;;; Abstracted use of prolog component of frags.
;;;; Prolog letification almost works. Need to adapt MERGE-FRAGS still.
;;;;
;;;; Revision 1.71  2000/03/15 18:40:35  matomira
;;;; LOCALLY and letification works.
;;;;
;;;; Revision 1.70  2000/03/15 09:05:39  matomira
;;;; Temporary NULL-OR wrap for some declarations.
;;;;
;;;; Revision 1.69  2000/03/14 10:48:09  matomira
;;;; Workaround for ACL 5.0.1 TAGBODY bug added.
;;;; ALL-TIME SERIES BUG FIX: wrappers now inserted more precisely.
;;;; Abstracted use of wrapper component of frags.
;;;; GENERATOR deftyped to CONS, not LIST, when necessary.
;;;;
;;;; Revision 1.68  2000/03/11 17:36:33  matomira
;;;; Added eval-when compatibility magic.
;;;;
;;;; Revision 1.67  2000/03/11 15:35:44  matomira
;;;; Fixed worsen-purity.
;;;;
;;;; Revision 1.66  2000/03/10 12:49:27  matomira
;;;; Letification works.
;;;; Started purity analysis.
;;;;
;;;; Revision 1.65  2000/03/09 13:28:03  matomira
;;;; Almost there with letification.
;;;; Activated GENERATOR deftype also for :excl.
;;;;
;;;; Revision 1.64  2000/03/08 18:20:35  matomira
;;;; Fixed fragL instead of *fragL bug in COLLECT.
;;;;
;;;; Revision 1.63  2000/03/08 17:58:24  matomira
;;;; Fixed mixed CL: before FUNCALL in DESTARRIFY.
;;;;
;;;; Revision 1.62  2000/03/08 12:30:53  matomira
;;;; Continued work on letification.
;;;;
;;;; Revision 1.61  2000/03/07 13:47:23  matomira
;;;; Removed gratuitous sorting in CODIFY.
;;;;
;;;; Revision 1.60  2000/03/07 08:54:20  matomira
;;;; Abstracted all uses of a frag's aux component.
;;;;
;;;; Revision 1.59  2000/03/06 18:24:35  matomira
;;;; Replaced IF by WHEN in non-output code when possible.
;;;; Abstracted use of aux frag field.
;;;;
;;;; Revision 1.58  2000/03/06 12:33:14  matomira
;;;; Simplified inserted aux var initialization.
;;;;
;;;; Revision 1.57  2000/03/06 12:11:53  matomira
;;;; Fixed declaration handling in GATHERING.
;;;;
;;;; Revision 1.56  2000/03/05 16:21:56  matomira
;;;; Fixed missing CL: before FUNCALL bug.
;;;; Removed NULL-ORs by using THE.
;;;; Renamed old fragL as *fragL.
;;;; New fragL does not do *type* substitution.
;;;;
;;;; Revision 1.55  2000/03/03 19:17:14  matomira
;;;; Series 2.0 - Change details in RELEASE-NOTES.
;;;;
;;;; Revision 1.51  2000/02/23 15:27:02  toy
;;;; o Fernando added an indefinite-extent declaration and uses
;;;;   it in the one place where it's needed.
;;;; o Fernando renamed split-assignment to detangle2 and
;;;;   corrected some bugs in my version.
;;;;
;;;; Revision 1.50  2000/02/22 23:37:22  toy
;;;; Remove the cmu version from scan-range.  It was generating bad
;;;; initialization code for things like (scan-range :length 10 :type
;;;; 'single-float).
;;;;
;;;; Revision 1.49  2000/02/22 22:25:51  toy
;;;; o One of Fernando's uses of dynamic-extent was wrong, as Fernando
;;;;   points out.
;;;;
;;;; o CLISP apparently has a bug in loop such that split-assignment is
;;;;   broken.  Replace that with an equivalent do loop.
;;;;
;;;; Revision 1.48  2000/02/22 15:21:38  toy
;;;; Fernando added dynamic-extent declarations wherever needed.
;;;;
;;;; Revision 1.47  2000/02/11 14:45:42  toy
;;;; Let's not use fix-types for CMU in optimize-producing.  This means the
;;;; compiler can't optimize things as well as it could, and I (RLT) want
;;;; to see these warnings.
;;;;
;;;; Revision 1.46  2000/02/10 17:15:11  toy
;;;; Fix a typo that got in the last few patches:  LET should really be
;;;; CL:LET.  (From Fernando.)
;;;;
;;;; Revision 1.45  2000/02/09 22:46:00  toy
;;;; Changed all occurrences of defunique to be just defun and added a
;;;; comment on where the function is called.
;;;;
;;;; Revision 1.44  2000/02/08 17:08:36  toy
;;;; o As discussed with Fernando, the "optional" type is renamed to
;;;;   null-or.
;;;; o Cleaned up and indented some of the comments.
;;;;
;;;; Revision 1.43  2000/02/04 23:05:57  toy
;;;; A few more changes from Fernando:
;;;;
;;;; o All functions called from a single site are defined with DEFUNIQUE
;;;;   for documentation purposes (and eventual inlining via DEFEMBEDDED).
;;;; o Fixed the long standing bug that install uninterned everything that
;;;;   it didn't like.  Shadowing import is used now.
;;;;
;;;; There were some other changes that I (RLT) don't understand.
;;;;
;;;; Revision 1.41  2000/02/04 16:34:30  toy
;;;; o  Some more changes from Fernando.  This fixes some bugs that show up
;;;;    in the test suite.  The test suite now passes on CMUCL.
;;;;
;;;; o  Fixed up some declarations that used the optional type when, in
;;;;    fact, it didn't.  (Hope I got these all right.)
;;;;
;;;; Revision 1.40  2000/02/03 17:30:08  toy
;;;; Two major fixes:
;;;;
;;;; o Bug in collect (missing set of parens)
;;;; o Change some defconstants back to defvar.  (Tickles CMUCL inf loop).
;;;;
;;;; Revision 1.39  2000/02/02 21:31:44  toy
;;;; Here are the changes that Fernando made.  I (RLT) don't claim to
;;;; understand everything that was changed or why.
;;;;
;;;; 1. Removed 1 redundant eval-when
;;;; 2. Sorted functions so that it can be loaded w/o `undefined function'
;;;;    warnings.
;;;; 3. Added inline declarations for all functions called from a single
;;;;    site.
;;;; 4. Sorted functions so that inlining will work even if a compiler does
;;;;    not inline forward references to functions defined in the same
;;;;    file.
;;;; 5. Setup eval-when's so that it can be compiled w/o having to load the
;;;;    source first.
;;;; 6. Simplified redundant (OR NULL T) to T
;;;; 7. Simplified redundant #'(lambda (x) (foo x)) to #'foo where foo is a
;;;;    function.
;;;; 8. Fixed so it won't complain when LispWorks adds
;;;;    CLOS::VARIABLE-REBINDING declarations after CLOS macro
;;;;    transformations (general support provided via the constant
;;;;    allowed-generic-opts).
;;;; 9. Added support for MACROLET on LispWorks (necessary because of CLOS
;;;;    macro transformations).
;;;; 10. Only declare variables as (OR foo NULL) on implementations that
;;;;     won't allow to store NIL otherwise (currently, only CMUCL).
;;;; 11. Added specialization to series declarations (eg: (SERIES FIXNUM)).
;;;; 12. Do more precise type propagation in PRODUCING forms.
;;;; 13. Allow `SETF like SETQ' in PRODUCING forms.
;;;; 14. Added COLLECT-PRODUCT.
;;;; 15. Extended SETQ-P to take into account multiassignments (not used
;;;;     yet). This should still be trivially generalized to support PSETQ
;;;;     and SETF, BTW.
;;;; 16. Added DEFTYPE for GENERATOR so that LispWorks and CMUCL won't
;;;;     complain "because it's not a list" (IT IS!!)
;;;; 17. Replaced PROCLAIMS with DECLAIMS.
;;;; 18. Replaced DEFVARs with DEFCONSTANTs where appropriate.
;;;; 19. Removed function namespace pollution by defS-generated code.
;;;;
;;;; Revision 1.38  2000/01/20 18:19:21  toy
;;;; Merged 1.32.2.2 (Fernando's changes) with 1.37.
;;;; I hope I got this right.
;;;;
;;;; Revision 1.32.2.2  2000/01/20 18:05:26  toy
;;;; Merged the changes between 1.32 and 1.37 into this revision.
;;;; This should merge my changes with Fernando's.
;;;;
;;;; Revision 1.32.2.1  2000/01/20 17:51:45  toy
;;;; Checking in the changes from Fernando Mato Mira <matomira@iname.com>
;;;; with the hope of merging our two versions together.
;;;;
;;;; Revision 1.32  1999/07/02 20:37:39  toy
;;;; Moved the package stuff out to a separate file.
;;;;
;;;; Revision 1.31  1999/07/02 15:09:49  toy
;;;; o  Need explicit package qualifier for multiple-value-bind in
;;;;    init-elem.
;;;; o  Reordered some of the tests in init-elem.
;;;;
;;;; Revision 1.30  1999/07/01 16:44:23  toy
;;;; A comment was in the wrong place.
;;;;
;;;; Revision 1.29  1999/07/01 14:15:39  toy
;;;; o  "Pekka P. Pirinen" <pekka@harlequin.co.uk> supplied a new version
;;;;    of aux-init (and init-elem).  This is probably better.  I added one
;;;;    additional case.
;;;;
;;;; o  Added simple-base-string to the tests in decode-seq-type.  (Needed
;;;;    by the new aux-init.
;;;;
;;;; Revision 1.28  1999/06/30 19:34:49  toy
;;;; "Pekka P. Pirinen" <pekka@harlequin.co.uk> says:
;;;;
;;;;    "Tests 289, 290, 417, and 426 [on Liquid CL] fail because of
;;;;    incorrect type decls generated in ADD-PHYSICAL-OUT-INTERFACE.
;;;;    The variable NEW-OUT is used for two conflicting purposes; The
;;;;    fix is to split it into two."
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

;;;; Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;;;; Permission to use, copy, modify, and distribute this software and
;;;; its documentation for any purpose and without fee is hereby
;;;; granted, provided that this copyright and permission notice
;;;; appear in all copies and supporting documentation, and that the
;;;; name of M.I.T. not be used in advertising or publicity pertaining
;;;; to distribution of the software without specific, written prior
;;;; permission. M.I.T. makes no representations about the suitability
;;;; of this software for any purpose.  It is provided "as is" without
;;;; express or implied warranty.

;;;;     M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
;;;;     INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;;;     FITNESS, IN NO EVENT SHALL M.I.T. BE LIABLE FOR ANY SPECIAL,
;;;;     INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
;;;;     RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;;     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;;     ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
;;;;     OF THIS SOFTWARE.

;;;;------------------------------------------------------------------------

;;;; This file implements efficient computation with series
;;;; expressions in Common Lisp.  The functions in this file
;;;; are documented in Appendices A and B of Common Lisp: the Language,
;;;; Second Edition, Guy L. Steele Jr, Digital press, 1990,
;;;; and in even greater detail in
;;;;   MIT/AIM-1082 and MIT/AIM-1083 both dated December 1989
;;;; These reports can be obtained by writing to:
;;;;
;;;;                Publications
;;;;                MIT AI Laboratory
;;;;                545 Tech. Sq.
;;;;                Cambridge MA 02139

;;;; This file attempts to be as compatible with standard Common Lisp
;;;; as possible. It has been tested on the following Common Lisps to
;;;; date (1/18/89).
;;;;
;;;;   Symbolics CL version 8.
;;;;   LUCID CL version 3.0.2 on a sun.
;;;;   Allegro CL version 1.2.1 on a Macintosh.
;;;;   LispWorks CL version 2.1.
;;;;
;;;; This version has been tested on
;;;;
;;;;   CMUCL 18b
;;;;   Lispworks CL
;;;;   Allegro CL 5.0
;;;;
;;;; The companion file "STEST.LISP" contains several hundred tests.
;;;; You should run these tests after the first time you compile this
;;;; file on a new system.
;;;;
;;;; The companion file "SDOC.TXT" contains brief documentation.

#+(and series-ansi)
(in-package :series)

#-(or series-ansi)
(eval-when (compile load eval)
  (in-package "SERIES")
) ; end of eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defvar *suppress-series-warnings* nil
  "Suppress warnings when the restrictions are violated.")

(defvar *series-expression-cache* t
  "Avoids multiple expansions")

(defvar *last-series-loop* nil
  "Loop most recently created by SERIES.")

(defvar *last-series-error* nil
  "Info about error found most recently by SERIES.")

;(pushnew :series-plain *features*)

;;; END OF TUNABLES

(defmacro defconst-once (name value &optional documentation)
  "Like `defconstant' except that if the variable already has a
value, the old value is not clobbered."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when documentation (list documentation))))


;; SERIES::INSTALL changes this
(defvar *series-implicit-map* nil 
  "T enables implicit mapping in optimized expressions")

(defconst-once /ext-conflicts/
  #+(or cmu scl) '(collect iterate)
  #+allegro-v6.1 '(until)
  #-(or cmu scl allegro-v6.1) '())


(defconst-once /series-forms/
  '(let let* multiple-value-bind funcall defun)
  "Forms redefined by Series.")

#+:gcl
(declaim (declaration dynamic-extent)) ; Man, is GCL broken!

(declaim (declaration indefinite-extent))

(declaim (declaration optimizable-series-function off-line-port
                      propagate-alterability))
) ; end of eval-when


;;; Generic stuff that should be moved to/imported from EXTENSIONS

;; mkant EXTENSIONS should push :EXTENSIONS in *FEATURES*!!!

(cl:defun atom-or-car (x)
  (if (listp x)
      (car x)
    x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype nonnegative-fixnum ()
    `(mod ,most-positive-fixnum))
  (deftype nonnegative-integer ()
    `(integer 0))
  (deftype positive-integer ()
    `(integer 1))
  (deftype -integer  (m &optional (n nil) (under 1))
    `(integer ,(- m under) ,@(when n `(,n))))
  (deftype integer+  (m &optional (n nil) (over 1))
    `(integer ,m ,@(when n `(,(+ n over)))))
  (deftype integer-  (m &optional (n nil) (over 1))
    `(integer ,m ,@(when n `(,(- n over)))))
  (deftype -integer- (m &optional (n nil) (over 1) (under 1))
    `(integer ,(- m under) ,@(when n `(,(- n over)))))
  (deftype mod+ (n &optional (over 1))
    `(mod ,(+ n over)))
  (deftype null-or (&rest types)
    `(or null ,@types))
  (deftype uninitialized (typ)
    `(null-or ,typ))
  (deftype defaulted (typ)
    `(null-or ,typ))
  (deftype -vector-index  ()
    `(-integer- 0 ,array-total-size-limit))
  (deftype vector-index   ()
    `(integer-  0 ,array-total-size-limit))
  (deftype vector-index+  ()
    `(integer   0 ,array-total-size-limit)) 
  (deftype -vector-index+ ()
    `(-integer  0 ,array-total-size-limit))
    
#-:extensions
(progn
(defmacro when-bind ((symbol predicate) &body body)
  "Binds the symbol to predicate and executes body only if predicate
   is non-nil."
  `(cl:let ((,symbol ,predicate))
     (when ,symbol
       ,@body)))

(defmacro bind-if ((symbol predicate) then &optional else)
  "Binds the symbol to predicate and executes body only if predicate
   is non-nil."
  `(cl:let ((,symbol ,predicate))
     (if ,symbol
	 ,then
       ,@(when else `(,else)))))

(defmacro bind-if* ((symbol predicate) then &body else)
  "Binds the symbol to predicate and executes body only if predicate
   is non-nil."
  `(cl:let ((,symbol ,predicate))
     (if ,symbol
	 ,then
       ,@(when else
	   `(,(if (cdr else)
		  `(progn ,@else)
		else))))))
) ; end of progn

;; DEBUG
  (defmacro definline (&rest args) `(cl:defun ,@args))

  ;; Define an inline function
  ;; Comment out the #+:ignore line to inline stuff
  #+:ignore
  (defmacro definline (name &rest args)
    `(progn
       (declaim (inline ,name))
       (cl:defun ,name ,@args)))

  (cl:defun eq-car (thing item)
    (and (consp thing) (eq (car thing) item)))

  (cl:defun copy-list-last (orig)
    (if orig
	(cl:let* ((lastcons (list nil))
		  (lst lastcons))
	  (do ((remains orig (cdr remains)))
	      ((not (consp remains)) (values (cdr lst) (rplacd lastcons remains)))
	    (setq lastcons (setf (cdr  lastcons) (cons (car remains) nil)))))
      (values nil nil)))

  (cl:defun nmerge-1 (l1 l2)
    (do ((x l1 (cdr x))
	 (y l2 (cdr y))
	 z)
	((or (endp x) (endp y)) (when (endp x) (rplacd z y)))
      (rplaca x (nconc (car x) (car y)))
      (setq z x))
    l1)

  (cl:defun nmerge (l1 l2)
    (cond ((null l1) l2)
	  ((null l2) l1)
	  (t (nmerge-1 l1 l2))))

  (cl:defun noverlap (n l1 l2)
    "Overlap l2 over the last n components of n1."
    (cond ((eql n 0) (nconc l1 l2))
	  ((null l1) l2)
	  (t
	   (cl:let ((n1 (length l1)))
	     (if (<= n n1)
	       (cl:let ((l (last l1 n)))
		 (nmerge-1 l l2)      
		 l1)
	       (cl:let* ((n2 (length l2))
			 (nt (+ n1 n2)))
		(cond ((>= n nt) (nconc l2 (make-list (- n nt)) l1))
		      (t
		       (cl:let ((l (nthcdr (- n n1) l2)))
	                 (do ((x l (cdr x))
			      (y l1 (cdr y))
			      z)
			     ((or (endp x) (endp y)) (when (endp x) (rplacd z y)))
			   (rplaca x (nconc (car y) (car x)))
			   (setq z x)))))
		 l2))))))

  (cl:defun n-gensyms (n &optional (root (symbol-name '#:g)))
    "Generate n uninterned symbols with basename root"
    (do ((i n (1- i))
	 (l nil (cons (gensym root) l)))
	((zerop i) l)))

  (defmacro valuate (&rest args)
    `(cl:multiple-value-call #'values ,@args))

  (defmacro multiple-value-setf (places vals)
    (cl:let* ((n (length places))
	      (vars (n-gensyms n)))
      `(cl:multiple-value-bind ,vars ,vals
	 (values
	   ,@(mapcar #'(lambda (p v) `(setf ,p ,v)) places vars)))))

  #+:ignore
  (cl:defun polyapply (fun args)
    (declare (dynamic-extent args))
    (if args
	(cl:let ((d (cdr args)))
	  (if d
	      (valuate (cl:funcall fun (car args)) (polyapply fun (cdr args)))
	    (cl:funcall fun (car args))))
      (values)))

  (cl:defun polyapply (fun args)
    (declare (dynamic-extent args))
    (cl:labels ((polyapply-1 (args &rest results)
		  (declare (dynamic-extent args results))	     
		  (cl:let ((d (cdr args)))
		    (if d
			(cl:multiple-value-call #'polyapply-1
						d
						(values-list results) (cl:funcall fun (car args)))
		      (valuate (values-list results) (cl:funcall fun (car args)))))))
      (if args
          (polyapply-1 args)
	(values))))

  (declaim (inline polycall))
  (cl:defun polycall (fun &rest args)
    (declare (dynamic-extent args))
    (polyapply fun args))

  (defmacro multiple-value-polycall (fun vals)
    `(cl:multiple-value-call #'polycall ,fun ,vals))

  (cl:defun 2mapcar (fun orig)
    (if orig
	(cl:let* ((lastcons1 (list nil))
		  (lastcons2 (list nil))
		  (lst1 lastcons1)
		  (lst2 lastcons2))
	  (do ((remains orig (cdr remains)))
	      ((not (consp remains)) (values (cdr lst1) (cdr lst2)))
	    (multiple-value-setq (lastcons1 lastcons2)			    
	      (multiple-value-setf ((cdr  lastcons1) (cdr lastcons2))
				   (multiple-value-polycall #'list
							    (cl:funcall fun (car remains)))))))
      (values nil nil)))

  (cl:defun 3mapcar (fun orig)
    (if orig
	(cl:let* ((lastcons1 (list nil))
		  (lastcons2 (list nil))
		  (lastcons3 (list nil))
		  (lst1 lastcons1)
		  (lst2 lastcons2)
		  (lst3 lastcons3))
	  (do ((remains orig (cdr remains)))
	      ((not (consp remains)) (values (cdr lst1) (cdr lst2) (cdr lst3)))
	    (multiple-value-setq (lastcons1 lastcons2 lastcons3)			    
	      (multiple-value-setf ((cdr  lastcons1) (cdr lastcons2) (cdr lastcons3))
				   (multiple-value-polycall #'list
							    (cl:funcall fun (car remains)))))))
      (values nil nil nil)))

  (cl:defun 2mapcan (fun orig)
    (if orig
	(cl:let* ((lastcons1 (list nil))
		  (lastcons2 (list nil))
		  (lst1 lastcons1)
		  (lst2 lastcons2))
	  (do ((remains orig (cdr remains)))
	      ((not (consp remains)) (values (cdr lst1) (cdr lst2)))
	    (multiple-value-setq (lastcons1 lastcons2)
	      (multiple-value-polycall
	        #'last
		(multiple-value-setf ((cdr  lastcons1) (cdr lastcons2))
				     (multiple-value-polycall #'list
							      (cl:funcall fun (car remains))))))))
      (values nil nil)))

  (cl:defun 3mapcan (fun orig)
    (if orig
	(cl:let* ((lastcons1 (list nil))
		  (lastcons2 (list nil))
		  (lastcons3 (list nil))
		  (lst1 lastcons1)
		  (lst2 lastcons2)
		  (lst3 lastcons3))
	  (do ((remains orig (cdr remains)))
	      ((not (consp remains)) (values (cdr lst1) (cdr lst2) (cdr lst3)))
	    (multiple-value-setq (lastcons1 lastcons2 lastcons3)
	      (multiple-value-polycall
	        #'last
	        (multiple-value-setf ((cdr  lastcons1) (cdr lastcons2) (cdr lastcons3))
				     (multiple-value-polycall #'list
							      (cl:funcall fun (car remains))))))))
      (values nil nil nil)))

  (cl:defun nsubst-inline (new-list old list &optional (save-spot nil))
    (cl:let ((tail (member old list)))
      (cond ((not tail) old)
            (save-spot (rplacd tail (nconc new-list (cdr tail))))
            (new-list (rplaca tail (car new-list))
                      (rplacd tail (nconc (cdr new-list) (cdr tail))))
            ((cdr tail) (rplaca tail (cadr tail))
             (rplacd tail (cddr tail)))
            (t (setq list (nbutlast list)))))
    list)

  ;;Making unique variables.
  ;;Each call on this uses a different atom, so that you can tell
  ;;where a given variable came from when debugging.
  (cl:defun new-var (atom)
    (cl:let ((root (string atom)))
      (when (not (eql (aref root (1- (length root))) #\-))
	(setq root (concatenate 'string root "-")))
      (gensym root)))

  (cl:defun contains-p (item thing)
    (do ((tt thing (cdr tt)))
        ((not (consp tt)) (eq tt item))
      (when (contains-p item (car tt))
	(return t))))

  (cl:defun contains-any (items thing)
    (do ((tt thing (cdr tt)))
        ((not (consp tt)) (member tt items))
      (when (contains-any items (car tt))
	(return t))))

;; some Common Lisps implement copy-tree tail recursively.
(cl:defun iterative-copy-tree (tree)
  (if (not (consp tree))
      tree
    (prog (tail result ptr)
	  (setq tail (cdr tree))
	  (setq result (cons (iterative-copy-tree (car tree)) nil))
	  (setq ptr result)
	  L (when (not (consp tail))
	      (rplacd ptr tail)
	      (return result))
	  (rplacd ptr (cons (iterative-copy-tree (car tail)) nil))
	  (setq ptr (cdr ptr))
	  (setq tail (cdr tail))
	  (go L))))

  ;; Detangle a list into the odd positions, what is left from the
  ;; last odd position on, and the even positions [the first position
  ;; is 1].  (The list actually looks like a property list.  We return
  ;; a list of the properties, a list of the last property/value pair,
  ;; and a list of values of the properties.
  #+nil ; CLISP has bug in loop (?) and doesn't like this.
  (cl:defun detangle2 (args)
    (loop for d in args by #'cddr as b on args by #'cddr
      collect d into vars
      when (not (null (cdr b)))
      collect (cadr b) into binds
      finally (return (values vars b binds))))

  (cl:defun detangle2 (args)
    (cl:let ((vars '())
	     (binds '())
	     (lastbind '())
	     (more (cdr args)))
      (if (null more)
	  (values args args nil)
        (do* ((var-list args (cddr var-list))
	      (bind-list more (cdr var-list)))
          ((endp bind-list)
           (values (nreverse vars)
                   (if (endp var-list) lastbind (cddr lastbind))
                   (nreverse binds)))
	  (setq lastbind var-list)
	  (push (first var-list) vars)
	  (push (first bind-list) binds)))))

  ;; FDMM: SETQ can make parallel assignments.
  ;; But SERIES does not take that into account yet.
  (cl:defun setq-p (thing)
    #+:ignore	  
    (when (eq-car thing 'setq)
      (when-bind (args (cdr thing))
	(cl:multiple-value-bind (vars lastbind binds) (detangle2 args)
	  (when (and (not (null (cdr lastbind)))
		     (every #'symbolp vars))
	    (values vars binds)))))
    ;; Backward-compatible single-variable version
    #+:ignore
    (when (and (eq-car thing 'setq) (= (mod (length thing) 2) 1))
      (cadr thing))
    ;; original version
    (when (and (eq-car thing 'setq) (= (length thing) 3))
      (cadr thing))
   )

  (cl:defun n-integers (n)
    (do ((i (1- n) (1- i))
         (l nil (cons i l)))
        ((minusp i) l)))

  #+:ignore
  (cl:defun n-integer-values (n)
    (cl:labels ((n-integer-values-1 (n &rest args)
		  (declare (dynamic-extent args))		 
		  (cond ((> n 1) (cl:multiple-value-call #'n-integer-values-1
							 (1- n) (1- n) (values-list args)))
			((= n 1) (valuate 0 (values-list args)))
			(t (values)))))
      (n-integer-values-1 n)))

  (cl:defun n-integer-values (n)
    (cl:labels ((n-integer-values-1 (n &rest args)
		  (declare (dynamic-extent args))		 
		  (cond ((> n 0) (apply #'n-integer-values-1 (1- n) (1- n) args))
			(t (values-list args)))))
      (n-integer-values-1 n)))

) ; end of eval-when

;;; Code generation utilities

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro funcase ((binds funexpr) &rest cases)
  (cl:let (var expr
	   (a (gensym)))
    (if (consp binds)
	(destructuring-bind (v &optional (e (gensym))) binds
	  (setq var v
		expr e))
      (setq var binds
	    expr (gensym)))
    (cl:flet ((nameguard () `(and (eq ,a 'function) (symbolp (setq ,var (cadr ,expr)))))
	      (anonguard () `(setq ,var (cond ((eq ,a 'function) (cadr ,expr))
					      ((eq ,a 'lambda) ,expr)
					      (t nil))))
	      (varguard  () `(symbolp ,expr))
	      (compguard () `(and ,a (case ,a ((function lambda) nil) (t t))))
	      (elseguard () t))
      (cl:flet ((compute-guard-1 (tag)
		  (ecase tag
		    (name      (nameguard))
		    (anonymous (anonguard))
		    (variable  (varguard))
		    (computed  (compguard))
		    ((t otherwise else) (elseguard)))))
        (cl:flet ((compute-guard (tag)
	            (if (consp tag)
		        (cons 'or (mapcar #'compute-guard-1 tag))
		      (compute-guard-1 tag))))
          `(cl:let* ((,expr ,funexpr)
		     (,a (when (consp ,expr) (car ,expr)))
		     ,var)
             (cond ,@(mapcar #'(lambda (x) (list* (compute-guard (car x)) (cdr x)))
			     cases))))))))

(cl:defun prognize (forms &optional (prognize-p t))
  (if prognize-p
      (values
       (if (cdr forms)
	   `(progn ,@forms)
	 (car forms))
       nil)
    (values forms t)))

(cl:defun lister (wrapped-p)
  (if wrapped-p
      #'list*
    #'list))

(cl:defun localize (decls forms wrapped-p)
  (values	  
   (cl:funcall (lister wrapped-p)
	       'locally
	       (cons 'declare decls)
	       forms)
   nil))

(cl:defun prologize (prologs forms wrapped-prologs-p wrapped-p)
  (cl:let ((body (if prologs
		     (cl:funcall (if wrapped-prologs-p
				     #'nconc
				   #'append)
				 (if wrapped-prologs-p
				     (apply #'append prologs)
				   prologs)
				 (if wrapped-p
				     forms
				   (progn
				     (setq wrapped-p t)
				     (list forms))))
		   forms)))
    (values body wrapped-p)))
    
(cl:defun declarize (wrapper decls localdecls prologs forms wrapped-p &optional (prognize-p t) (wrapped-prologs-p nil))
  (when decls
    (setq decls (delete nil (cl:funcall wrapper decls))))
  (when localdecls
    (setq localdecls (delete nil (cl:funcall wrapper localdecls))))
  (cl:multiple-value-bind (body wrapped-p)
      (prologize prologs forms wrapped-prologs-p wrapped-p)
    (if decls
	(values
	 (if localdecls
	     (list
	      (cons 'declare decls)
	      (localize localdecls body wrapped-p))
	   (cl:funcall (lister wrapped-p)
		       (cons 'declare decls)
		       body))
	 t)
      (if localdecls
	  (localize localdecls body wrapped-p)
	(if wrapped-p
	    (prognize body prognize-p)
	  (values body nil))))))


(cl:defun destarrify-1 (base binds decls localdecls prologs forms
			&optional (wrapper #'list) (prognize-p t) (wrapped-prologs-p nil))
  (cl:let ((b (car binds))
	   (d (cdr binds)))
    (if b
	(values
	 (if d
	     (cl:multiple-value-bind (body wrapped-p)
		 (cl:multiple-value-call #'declarize
					 wrapper (car decls) (car localdecls) (car prologs)
					 (destarrify-1 base d (cdr decls) (cdr localdecls) (cdr prologs)
						       forms wrapper nil wrapped-prologs-p) ; do not prognize
					 nil ; do not prognize
					 nil)
	       (cl:funcall (lister wrapped-p)
			   base
			   (cl:funcall wrapper b)					
			   body))
	   (cl:multiple-value-bind (body wrapped-p)
	       (declarize wrapper (car decls) (car localdecls) (car prologs) forms t nil nil) ; do not prognize
	     (cl:funcall (lister wrapped-p)
			 base (cl:funcall wrapper b) body)))
	 nil)
      (if d
	  (cl:multiple-value-call #'declarize
				  wrapper (car decls) (car localdecls) (car prologs)
				  (destarrify-1 base d (cdr decls) (cdr localdecls) (cdr prologs)
						forms wrapper prognize-p wrapped-prologs-p)
				  prognize-p nil)
	(declarize wrapper decls localdecls prologs forms t prognize-p wrapped-prologs-p)))))

(cl:defun destarrify (base binds decls localdecls prologs forms &optional (wrapper #'list) (wrapped-prologs-p nil))
  "Covert a BASE* form into a nested set of BASE forms"
    ;;(declare (dynamic-extent #'destarrify-1))
    (if binds
	(destarrify-1 base binds decls localdecls prologs forms wrapper t wrapped-prologs-p)
      (prognize (prologize prologs forms wrapped-prologs-p t) t)))

(cl:defun compute-total-size (dims)
  (cond ((consp dims)
	 (cl:let ((n 1))
           (dolist (d dims)
	     (if (eq d '*)
		 (return-from compute-total-size nil)
	       (setq n (* n d))))
	   n))
	((eq dims '*) nil)
	(t 0)))

;;; DECODE-SEQ-TYPE
;;;
;;; DECODE-SEQ-TYPE tries to canonicalize the given type into the
;;; underlying type.  It returns three values: the sequence type
;;; (string, vector, simple-array, sequence), the length of the
;;; sequence (or NIL if not known) and the element type of the
;;; sequence (or T if not known).
(cl:defun decode-seq-type (type)
  (when (eq-car type 'quote)
    (setq type (cadr type)))
  (if type
      (cond ((and (symbolp type)
		  (string= (string type) #.(string 'bag)))
	     (values 'bag nil t))
	    ((and (consp type)
		  (symbolp (car type))
		  (string= (string (car type)) #.(string 'bag)))
	     (values 'bag nil (cadr type)))
	    ((and (symbolp type)
		  (string= (string type) #.(string 'set)))
	     (values 'set nil t))
	    ((and (consp type)
		  (symbolp (car type))
		  (string= (string (car type)) #.(string  'set)))
	     (values 'set nil (cadr type)))
	    ((and (symbolp type)
		  (string= (string type) #.(string 'ordered-set)))
	     (values 'ordered-set nil t))
	    ((and (consp type)
		  (symbolp (car type))
		  (string= (string (car type)) #.(string 'ordered-set)))
	     (values 'ordered-set nil (cadr type)))
	    (t
	     ;; Hmm, should we use subtypep to handle these?  Might be easier.
	     (cond ((eq type 'list)
		    (values 'list nil t))
		   ((eq-car type 'list)
		    (values 'list nil (cadr type)))
		   ((eq type 'sequence)
		    (values 'sequence nil t))
		   ;; A STRING is canonicalized to (VECTOR CHARACTER)
		   ((eq type 'string)
		    (values 'vector nil 'character))
		   ((eq-car type 'string)
		    (values 'vector
			    (when (numberp (cadr type))
			      (cadr type))
			    'character))
		   ;; But SIMPLE-STRING's are really (SIMPLE-ARRAY
		   ;; CHARACTER (*))
		   ((eq type 'simple-string)
		    (values 'simple-array nil 'character))
		   ((eq-car type 'simple-string)
		    (values 'simple-array
			    (when (numberp (cadr type))
			      (cadr type))
			    'character))
		   ;; A BASE-STRING is (VECTOR BASE-CHAR)
		   ((eq type 'base-string)
		    (values 'vector nil 'base-char))
		   ((eq-car type 'base-string)
		    (values 'vector
			    (when (numberp (cadr type))
			      (cadr type))
			    'base-char))
		   ;; But SIMPLE-BASE-STRING's are really (SIMPLE-ARRAY
		   ;; BASE-CHAR (*))
		   ((or (eq type 'simple-base-string))
		    (values 'simple-array nil 'base-char))
		   ((or (eq-car type 'simple-base-string))
		    (values 'simple-array
			    (when (numberp (cadr type))
			      (cadr type))
			    'base-char))
		   ;; A BIT-VECTOR is (VECTOR BIT)
		   ((eq type 'bit-vector)
		    (values 'vector nil 'bit))
		   ((eq-car type 'bit-vector)
		    (values 'vector (if (numberp (cadr type)) (cadr type)) 'bit))
		   ;; But a SIMPLE-BIT-VECTOR is really a
		   ;; (SIMPLE-ARRAY BIT (*))
		   ((eq type 'simple-bit-vector)
		    (values 'simple-array nil 'bit))
		   ((eq-car type 'simple-bit-vector)
		    (values 'simple-array
			    (when (numberp (cadr type))
			      (cadr type))
			    'bit))
		   ;; A VECTOR is just a VECTOR
		   ((eq type 'vector)
		    (values 'vector nil t))
		   ((eq-car type 'vector)
		    (values 'vector (if (numberp (caddr type)) (caddr type))
			    (if (not (eq (cadr type) '*))
				(cadr type)
			      t)))
		   ;; And a SIMPLE-VECTOR is just a SIMPLE-VECTOR
		   ((eq type 'simple-vector)
		    (values 'simple-vector nil t))
		   ((eq-car type 'simple-vector)
		    (values 'simple-vector (if (numberp (cadr type)) (cadr type)) t))
		   ((eq type 'simple-array)
		    (values 'simple-array nil t))
		   ((eq-car type 'simple-array)
		    (values 'simple-array
			    (when (caddr type)
			      (compute-total-size (caddr type)))
			    (if (not (eq (cadr type) '*))
				(cadr type)
			      t)))
		   ;; An ARRAY is an ARRAY.  We treat arrays
		   ;; essentially as 1D array, in row-major order.
		   ((eq type 'array)
		    (values 'array nil t))
		   ((eq-car type 'array)
		    (values 'array
			    (when (caddr type)
			      (compute-total-size (caddr type)))
			    (if (not (eq (cadr type) '*))
				(cadr type)
			      t)))
		   ;; Everything else is a sequence
		   (t
		    (values 'sequence nil t)))))
    (values 'sequence nil t))) ; "SEQUENCE" - It might be a multidimensional array

(declaim (inline retuns-list-p))
(cl:defun retuns-list-p (expr)
  (or (null expr) (lister-p expr)))

(cl:defun lister-p (expr)
  (when-bind (a (and (consp expr) (car expr)))
    (case a
      ((cons list 
	push pushnew
	last
	copy-list make-list append nconc member butlast nbutlast revappend nreconc
	rplaca rplacd
	mapcar mapcan mapl maplist mapcon
	assoc pairlis acons copy-alist assoc-if assoc-if-not 
	subst subst-if subst-if-not
	nsubst nsubst-if nsubst-if-not 
	sublis nsublis
	intersection nintersection set-difference nset-difference
	adjoin union nunion set-exclusive-or nset-exclusive-or
	get-properties ldiff) expr)
      (quote (when (listp (cadr expr)) expr))
      (collect (when (or (not (cadddr expr))
			 (case (decode-seq-type (caddr expr))
			   ((list bag set ordered-set) t)
			   (t nil)))
		 expr))
      (list* (when (or (caddr expr) (retuns-list-p (cadr expr)))
	       expr))
      (copy-seq (when (retuns-list-p (cadr expr))
		  expr))
      (t nil))))



(cl:defun matching-scan-p (expr pred)
  (cl:let (a)
    (and (eq-car expr 'scan)
	 (or (and (setq a (cadr expr))  (not (caddr expr)) (cl:funcall pred a))
	     (and (setq a (caddr expr)) (cl:funcall pred a))))))

) ; end of eval-when

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro coerce-maybe-fold (thing type)
  `(coerce ,thing ,type))
)


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Here are some changes from Joe Marshall who says that "Franz
;;; Allegro doesn't fold constants in coerce, so you end up calling it
;;; a *lot*.  This alleviates a fair amount of the pain."

;;;
;;; Constant folding
;;;

;;; The basic problem is that Allegro returns NIL
;;; for (constantp '(+ 2 3))
;;; The function (foldable-constant-expression-p '(+ 2 3) nil)
;;; returns T.

(defconst-once /foldable-operator-table/ (make-hash-table))
(defconst-once /unfoldable-operator-table/ (make-hash-table))

(defmacro declare-foldable-constant-operators (&rest names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (name (quote (,@names)))
       (setf (gethash name /foldable-operator-table/) t))))

(defmacro declare-unfoldable-constant-operators (&rest names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (name (quote (,@names)))
       (setf (gethash name /unfoldable-operator-table/) t))))

(declare-foldable-constant-operators
 * + - / /= 1+ 1- < <= = > >=
 abs acos acosh alpha-char-p alphanumericp arrayp ash asin asinh atan atanh atom
 assert

 block both-case-p byte

 caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr
 cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
 cddar cdddar cddddr cdddr cddr cdr ceiling char-code char-downcase char-equal
 char-greaterp char-int char-lessp char-name char-not-equal char-not-greaterp
 char-not-lessp char-upcase char/= char< char<= char= char> char>= character
 characterp cis code-char coerce complex complexp concatenate conjugate
 consp constantp cos cosh

 denominator digit-char digit-char-p double-float

 eighth endp eq eql equal equalp evenp exp expt

 fceiling ffloor fifth first float float-digits float-precision float-radix float-sign
 floatp floor fourth fround ftruncate

 gcd graphic-char-p

 identity imagpart integer-decode-float integer-length integerp isqrt

 keywordp

 lcm ldb ldb-test length listp list-length locally log logand logandc1 logandc2
 logbitp logcount logeqv logior lognand lognor lognot logorc1 logorc2 logtest
 logxor lower-case-p

 max min minusp mod

 name-char ninth not null numberp numerator

 oddp

 phase plusp progn

 rationalp realp realpart rem rest round

 seventh sin single-float sinh sixth sqrt standard-char-p string-equal string-greaterp
 string-lessp string-not-equal string-not-greaterp string-not-lessp string/= string<
 string<= string= string> string>= stringp symbolp

 tailp tan tanh tenth third truncate typep

 zerop
 )

;; a few macros that aren't worth the bother
;; to walk, stuff that we know won't fold, etc.

(declare-unfoldable-constant-operators
 case
 cons
 check-type
 do
 dolist
 dotimes
 ecase
 lambda
 loop
 random)

(cl:defun foldable-constant-expression-p (expression env)
  (or (constantp expression env)
      (and
       (consp expression)
       (cl:let ((operator (car expression))
		 (operands (cdr expression)))
	     (cond ((symbolp operator)
		    (case operator

		  ;; AND can be folded if first arg is foldable
		      (and (or (null operands)
			       (and (foldable-constant-expression-p (first operands) env)
				    (cl:let ((value (eval (first operands))))
				      (or (null value)
					  (null (cdr operands))
					  (foldable-constant-expression-p `(and ,@(rest operands)) env))))))

		  ;; COND can be folded if first clause is foldable
		      (cond (or (null operands)
				(and (foldable-constant-expression-p (car (first operands)) env)
				     (cl:let ((value (eval (car (first operands)))))
				       (if value
					   (foldable-constant-expression-p* (cdr (first operands)) env)
					   (foldable-constant-expression-p `(cond ,@(rest operands)) env))))))
		      (declare t)

		  ;; IF can be folded if condition is foldable
		      (if  (and (foldable-constant-expression-p (first operands) env)
				(cl:let ((condition (eval (first operands))))
				  (if condition
				      (foldable-constant-expression-p (second operands) env)
				      (foldable-constant-expression-p (third operands) env)))))

		  ;; CL:LET can be folded if all the bindings are foldable,
		  ;; and the body is foldable.
		  ((cl:let cl:let*) (and (every (lambda (binding)
					    (or (not (consp binding))
						(foldable-constant-expression-p (second binding) env)))
					  (car operands))
				   (foldable-constant-expression-p* (rest operands) env)))

		  ;; OR can be folded if the first arg is foldable
		      (or  (or (null operands)
			       (and (foldable-constant-expression-p (first operands) env)
				    (cl:let ((value (eval (first operands))))
				      (or value
					  (null (cdr operands))
					  (foldable-constant-expression-p `(or ,@(rest operands)) env))))))

		  ;; THE can be folded if the value clause is foldable
		      (the (foldable-constant-expression-p (second operands) env))

		  ;; UNLESS can be folded if the condition is foldable
		      (unless (and (foldable-constant-expression-p (first operands) env)
				   (cl:let ((condition (eval (first operands))))
				     (or condition
					 (foldable-constant-expression-p* (cdr operands) env)))))

		  ;; WHEN can be folded if the condition is foldable
		      (when (and (foldable-constant-expression-p (first operands) env)
				 (cl:let ((condition (eval (first operands))))
				   (or (null condition)
				       (foldable-constant-expression-p* (cdr operands) env)))))

		  ;; Otherwise, look it up in the table.
		      (t

		   (unless (gethash operator /unfoldable-operator-table/)
		       #||
		       (cond ((gethash operator /foldable-operator-table/)
			      (foldable-constant-expression-p* operands env))
			     ((foldable-constant-expression-p* operands env)
			      (warn "Couldn't fold ~s ~s" operator operands)
			      nil)
			     (t nil))
		       ||#
			 (and (gethash operator /foldable-operator-table/)
			      (foldable-constant-expression-p* operands env))))))

	       ;; A literal lambda can be folded if it is in operator
	       ;; position, all its arguments are foldable, and its body
	       ;; is foldable
		   ((and (consp operator)
			 (eq (car operator) 'lambda)
			 (foldable-constant-expression-p* operands env))
		    (foldable-constant-expression-p* (cddr operator) env))
	       ;; nothing else is foldable
		   (t nil))))))

(cl:defun foldable-constant-expression-p* (forms env)
  (every (lambda (form)
	     (foldable-constant-expression-p form env))
	 forms))

(cl:defun fold-constant-expression (expression &optional expected-type)
  (cl:let ((value (eval expression)))
    (when (and expected-type
	       (not (typep value expected-type)))
      (warn "While folding expression ~s, ~
             the value ~s was not of the expected type ~s."
	    expression value expected-type))
    #||
      ;; 99% of folding involves a symbol constant.
      (unless (or (symbolp expression)
		  (and (consp expression)
		       (eq (car expression) 'quote))
		  (and (consp expression)
		       (eq (car expression) 'the)
		       (or (numberp (third expression))
			   (symbolp (third expression))))
		  (eq expression value))
	(format t "~&Folding ~s => ~s" expression value))
      ||#
    value))

(cl:defun fold-if-possible (form env &optional expected-type)
  "Return either FORM or it's folded value if one can be computed.
   If expected type is given and form is folded, but does not equal
   the expected type, a warning is issued and form is returned unfolded."
  (cl:let ((is-constant (foldable-constant-expression-p form env)))
    (if is-constant
	(cl:let ((value (fold-constant-expression form)))
	  (if (and expected-type
		   (not (typep value expected-type)))
	      (progn
		(warn "While folding expression ~s, ~
                       the value ~s was not of the expected type ~s."
		      form value expected-type)
		form)
	      value))
	form)))

(cl:defun expression-is-constant-equal-to (form env value)
  (eq (fold-if-possible form env) value))

(defmacro coerce-maybe-fold (&environment env thing type)
  (if (and (foldable-constant-expression-p thing env)
	   (foldable-constant-expression-p type env))
      (coerce (fold-constant-expression thing)
	      (fold-constant-expression type))
    `(coerce ,thing ,type)))

) ; end eval-when

(cl:defun extract-declarations (forms)
  "Grab all the DECLARE expressions at the beginning of the list forms"
  (cl:let ((decls nil))
    (do* ((r forms (cdr r))
	  (i (car r) (car r)))
	 ((not (eq-car i 'declare)) (values decls r))
      (push i decls))))

(cl:defun collect-decls (category decls)
  (mapcan #'(lambda (d)
	      (remove-if-not #'(lambda (x)
				 (string-equal (string-upcase (symbol-name (car x)))
					       (string-upcase (symbol-name category))))
			     (cdr d)))
	  decls))

(cl:defun collect-other-decls (category decls)
  (mapcan #'(lambda (d)
	      (remove-if #'(lambda (x)
				 (string-equal (string-upcase (symbol-name (car x)))
					       (string-upcase (symbol-name category))))
			     (cdr d)))
	  decls))

(cl:defun merge-decs (decls)
  (when decls
    (mapcan #'cdr decls)))

(cl:defun collect-declared (category decls)
  "Given a list of DECLARE forms, concatenate all declarations of the same category, with  DECLAREs and category removed"
  (merge-decs (collect-decls category decls)))
  
(cl:defun dynamize-vars (vars forms test)
  (cl:multiple-value-bind (declarations body) (extract-declarations forms)
    (cl:let ((indef (collect-declared 'indefinite-extent declarations))
	     (notindecls (collect-other-decls 'indefinite-extent declarations)))
      (values
        `(,@(when-bind (dynvars (set-difference vars indef :test test))			  
	     `((declare ,(cons 'dynamic-extent dynvars))))
	  ,@(when-bind (indefvars (set-difference indef vars :test test))
	     `((declare ,(cons 'indefinite-extent indefvars))))	
	  ,@(when notindecls
	     `((declare ,@notindecls))))
	body))))

(cl:defun variable-p (thing)
  "Return T if thing is a non-keyword symbol different from T and NIL"
  (and thing (symbolp thing) (not (eq thing t)) (not (keywordp thing))))

(cl:defun simple-quoted-lambda (form)
  (or (and (eq-car form 'cl:function)
	   (eq-car (cadr form) 'cl:lambda)
	   (every #'variable-p (cadr (cadr form))))
      (and (eq-car form 'cl:lambda)
	   (every #'variable-p (cadr form)))))

(cl:defun simple-quoted-lambda-arguments (form)
  (ecase (car form)
    (cl:function (cadr (cadr form)))
    (cl:lambda (cadr form))))

(cl:defun simple-quoted-lambda-body (form)
  (ecase (car form)
    (cl:function (cddr (cadr form)))
    (cl:lambda (cddr form))))

(cl:defun nullable-p (typ)
  (or (member typ '(t null list boolean))
      (and (consp typ)
	   (cl:let ((a (car typ)))
	     (or (eq a 'null-or)
		 (eq a 'list)
		 (and (eq a 'or)
		      (some #'nullable-p (cdr typ))))))))

(cl:defun make-nullable (typ)
  "Make a type specifier of the form: (or FOO NULL)"
  (if (nullable-p typ)
      typ
    `(null-or ,typ)))

(cl:defun type-or-list-of-type (typ)
  "Make a type specifier of the form: (or FOO (list FOO))"
  `(or ,typ list))

(cl:defun never (&rest stuff)
  "Always returns NIL"
  (declare (ignore stuff)) nil)

(cl:defun make-general-setq (vars value)
  (cond ((= (length vars) 0) value)
        ((= (length vars) 1) `(setq ,(car vars) ,value))
        ((and (eq-car value 'values)
              (= (length (cdr value)) (length vars)))
         `(psetq ,@(mapcan #'list vars (cdr value))))
        (t `(multiple-value-setq ,vars ,value))))

(cl:defun pos (&rest bools)
  (declare (dynamic-extent bools))
  (do ((bs bools (cdr bs))
       (i 0 (1+ i)))
      ((null bs) i)
    (when (car bs)
      (return i))))

(cl:defun pos-if (item &rest fns)
  (declare (dynamic-extent fns))	  
  (do ((fs fns (cdr fs))
       (i 0 (1+ i)))
      ((null fs) i)
    (when (cl:funcall (car fs) item)
      (return i))))

(cl:defun forceL (n list)
  (declare (type fixnum n)
           (type list list))
  (if (= n (length list))
      list
    (cl:let* ((new (make-list n :initial-element nil))
	      (ptr new))
      (do ((i (min n (length list)) (1- i)))
	  ((zerop i))
	(rplaca ptr (pop list))
	(pop ptr))
      new)))


;;;; SERIES-specific code

(cl:defun install (&key (pkg *package*) (macro t) (shadow t) (implicit-map nil)
			(remove nil) (shadow-extensions shadow))
  (setq *series-implicit-map* implicit-map)
  (when (not (packagep pkg)) (setq pkg (find-package pkg)))
  (cl:let ((spkg (find-package :series)))
    (when (not remove)
      (when macro
        (set-dispatch-macro-character #\# #\Z (cl:function series-reader))
        (set-dispatch-macro-character #\# #\M (cl:function abbreviated-map-fn-reader)))
        (when (not (eq pkg spkg))
	  ;;This is here because UNTIL and COLLECT are loop clauses.
	  (cl:multiple-value-bind (sym code) (find-symbol (symbol-name '#:until) pkg)
	    (when (and sym (eq code :internal)
		       (not (boundp sym)) (not (fboundp sym)) (null (symbol-plist sym)))
	      (unintern sym pkg)))
	  (cl:multiple-value-bind (sym code) (find-symbol (symbol-name '#:collect) pkg)
	    (when (and sym (eq code :internal)
		       (not (boundp sym)) (not (fboundp sym)) (null (symbol-plist sym)))
	      (unintern sym pkg)))
	  #+(or cmu scl Harlequin-Common-Lisp)
	  (unintern 'series :common-lisp-user)
	  (shadowing-import /ext-conflicts/ pkg)
	  (use-package :series pkg)
	  (when shadow (shadowing-import /series-forms/ pkg))
	  (unless shadow-extensions
	    (when /ext-conflicts/
	      (cl:let* ((ext (find-package :extensions))
			(syms (mapcar #'(lambda (s)
					  (find-symbol (symbol-name s) ext))
				      /ext-conflicts/)))
		 
		(shadowing-import syms pkg))))
	  ))
    (when (and remove (member spkg (package-use-list pkg)))
      (unuse-package :series pkg)
      (dolist (sym (intersection (union /series-forms/ /ext-conflicts/)
				 (package-shadowing-symbols pkg)))
	(unintern sym pkg))))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;Internally used special variables.  Every one is collected here except some
;;scan templates used in macro expansion.

(defvar *optimize-series-expressions* t)
(defvar *in-series-expr* nil
  "the topmost series expression")
(defvar *testing-errors* nil
  "Used only be the file of tests")
(defvar *not-straight-line-code* nil
  "not-nil if nested in non-straight-line code")

(declaim (special *graph*               ;list of frags in expression
                  *renames*             ;alist of variable renamings
                  *user-names*          ;series::let var names used by user
                  *env*                 ;environment of containing series macro call
                  *call*                ;bound to whole form when running optimizer
                  *being-setqed*        ;T if in the assignment part of a setq
                  *fn*                  ;FN being scanned over code
                  *type*                ;Communicates types to frag instantiations
                  *limit*               ;Communicates limits to frag instantiations
		  *frag-arg-1*          ;Communicates arg to frag instantiations
		  *frag-arg-2*          ;Communicates arg to frag instantiations
		  ))
;; DEBUG
;;
;; With these two assigments you can use (SERIES::PROCESS-TOP <quoted form>)
;; to view series expansions:
;; (setq series::*renames* nil) (setq series::*env* nil)

;; *renames* has three kinds of entries on it.  Each is a cons of a
;; variable and something else: (type 1 cannot ever be setqed.)
;;
;; 1- a ret, var is a series::let var or a series::lambda var.  You
;; can tell between the two because series::lambda var frags are not
;; in *graph*.
;;
;; 2- a new var, var is an aux var.
;;
;; 3- nil, var is rebound and protected from renaming.

(defconst-once /short-hand-types/
  '(array atom bignum bit bit-vector character common compiled-function
    complex cons double-float fixnum float function hash-table integer
    keyword list long-float nil null number package pathname random-state
    ratio rational readtable sequence short-float simple-array
    simple-bit-vector simple-string simple-vector single-float standard-char
    stream string string-char symbol t vector series)
  "table 4.1 from CLTL")

(defvar *standard-function-reader* (get-dispatch-macro-character #\# #\'))

;;;;                         ---- ERROR REPORTING ----

;; HELPER
(cl:defun report-error (info)
  (setq *last-series-error* info)
  (loop (unless info (return nil))
        (if (stringp (car info))
            (format *error-output* (pop info))
	  (write (pop info) :stream *error-output* :escape t :pretty t
		 :level nil :length nil :case :upcase))))

(cl:defun ers (id &rest args)  ;Fatal errors.
  (declare (dynamic-extent args))	  
  (when *testing-errors*
    (throw :testing-errors id))
  (if *in-series-expr*
      (report-error (list* "~&Error " id " in series expression:~%"
			   *in-series-expr* (copy-list args)))
    (report-error (list* "~&Error " id (copy-list args))))
  (error ""))

(cl:defun wrs (id always-report-p &rest args) ;Warnings.
  (declare (dynamic-extent args))	  
  (when (or always-report-p (not *suppress-series-warnings*))
    (report-error (list* "~&Warning " id
                         " in series expression:~%"
                         (or *in-series-expr*
                             (and (boundp '*not-straight-line-code*)
                                  *not-straight-line-code*))
                         (copy-list args)))
    ;; What is this for?  Why do we want to print out this warning
    ;; when *testing-errors* is NIL?  Remove this.  Tests still pass.
    #+nil
    (when (not *testing-errors*)
      (warn ""))))

;; HELPER
(cl:defun rrs (id &rest args) ;Restriction violations.
  (declare (dynamic-extent args))	  
  (when (not *suppress-series-warnings*)
    (report-error (list* "~&Restriction violation " id
                         " in series expression:~%"
                         (or *in-series-expr* *not-straight-line-code*)
                         (copy-list args)))
    (when (not *testing-errors*)
      (warn "")))
  (throw :series-restriction-violation nil))

;;;;             ---- UTILITIES FOR MANIPULATING FRAGMENTS ----

(defvar end 'end "used to force copying of frag lists")

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
  (wrappers nil)  ;functions that wrap forms around the whole loop.
  (impure nil)    ;whether this is referentially transparent or not (nil (referentially transparent (but movement constrained by series flow); t (not at all); :args, if it depends on args; :mutable, if it depends on mutable arguments that could be literal or private; :context, if movable depending on the whole expr(mutables that cannot be literal or private)) 
)
;; CMUCL 18b BUG fixnup macro - KEEP IN SYNC WITH THE ABOVE!!
#+:cmu18
(defmacro frag-wrappers (f) `(nth 11 ,f))
#-:cmu18
(defmacro frag-wrappers (f) `(wrappers ,f))

;; what whould be nil if loop wrapper only
(declaim (inline add-wrapper))
(cl:defun add-wrapper (frag wrap-fn &optional (what t))
  (push `(,wrap-fn ,what) (wrappers frag)))

(declaim (inline wrapper-function))
(cl:defun wrapper-function (w)
  (car w))

(declaim (inline wrapper-type))
(cl:defun wrapper-type (w)
  (cadr w))

(declaim (inline loop-wrapper-p))
(cl:defun loop-wrapper-p (w)
  (eq (wrapper-type w) :loop))

(declaim (inline epilog-wrapper-p))
(cl:defun epilog-wrapper-p (w)
  (eq (wrapper-type w) :epilog))

#+:series-plain
(progn

  ;;; Prologs
  
  (defmacro doprolog ((v prologs) &body body)
    `(dolist (,v ,prologs)
       ,@body))

  (declaim (inline makeprolog))
  (cl:defun makeprolog (prologlist)
    prologlist)	  

  (declaim (inline flatten-prolog))
  (cl:defun flatten-prolog (prologs)
    prologs)

  (declaim (inline mapprolog))
  (cl:defun mapprolog (fun prologs)
    (mapcar fun prologs))

  (declaim (inline 2mapprolog))
  (cl:defun 2mapprolog (fun prologs)
    (2mapcar fun prologs))

  (declaim (inline 3mapprolog))
  (cl:defun 3mapprolog (fun prologs)
    (3mapcar fun prologs))

  (declaim (inline first-prolog-block))
  (cl:defun first-prolog-block (prologs)
    prologs)

  (declaim (inline last-prolog-block))
  (cl:defun last-prolog-block (prologs)
    prologs)

  (declaim (inline find-prolog))
  (cl:defun find-prolog (var prologs)
    (assoc var prologs))

  (declaim (inline delete-prolog))
  (cl:defun delete-prolog (var prologs)
    (delete var prologs :key #'car))

  (declaim (inline delete-last-prolog))
  (cl:defun delete-last-prolog (prologs)
    (nbutlast prologs))

  (declaim (inline delete-prolog-if))
  (cl:defun delete-prolog-if (p prologs)
    (delete-if p prologs))

  (declaim (inline remove-prolog-if))
  (cl:defun remove-prolog-if (p prologs)
    (remove-if p prologs))

  (declaim (inline add-prolog))
  (cl:defun add-prolog (frag p)
    (push p (prolog frag)))

  (declaim (inline prolog-append))
  (cl:defun prolog-append (p l)
    (append p l))
  
  (declaim (inline prolog-length))
  (cl:defun prolog-length (p)
    (length p))

  (declaim (inline merge-prologs))
  (cl:defun merge-prologs (p1 p2)
    (nconc p1 p2))
  
  ;;; Auxs

  (defmacro doaux ((v auxs) &body body)
    `(dolist (,v ,auxs)
       ,@body))

  (declaim (inline makeaux))
  (cl:defun makeaux (auxlist)
    auxlist)	  

  (declaim (inline flatten-aux))
  (cl:defun flatten-aux (auxs)
    auxs)

  (declaim (inline mapauxn))
  (cl:defun mapauxn (fun auxs)
    (mapcar fun auxs))

  (declaim (inline mapaux))
  (cl:defun nmapaux (fun auxs)
    (mapcan fun auxs))

  (declaim (inline mapaux))
  (cl:defun mapaux (fun auxs)
    (mapcar fun auxs))

  (declaim (inline 2mapaux))
  (cl:defun 2mapaux (fun auxs)
    (2mapcar fun auxs))

  (declaim (inline 3mapaux))
  (cl:defun 3mapaux (fun auxs)
    (3mapcar fun auxs))

  (declaim (inline first-aux-block))
  (cl:defun first-aux-block (auxs)
    auxs)

  (declaim (inline find-aux))
  (cl:defun find-aux (var auxs)
    (assoc var auxs))

  (declaim (inline delete-aux))
  (cl:defun delete-aux (var auxs)
    (delete var auxs :key #'car))

  (declaim (inline delete-aux-if))
  (cl:defun delete-aux-if (p auxs)
    (delete-if p auxs))

#|  
  (declaim (inline remove-aux-if))
  (cl:defun remove-aux-if (p auxs)
    (remove-if p auxs))
|#
  (declaim (inline segregate-aux))
  (cl:defun segregate-aux (p auxs)
    (values (remove-if-not p auxs) (remove-if p auxs)))

  (cl:defun add-aux (frag var typ &optional (val nil val-p))
    (push (if val-p
	      `(,var ,typ ,val)
	    `(,var ,typ))
	  (aux frag)))
  )

#-:series-plain
(progn
  ;;; Prologs
  
  (defmacro doprolog ((v prologs) &body body)
    (cl:let ((b (gensym)))
      `(dolist (,b ,prologs)
	 (dolist (,v ,b)
	   ,@body))))

  (declaim (inline makeprolog))
  (cl:defun makeprolog (prologlist)
    (when prologlist
      (list prologlist)))	  

  (declaim (inline flatten-prolog))
  (cl:defun flatten-prolog (prologs)
    (apply #'append prologs))

  (declaim (inline mapprolog))
  (cl:defun mapprolog (fun prologs)	  
    (mapcan #'(lambda (b) (mapcar fun b)) prologs))

  (declaim (inline 2mapprolog))
  (cl:defun 2mapprolog (fun prologs)	  
    (2mapcar #'(lambda (b) (2mapcar fun b)) prologs))

  (declaim (inline 3mapprolog))
  (cl:defun 3mapprolog (fun prologs)	  
    (3mapcar #'(lambda (b) (3mapcar fun b)) prologs))

  (declaim (inline first-prolog-block))
  (cl:defun first-prolog-block (prologs)
    (car prologs))

  (declaim (inline last-prolog-block))
  (cl:defun last-prolog-block (prologs)
    (car (last prologs)))

  (cl:defun add-prolog (frag p)
    (cl:let ((prologs (prolog frag)))
      (if prologs
	  (progn
	    (push p
		  (car prologs))
	    prologs)
	(setf (prolog frag) (makeprolog (list p))))))

  (cl:defun prolog-append (prologs l)
    (if l
	(if prologs
	    (cl:let* ((p (copy-list prologs))
		      (la (last p)))
	      (rplaca la (append (car la) l))
	      p)
	  (makeprolog l))
      prologs))
  
  (cl:defun prolog-length (p)
    (cl:let ((x 0))
      (dolist (l p)
	(setq x (+ x (length l))))
      x))

  (declaim (inline merge-prologs))
  (cl:defun merge-prologs (p1 p2)
    (nmerge p1 p2))
  
  (declaim (inline find-prolog))
  (cl:defun find-prolog (var prologs)
    (dolist (b prologs)
      (when-bind (a (assoc var b))
        (return a))))	       

  (declaim (inline delete-prolog))
  (cl:defun delete-prolog (var prologs)
    (mapcar #'(lambda (b) (delete var b :key #'car)) prologs))

  (declaim (inline delete-last-prolog))
  (cl:defun delete-last-prolog (prologs)
    (cl:let ((l (last prologs)))
      (rplaca l (nbutlast (car l)))
      prologs))

  (declaim (inline remove-prolog-if))
  (cl:defun remove-prolog-if (p prologs)
    (mapcar #'(lambda (b) (remove-if p b)) prologs))


  ;;; Local variable handling (Auxs)

  (defmacro doaux ((v auxs) &body body)
    (cl:let ((b (gensym)))
      `(dolist (,b ,auxs)
	 (dolist (,v ,b)
	   ,@body))))

  (declaim (inline makeaux))
  (cl:defun makeaux (auxlist)
    (when auxlist
      (list auxlist)))	  

  (declaim (inline flatten-aux))
  (cl:defun flatten-aux (auxs)
    (apply #'append auxs))

  (declaim (inline mapauxn))
  (cl:defun mapauxn (fun auxs)	  
    (mapcan #'(lambda (b) (mapcar fun b)) auxs))

  (declaim (inline mapaux))
  (cl:defun nmapaux (fun auxs)	  
    (mapcar #'(lambda (b) (mapcan fun b)) auxs))

  (declaim (inline mapaux))
  (cl:defun mapaux (fun auxs)	  
    (mapcar #'(lambda (b) (mapcar fun b)) auxs))

  (declaim (inline 2mapaux))
  (cl:defun 2mapaux (fun auxs)	  
    (2mapcar #'(lambda (b) (2mapcar fun b)) auxs))

  (declaim (inline 3mapaux))
  (cl:defun 3mapaux (fun auxs)	  
    (3mapcar #'(lambda (b) (3mapcar fun b)) auxs))

  (declaim (inline first-aux-block))
  (cl:defun first-aux-block (auxs)
    (car auxs))

  (cl:defun add-aux (frag var typ &optional (val nil val-p))
    (cl:let ((auxs (aux frag))
	     (entry (if val-p
			`(,var ,typ ,val)
		      `(,var ,typ))))
      (if auxs
	  (progn
	    (push entry
		  (car auxs))
	    auxs)
	(setf (aux frag) (makeaux (list entry))))))

  (declaim (inline find-aux))
  (cl:defun find-aux (var auxs)
    (dolist (b auxs)
      (when-bind (a (assoc var b))
        (return a))))	       

  (declaim (inline delete-aux))
  (cl:defun delete-aux (var auxs)
    (mapcar #'(lambda (b) (delete var b :key #'car)) auxs))

  (declaim (inline delete-aux-if))
  (cl:defun delete-aux-if (p auxs)
    (mapcar #'(lambda (b) (delete-if p b)) auxs))

  (declaim (inline delete-aux-if-not))
  (cl:defun delete-aux-if-not (p auxs)
    (mapcar #'(lambda (b) (remove-if-not p b)) auxs))

#|  
  (declaim (inline remove-aux-if))
  (cl:defun remove-aux-if (p auxs)
    (mapcar #'(lambda (b) (remove-if p b)) auxs))

  (declaim (inline remove-aux-if-not))
  (cl:defun remove-aux-if-not (p auxs)
    (mapcar #'(lambda (b) (remove-if-not p b)) auxs))
|#
  (declaim (inline segregate-aux))
  (cl:defun segregate-aux (p auxs)
    (2mapcar #'(lambda (b) (values (remove-if-not p b) (remove-if p b))) auxs))

  )

;;; Prologs

(declaim (inline first-prolog))
(cl:defun first-prolog (prologs)
  (car (first-prolog-block prologs)))

(cl:defun append-prolog (frag p)
  (setf (prolog frag)
	(prolog-append (prolog frag) p)))

;;; Auxs

(declaim (inline first-aux))
(cl:defun first-aux (auxs)
  (car (first-aux-block auxs)))

(declaim (inline add-literal-aux))
(cl:defun add-literal-aux (frag var typ val)
  (add-aux frag var typ val))

(cl:defun add-nonliteral-aux (frag var typ val)
  (add-aux frag var typ #-:series-plain val)
  #+:series-plain 
  (push `(setq ,var ,val) (prolog frag)))

(cl:defun simplify-aux (auxs)
  (mapaux #'(lambda (v)
	      (cl:let ((d (cddr v)))
		      (if (and d (not (constantp (car d))))
			  `(,(car v) ,(cadr v))
			v)))
	  auxs))

(cl:defun aux->prolog (auxs)
  (nmapaux #'(lambda (v)
	       (cl:let ((d (cddr v)))
		       (when (and d (not (constantp (car d))))
			 `((setq ,(car v) ,(car d))))))
	   auxs))

;;; There cannot be any redundancy in or between the args and aux.
;;; Each ret variable must be either on the args list or the aux list.
;;; The args and ret have additional data as discussed below.  The aux
;;; is just a list of lists of a symbol and a type specifier.  Every
;;; symbol used in a frag which could possible clash with other frags
;;; (eg args, rets, aux, and also labels) must be gensyms and unique
;;; in the whole world.
;;;
;;; The order of the args is important when the frag is first
;;; instantiated and funcalled.  However, it does not matter after
;;; that. Similarly, the order of the rets also matters at the time it
;;; is instantiated, and at the time that a whole expression is turned
;;; into one frag, but it does not matter at other times.
;;;
;;; There are two basic kinds of frags, series frags and non-series
;;; frags.  A non-series frag is a frag which just has a simple
;;; computation which has to be performed only once.  The rets and
;;; args must be non-series values, and the body and epilog must be
;;; empty.  (The code below maintains the invariant that if all the
;;; ports of a frag are non-series then the body and epilog are
;;; empty.)
;;;
;;; A frag has three internal parts so that a wide variety of
;;; fragmentary series functions can be compressed into a single frag.
;;;
;;; Inside frags there is a label which has a special meaning.
;;;  END is used as the label after the end of the loop created.  If the
;;;     body of a fragment contains (go END) then the fragment is an
;;;     active terminator.
;;;
;;; If a programmer uses these symbols in his program, very bad things
;;; could happen.  However, it is in the series package, so there
;;; should not be any conflict problems.  the code in this file
;;; assumes in many places that no symbol in the "SERIES" package can
;;; possibly clash with a user symbol.
;;;
;;; The code field is used solely to generate error messages.
;;; However, it is never the less very important.  In particular, it
;;; is important that the code field only contain things that were
;;; actually in the user's source code.  It is also important that it
;;; always contain something.
;;;
;;; There are several reasons why the code might end up being
;;; something the user did not write.  The foremost reason is macro
;;; expansion.  It might be the result of some expansion that turns
;;; into a frag.  To fix this, my-macroexpand saves the first form
;;; before macro expansion, and puts that in the code field.  To make
;;; this work, it must be the case that every macro that can possibly
;;; expand into something that will trigger the process of converting
;;; an expression into a loop must call PROCESS-TOP.  To ensure this
;;; it must be the case that every macro the user can type must be
;;; defined with defS or DEFUN with an OPTIMIZABLE-SERIES-FUNCTION
;;; declaration.  (Note it is fine for things the user cannot type
;;; anyway to be defined with defmacro.)  (Unfortunately, the end user
;;; can break this rule if they define a new collector with DEFMACRO,
;;; but you cannot make everything work.  At least all they will see
;;; is things generated by their own macro)

(cl:defun annotate (code frag)
  (when (frag-p frag)
    (setf (code frag) code))
  frag)

;;; Considerable effort is expended to see that the code field usually
;;; contains code that makes sense to the user.  Extensive testing
;;; indicates that it never ends up containing :||, and that the code it
;;; contains always is part of the code the user types except that an
;;; optional argument can end up having the default value which ends up
;;; in the annotation.
;;;
;;; Each arg and ret has the following parts.

(defstruct (sym (:conc-name nil) (:type list) :named)
  (back-ptrs (make-array 2 :initial-element nil))
  (var nil)              ;gensymed variable.
  (series-var-p nil)     ;T if holds a series.
  (off-line-spot nil)    ;if off-line, place to insert the computation.
  (off-line-exit nil)  ;if non-passive input, label to catch exit.
  )

;;; If there is an on-line-spot, it must appear in the frag code exactly
;;; once at top level.  It cannot be nested in a form.  It also can only be
;;; referred to from a single input or output.

;;; A number of functions depend on the fact that frags and syms are list
;;; structures which can be traversed by functions like nsubst.  The
;;; following three circular pointers are hidden in an array so they
;;; won't be followed.  (Note that ins only have prv and rets only have
;;; nxts, as a result, they can both be stored in the same place.  two
;;; names are used in order to enhance the readability of the program.)

(defmacro fr (s)       ;back pointer to containing frag.
  `(aref (back-ptrs ,s) 0))
(defmacro nxts (s)     ;list of destinations of dflows starting here.
  `(aref (back-ptrs ,s) 1))
(defmacro prv (s)      ;the single source of dflow to here.
  `(aref (back-ptrs ,s) 1))

;;; The sym vars are symbols which appear in the body of the frag
;;; where they should.  All of the symbols must be unique in all the
;;; world.  Every instance of the symbol anywhere must be a use of the
;;; symbol.
;;;
;;; Output variables can be freely read and written. Input variables
;;; can be read freely, but cannot ever be written.
;;;
;;; These restrictions guarantee that when frags are combined, it is
;;; OK to rename the input var of one to be the output var of the
;;; other.  In addition, the creator of an output can depend on the
;;; output variable being unchanged by the user(s).  However, this is
;;; not the main point. More critical is the situation where two frags
;;; use the same value. The second frag can be sure that the first
;;; frag did not mess up the value. (Side-effects could still cause
;;; problems.  The user must guard against destroying some other
;;; fragment's internal state.)
;;;
;;; In the interest of good output code, some work is done to simplify
;;; things when frags are merged.  If an output is of the form (setq
;;; out c) where c is T, nil, or a number, then c is substituted
;;; directly for the input.  Substitution is also applied if c is a
;;; variable which is not bound in the destination frag.  In addition,
;;; other kinds of constants are substituted if they are only used in
;;; one place.  A final pass gets rid of setqs to variables that are
;;; never used for anything.

(defmacro free-out (s) ;var output is assigned to if any.
  `(off-line-exit ,s))

;;; only inputs can have off-line exits, so we can reuse the same
;;; field for this.  if an output is assigned to a variable on
;;; *renames*, the variable is recorded here.  This is used in some
;;; situations to hook up data flow correctly.  It also indicates a
;;; few additional things.
;;;
;;;  (A) you cannot every kill this ret, because you may need it even if
;;;      you do not need it for dflow by nesting of expressions.
;;;
;;;  (B) if you have it still existing at the end of everything, because
;;;      it was never used, then this is something to issue a warning about,
;;;      but it is not a value to be returned by the expression as a whole.
;;;
;;; The third key internal form is a graph of frags.  This is
;;; represented in an indirect way.  The special variable *graph*
;;; contains a list of all of the frags in the series expression
;;; currently being processed.  The order of the frags in this list is
;;; vitally important.  It corresponds to their lexical order in the
;;; input expression and controls the default way things with no data
;;; flow between them are ordered when combined.  In addition, many of
;;; the algorithms depend on the fact that the order in *graph* is
;;; compatible with the data flow in that there can never be data flow
;;; from a frag to an earlier frag in the list.
;;;
;;; Subexpressions and regions within the expression as a whole are
;;; delineated by setting marking bits in the frags in the region.
;;;
;;; lambda-series makes special frags for arguments which are not in
;;; the list *graph*.  They exist to record info about the arguments
;;; and to preserve an invariant that every input of every frag in
;;; *graph* must have data flow ending on it.  A related invariant
;;; states that if a frag in *graph* has a ret then this ret must be
;;; used either by having dflow from it, or as an output of the
;;; expression as a whole.  Unused rets are removed from frags when
;;; the frags are created.
;;;
;;; for the purposes of testing whether a subexpression is strongly
;;; connected to its outputs, a frag with no rets is considered to be
;;; an output of the subexpression.

(cl:defun non-series-p (frag)
  (and (notany #'series-var-p (rets frag))
       (notany #'series-var-p (args frag))))

;; this assumes that every instance of one of series's funny labels is
;; really an instance of that label made by the macros below.
(cl:defun branches-to (label tree)
  (cond ((and (eq-car tree 'tagbody) (member label tree)) nil)
        ((and (eq-car tree 'go) (eq-car (cdr tree) label)) t)
        (t (do ((tt tree (cdr tt)))
               ((not (consp tt)) nil)
             (when (branches-to label (car tt))
	       (return t))))))

#+:series-plain
(cl:defun prolog-branches-to (label frag)
  (branches-to label (prolog frag)))

#-:series-plain
(cl:defun prolog-branches-to (label frag)
  (some #'(lambda (p) (branches-to label p)) (prolog frag)))

(cl:defun active-terminator-p (frag)
  (or (prolog-branches-to end frag)
      (branches-to end (body frag))))

;; This gets rid of duplicate labs in a row.
(cl:defun clean-labs (frag stmtns)
  (cl:let ((alist nil))
    (do ((l stmtns (cdr l))) ((not (consp (cdr l))))
      CLEANL (when (and (car l) (symbolp (car l))
                        (cadr l) (symbolp (cadr l)))
               (push (cons (pop (cdr l)) (car l)) alist)
               (go CLEANL)))
    (nsublis alist frag)))

(cl:defun apply-wrappers (wrps code &optional (test #'identity))
  (dolist (wrp wrps)
    (when (cl:funcall test wrp)
      (setq code (cl:funcall (eval (wrapper-function wrp)) code))))
  code)
	  
(cl:defun wrap-code (wrps code &optional (test #'identity))
  (if (cdr code)
      (setq code (cons 'progn code))
    (setq code (car code)))
  (list (apply-wrappers wrps code test)))

(cl:defun clean-tagbody-redundancy (expr)
  (if expr
    (cl:let ((a (car expr)))
      (if (and (eq-car a 'go)
	       (eq (cadr a) (cadr expr)))
	  (values (rplacd (cdr expr) (clean-tagbody-redundancy (cddr expr)))
		  t)
	(cl:multiple-value-bind (remaining cleaned) (clean-tagbody-redundancy (cdr expr))
	  (if cleaned
	      (values (rplacd expr remaining) t)
	    (values expr nil)))))
    (values nil nil)))

(cl:defun clean-tagbody-deadcode (expr)
  (if expr
    (cl:let ((a (car expr)))
      (if (eq-car a 'go)
	  (cl:let ((remaining (member-if #'symbolp (cdr expr))))	    
	    (values (rplacd expr (and remaining (rplacd remaining (clean-tagbody-deadcode (cdr remaining)))))
		    t))
	(cl:multiple-value-bind (remaining cleaned) (clean-tagbody-deadcode (cdr expr))
	  (if cleaned
	      (values (rplacd expr remaining) t)
	    (values expr nil)))))
    (values nil nil)))

(cl:defun clean-tagbody-body (expr)
  (when expr
    (cl:let ((cleaned t))
    (clean-tagbody-deadcode expr)

    ;; This should be done better, with the list reversed
    (tagbody
     L
     (cl:multiple-value-setq (expr cleaned) (clean-tagbody-redundancy expr))
     (if cleaned (go L)))
    
    (if (every #'symbolp expr)
	nil
      (if (every #'(lambda (x) (or (symbolp x) (eq-car x 'go))) expr)
	  (cl:let ((tags (remove-if-not #'symbolp expr))
		   (stms (remove-if #'symbolp expr)))
	    (setq cleaned nil)	   
	    (dolist (s tags)
	      (unless (find-if #'(lambda (x) (eq (cadr x) s)) stms)
	        (setq expr (delete s expr))
		(setq cleaned t)))
	    (if cleaned
		(clean-tagbody-body expr)
	      expr))
	expr)))))
	
(cl:defun clean-tagbody (expr)
  (when-bind (body (clean-tagbody-body (cdr expr)))
    (if (cdr body)
	(cons 'tagbody body)
      (car body))))
    
;; This takes a series frag all of whose inputs and outputs are
;; non-series things and makes it into a non-series frag.
(cl:defun maybe-de-series (frag &optional (prologize t))
  (when (non-series-p frag)
    (cl:let ((bod (body frag)))
      (when (or bod (epilog frag))
	(cl:let ((loop nil)
		 (wrps (wrappers frag)))
	  (when bod
	    (when (not (active-terminator-p frag))
	      (wrs 29 nil "~%Non-terminating series expression:~%" (code frag)))
	    (cl:let ((lab (new-var 'll)))
	      (setq loop (clean-tagbody
			       `(tagbody ,lab ,@bod (go ,lab)
				      ,@(when (branches-to end bod) `(,end))
				      ))))
	    (when (and wrps loop)	      
	      (setq loop (apply-wrappers wrps loop #'loop-wrapper-p))))
	  (when wrps
	    (setf (wrappers frag) (delete-if #'loop-wrapper-p wrps)))
	  (cl:let ((ending (if loop
			       (append (list loop) (epilog frag))
			     (epilog frag))))
	    (if prologize
		(progn
		  (append-prolog frag ending)
		  (setf (body frag) nil))
	      (setf (body frag) ending)))
	  (setf (epilog frag) nil) 
	  (clean-labs frag (cdr loop))))))
  frag)


;; hacking marks

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

(cl:defun worsen-purity (p1 p2)
  (if p1
      (if p2
	  (ecase p1
	    ((t) t)
	    (:context (if (eq p2 t) t :context))
	    (:fun p2)
	    (:args (if (eq p2 :fun)
		       :args
		     p2))
	    (:mutable (if (or (eq p2 :fun)
			      (eq p2 :args))
			  :mutable
			p2)))
	p1)
    p2))

;; many of the functions in this file depend on the fact that frags
;; and syms are list structures.
;; But at least, the following function does not depend anymore on the
;; exact position of parts of these structures. I hope no other one still does.
;; BTW, note that the CL manual guarantees that these
;; positions would be correct in all implementations. 
(cl:defun merge-frags (frag1 frag2)
  (when (must-run frag1) (setf (must-run frag2) t))

  #+:series-plain
  (progn
    (setf (aux frag2)  (nconc (aux frag1)  (aux frag2)))
    (setf (prolog frag2)    (nconc (prolog frag1)    (prolog frag2))))

  #-:series-plain
  (cl:let ((a1 (aux frag1))
	   (a2 (aux frag2))
	   (p1 (prolog frag1))
	   (p2 (prolog frag2)))
    (if (some #'(lambda (i)
		  (some #'(lambda (o)
			    (member i (nxts o)))
			(rets frag1)))
	      (args frag2))
	(progn
	  (when (or p1 p2)
	    (setf (prolog frag2)
		  (nconc (if a1
			     (or p1 (make-list (length a1)))
			   p1)
			 (if a2
			     (or p2 (make-list (length a2)))
			   p2))))

	  (when (or a1 a2)	
	    (setf (aux frag2)
		  (nconc (if p1
			     (or a1 (make-list (length p1)))
			   a1)
			 (if p2
			     (or a2 (make-list (length p2)))
			   a2)))))
      (progn
	  (setf (prolog frag2)
		(noverlap 1 (if a1
				(or p1 (make-list (length a1)))
			      p1)
			  (if a2
			      (or p2 (make-list (length a2)))
			    p2)))
	  (setf (aux frag2)
		(noverlap 1 (if p1
				(or a1 (make-list (length p1)))
			      a1)
			  (if p2
			      (or a2 (make-list (length p2)))
			    a2))))))

  (mapc #'(lambda (s) (setf (fr s) frag2)) (rets frag1))
  (mapc #'(lambda (s) (setf (fr s) frag2)) (args frag1))
  (setf (args frag2) (nconc (args frag1) (args frag2)))
  (setf (rets frag2) (nconc (rets frag1) (rets frag2)))
  (setf (alterable frag2) (nconc (alterable frag1) (alterable frag2)))
  (setf (body frag2)      (nconc (body frag1)      (body frag2)))
  (setf (epilog frag2)    (nconc (epilog frag1)    (epilog frag2)))
  (setf (wrappers frag2)  (nconc (wrappers frag1)  (wrappers frag2)))
  (setf (impure frag2) (worsen-purity (impure frag1)
				      (impure frag2)))
  frag2)

(cl:defun find-gensyms (tree &optional (found nil))
  (do ((tt tree (cdr tt)))
      ((not (consp tt))
       (if (and (symbolp tt) (null (symbol-package tt)))
           (adjoin tt found)
	 found))
    (setq found (find-gensyms (car tt) found))))

(cl:defun copy-ptrs (sym frag)
  (setf (back-ptrs sym) (make-array 2))
  (setf (nxts sym) nil)
  (setf (fr sym) frag))

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

(cl:defun frag->list (frag)
  (setq frag (copy-list frag))
  (setf (rets frag) (copy-tree (mapcar #'cddr (rets frag))))
  (setf (args frag) (copy-tree (mapcar #'cddr (args frag))))
  (cl:let ((gensyms (find-gensyms frag)))
    (sublis (mapcar #'(lambda (v) (cons v (new-var (root v)))) gensyms)
      (cons gensyms (iterative-copy-tree (cddddr frag))))))

(cl:defun root (symbol)
  (cl:let* ((string (string symbol))
              (pos (position #\- string :start (min (length string) 1))))
    (if pos
	(subseq string 0 (1+ pos))
      (concatenate 'string string "-"))))

(cl:defun list->sym (list frag)
  (cl:let ((s (make-sym :var (car list) :series-var-p (cadr list)
                          :off-line-spot (caddr list)
                          :off-line-exit (cadddr list)
			  )))
    (setf (fr s) frag)
    s))

(cl:defun list->frag1 (list)
  (cl:let* ((alist (mapcar #'(lambda (v) (cons v (gensym (root v)))) (pop list)))
              (frag (list* 'frag :|| 0 nil
                           (nsublis alist (iterative-copy-tree list)))))
    (setf (args frag) (mapcar #'(lambda (s) (list->sym s frag)) (args frag)))
    (setf (rets frag) (mapcar #'(lambda (s) (list->sym s frag)) (rets frag)))
    (values frag alist)))

(cl:defun list->frag (list)
  (cl:multiple-value-bind (frag alist) (list->frag1 list)
    (setf (aux frag)    (makeaux (aux frag)))
    (setf (prolog frag) (makeprolog (prolog frag)))
    (values frag alist)))

;; Special form for defining series functions directly in the internal
;; form. The various variables and the exit label must be unique in
;; the body. The exit label must be END.  Also everything is arranged
;; just as it is in an actual frag structure.

(cl:defun literal-frag (stuff) ;(args rets aux alt prolog body epilog wraprs impure)
  (cl:let ((gensyms (nconc (mapcar #'car (nth 0 stuff))
                           (mapcar #'car (nth 2 stuff)))))
    (dolist (f (nth 5 stuff))
      (when (symbolp f)
	(push f gensyms)))
    (list->frag (cons gensyms stuff))))

(defmacro delete1 (thing list)
  `(setf ,list (delete1a ,thing ,list)))

(cl:defun delete1a (item list)
  (if (eq item (car list))
      (cdr list)
    (do ((l list (cdr l)))
	((null (cdr list)))
      (when (eq item (cadr l))
	(rplacd l (cddr l))
	(return list)))))

(cl:defun pusharg (arg frag)
  (setf (fr arg) frag)
  (setf (args frag) (cons arg (args frag))))

(cl:defun +arg (arg frag)
  (setf (fr arg) frag)
  (setf (args frag) (nconc (args frag) (list arg)))) ;needed by cotruncate

(cl:defun -arg (arg)
  (delete1 arg (args (fr arg))))

(cl:defun +ret (ret frag)
  (setf (fr ret) frag)
  (setf (rets frag) (nconc (rets frag) (list ret)))) ;needed by coerce-to-type

(cl:defun delete-ret (ret frag)
  (delete1 ret (rets frag)))

(cl:defun -ret (ret)
  (delete1 ret (rets (fr ret))))

(cl:defun kill-ret (ret)
  (when (off-line-spot ret)
    (setf (body (fr ret))
          (nsubst-inline nil (off-line-spot ret) (body (fr ret)))))
  (when (and (not (series-var-p ret))
             (every #'(lambda (r) (or (eq r ret) (series-var-p r)))
                     (rets (fr ret))))
    (setf (must-run (fr ret)) t)) ;signal must run to cause any side-effects.
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

(cl:defun yes (call) (declare (ignore call)) t)
(cl:defun no (call) (declare (ignore call)) nil)

);end of eval-when

;;;;                ---- FUNCTIONS FOR CODE WALKING ----

;;; M-&-R takes in a piece of code.  It assumes CODE is a semantic
;;; whole.  Ie, it is something which could be evaled (as opposed to a
;;; disembodied cond clause). It scans over CODE macroexpanding all of
;;; the parts of it, and performing renames as specified by *RENAMES*.
;;; M-&-R puts entries on the variable *RENAMES* which block the
;;; renaming of bound variables.
;;;
;;; M-&-R also calls FN (if any) on every subpart of CODE (including
;;; the whole thing) which could possibly be evaluated.  The result of
;;; consing together all of the results of FN is returned.  Ie, the
;;; result is isomorphic to the input with each part replaced with
;;; what FN returned.  This is done totally by copying.  The input is
;;; not altered.
;;;
;;; In addition, M-&-R checks to see that the code isn't setqing
;;; variables it shouldn't be.
;;;
;;; In order to do the above, M-&-R has to be able to understand
;;; fexprs.  It understands fexprs by having a description of each of
;;; the standard ones (see below).  It will not work on certain weird
;;; ones.
;;;
;;; fexprs are understood by means of templates which are (usually
;;; circular) lists of function names.  These fns are called in order
;;; to processes the various fields of the fexpr.  The template can be
;;; a single fn in which case this fn is called to process the fexpr
;;; as a whole.

(defmacro make-template (head rest)
  `(cl:let ((h (append ',head nil))
	    (r (append ',rest nil)))
     (nconc h r r)))

(defmacro deft (name head rest)
  `(setf (get ',name 'scan-template) (make-template ,head ,rest)))

;; These two should de DEFCONSTANTs, but CMUCL gets confused by
;; circularity.  This happens when you load up series and then try to
;; reload series.  CMUCL tries to compare the old value with the new
;; value to see if they differ.  If the objects are circular, CMUCL
;; gets stuck.

(defvar /expr-template/ (make-template (q) (e)))

(defvar /eval-all-template/ (make-template () (e)))

;; Sample LispWorks' MACROLET walking
;;
;; CL-USER 1> (defmacro foo (x) 
;;              `(bar ,x))
;; FOO
;;
;; CL-USER 2> (walker:walk-form '(macrolet ((bar (x) `(print ,x))) 
;;                                  (foo 3)))
;; (MACROLET ((BAR (X) (LIST (QUOTE PRINT) X))) 
;;   (PRINT 3))
;;
;; CL-USER 3> (walker:walk-form '(macrolet ((bar (x) `(print ,x)))
;;                                 (macrolet ((baz (x) `(bar ,x))) 
;;                                   (baz 3))))
;; (MACROLET ((BAR (X) (LIST (QUOTE PRINT) X)))
;;   (MACROLET ((BAZ (X) (LIST (QUOTE BAR) X))) 
;;     (PRINT 3)))
;;
;; CL-USER 4> (walker::walk-form '(macrolet ((bar (x) `(print ,x)))
;;                                  (macrolet ((baz (x) `(bar ,x)) 
;;                                             (froboz (x) `(format t ,x)))
;;                                    (baz 3) 
;;                                    (froboz 5))))
;; (MACROLET ((BAR (X) (LIST (QUOTE PRINT) X))) 
;;   (MACROLET ((BAZ (X) (LIST (QUOTE BAR) X)) 
;;              (FROBOZ (X) (LIST (QUOTE FORMAT) T X))) 
;;     (PRINT 3) 
;;     (FORMAT T 5)))
;;

;; Expand a MACROLET form
(cl:defun expand-macrolet (code)
  ;; LispWork's WALKER can handle MACROLET. Remove the expanded
  ;; MACROLET still sticking around - Seems correct (see
  ;; above). Optimization hint: Walking once and just removing
  ;; MACROLETs later?
  ;;
  ;; CMUCL's PCL walker behaves like LispWorks.
  ;;
  ;; Clisp's walker doesn't leave the macrolet forms around so it's a
  ;; little easier.
  #+:lispworks
  (loop 
    (unless (and (listp code)
             (case (car code)
               ((cl:macrolet cl:symbol-macrolet) t)
               (t nil)))
      (return code))
    (setq code (cons 'progn (cddr (walker::walk-form code)))))
  #+(and cmu (not :cmu18e))
  (cl:let ((pcl::walk-form-macroexpand-p t))
    (loop 
	(unless (and (listp code)
		     (case (car code)
		       ((cl:macrolet cl:symbol-macrolet) t)
		       (t nil)))
	  (return code))
	(setq code (cons 'progn (cddr (walker::walk-form code))))))
  #+(and cmu :cmu18e)
  (loop 
      (unless (and (listp code)
		   (case (car code)
		     ((cl:macrolet cl:symbol-macrolet) t)
		     (t nil)))
	(return code))
      (setq code (cons 'progn (cddr (walker::macroexpand-all code)))))
  #+clisp
  (loop 
      (unless (and (listp code)
		   (case (car code)
		     ((cl:macrolet cl:symbol-macrolet) t)
		     (t nil)))
	(return code))
      (cl:let ((SYSTEM::*FENV* nil) (SYSTEM::*vENV* nil))
	(setq code (list 'progn (sys::%expand-form code)))))
  #-(or :lispworks :cmu :clisp)
  code)

;; on lispm '(lambda ...) macroexpands to (function (lambda ...)) ugh!

(cl:defun my-macroexpand (original-code)
  (cl:let ((code original-code))
    (setq code (expand-macrolet code))
    (cl:let ((flag (not (frag-p original-code))) head temp)
      (loop
        (when (not (and flag (consp code))) 
          (return code))
        (when (setq temp (or (and (symbolp (setq head (car code)))
                                  (get head 'my-macro))
                             (and (consp head)
                                  (symbolp (car head))
                                  (get (car head) 'my-macro))))
          (setq code (expand-macrolet (cl:funcall temp code))))
        (when (not (symbolp (setq head (car code)))) 
          (return code))
        (loop
          (when (not (get (setq head (car code)) 'series-optimizer)) 
	    (return nil))
          (when (not *in-series-expr*)
            (when (and *not-straight-line-code*
                       (cl:funcall (get head 'returns-series) code))
              (rrs 20 "~%Not straight-line code~%" *not-straight-line-code*))
            (return nil))
          (cl:let ((*call* code))
            (setq code (expand-macrolet (apply (get head 'series-optimizer) (cdr code))))))
        (when (frag-p code) 
          (annotate original-code code) 
          (return code))
        (when (get (car code) 'scan-template)
          (return code))
        ;; protects from any macro side-effects
        (when (eq code original-code) 
	  (setq code (expand-macrolet (iterative-copy-tree code))))
	(multiple-value-setq (code flag) 
          (macroexpand-1 code *env*))
        (setq code (expand-macrolet code))
        ))))

;; special macro-like forms to handle setq forms.  Note psetq is
;; already a macro.

(cl:defun my-lambda-macro (form)
  (if (not (consp (car form)))
      form
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
        (t form)))

(setf (get 'setq 'my-macro) #'my-setq-macro)

(cl:defun m-&-r (code &optional (*fn* nil))
  (cl:let ((*being-setqed* nil))
    (m-&-r1 code)))

(defconst-once /fexprs-not-handled/ '(flet labels #-:lispworks macrolet))

(defconst-once /expr-like-special-forms/ 
  '(multiple-value-call multiple-value-prog1 progn progv the throw
    ;;some simple additions for lispm
    *catch multiple-value-return progw return-list 
    variable-boundp variable-location variable-makunbound)
  "special forms that can be treated like ordinary functions.
   e.g., they have the same template as expr-template.")

(cl:defun not-expr-like-special-form-p (sym)
  (and #-series-ansi(special-form-p sym)
       #+series-ansi(special-operator-p sym)
       (not (member sym /expr-like-special-forms/))))


(cl:defun m-&-r2 (code template)
  (if (not (listp template))
      (cl:funcall template code)
    (mapcar #'(lambda (tm c) (cl:funcall tm c)) template code)))

(cl:defun m-&-r1 (code)
  (cl:let ((*renames* *renames*))
    (setq code (my-macroexpand code))
    (when (symbolp code)
      (setq code (or (cdr (assoc code *renames*)) code)))
    (when *fn*
      (setq code (cl:funcall *fn* code)))
    (if (not (consp code))
	code
      (cl:let* ((head (car code))
		(template (and (symbolp head) (get head 'scan-template))))
	;;(format t "code = ~A~%" code)
        (when (or (member head /fexprs-not-handled/) 
		  (and (not-expr-like-special-form-p head) (null template))
		  (and *in-series-expr* (eq head 'multiple-value-call)))
	  (rrs 6 "~%The form " head " not allowed in SERIES expressions."))
	#+nil
	(let ((*print-circle* t))
	  (format t "template = ~A~%" template))
	  
	(m-&-r2 code
		(if (symbolp head)
		    (or template /expr-template/) 
		  /eval-all-template/))))))


;; This macro-expands everything in the code making sure that all free
;; variables (that are not free in the whole series expression) are
;; appropriately changed to gensyms.  It returns the new code plus
;;
;; (a) list of pairs of internal var gensyms and external values.
;;     Typically, dflow should be inserted from the external values to
;;     ports made with these gensyms.
;;
;; (b) list of pairs of output gensyms and the actual var names they modify.
;; (c) list of vars from the list CHECK-SETQ that are setqed.
;;
;; If the state argument is supplied, it contains lists of input and
;; output info that is used to initialize things. error messages are
;; issued if a series value is used in an improper context.

(cl:defun handle-non-series-stuff (code &optional (state nil) (check-setq nil))
  (cl:let ((free-ins (car state)) (free-outs (cadr state)) (setqed nil))
    (setq code
          (m-&-r code
                 #'(lambda (cd)
                     (when (and check-setq (symbolp cd) *being-setqed*
				(member cd check-setq))
		       (setq setqed (adjoin cd setqed)))
                     (cl:let ((c (if (frag-p cd)
				     (retify cd)
				   cd))
			      temp)
                       (when (sym-p c)
                         (if *being-setqed*
			     (if (setq temp (assoc c free-outs))
				 (setq c (car (cdr temp)))
			       (cl:let ((new (if (setq temp (assoc c free-ins))
						 (car (cdr temp))
					       (new-var 'freeout)))
					(v (car (rassoc cd *renames*))))
                                 (push (cons c (cons new v)) free-outs)
				 (setq c new)))
                           (if (setq temp (assoc c free-ins))
			       (setq c (car (cdr temp)))
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


;;;            ---- TURNING EXPRESSIONS INTO FRAGS (FRAGMENTATION) ----

;; This parses code down to fundamental chunks creating a graph of the
;; expression.  Note that macroexpanding and renaming is applied while
;; this happens. (`fragmentation')

;; FRAGMENTATION
(cl:defun decode-type-arg (type &optional (allow-zero nil))
  (cond ((eq type '*) '*)
        ((eq-car type 'values)
         (when (and (not allow-zero) (equal type '(values)))
	   (ers 62
		"~%The type (VALUES) specified where at least one value required."))
         (subst t '* (cdr type)))
        ((and (not (symbolp type)) (functionp type))
         (ers 70 "~%Function supplied where type expected."))
        (t (list type))))

;; FRAGMENTATION
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

;; FRAGMENTATION
;;
;; note this can assume that the vars are gensyms that they only
;; appear where they are really used.  HERE with the way if works,
;; because it will not catch nested lets!

;; This is only called from isolate-non-series
(cl:defun map-exp (exp vars)
  (cl:let ((prolog-exps nil)
	   (new-aux nil))
    (cl:labels ((map-exp0 (exp) ;can assume exp contains vars
		 (cond ((symbolp exp) exp)
		       ((eq (car exp) 'if) exp)
		       ((not-expr-like-special-form-p (car exp))
			(ers 99 "~%Implicit mapping cannot be applied to the special form "
			     (car exp)))
		       (t `(,(car exp)
			    ,@(mapcar #'(lambda (x)
					  (cond ((contains-any vars x) (map-exp0 x))
						((or (symbolp x)
						     (constantp x)
						     (eq-car x 'cl:function)
						     (eq-car x 'cl:lambda)) x)
						(t (cl:let ((v (new-var 'm)))
                                                     (push v new-aux)
                                                     (push `(setq ,v ,x) prolog-exps)
						     v))))
				      (cdr exp)))))))
      (declare (dynamic-extent #'map-exp0))
      (cl:let ((body-exp (map-exp0 exp)))
        (values (nreverse prolog-exps) body-exp (nreverse new-aux))))))

;; FRAGMENTATION
;;
;; note that this does implicit mapping when appropriate.  Note also
;; that it only maps the absolute minimum necessary.  This is to
;; ensure that things will come out the same no matter how they were
;; syntactically expresssed in the input.  Also mapping of special
;; forms other than if is not allowed. If it were it could lead to all
;; kinds of problems with binding scopes and scopes for gos and the
;; like.

;; This is called only from fragify
(cl:defun isolate-non-series (n code)
  (cl:multiple-value-bind (exp free-ins free-outs)
      (handle-non-series-stuff code)
    (cl:let* ((vars (n-gensyms n (symbol-name '#:out-)))
	      (mapped-inputs nil)
	      (frag (make-frag :aux (makeaux (mapcar #'(lambda (v) (list v t)) vars)))))
      (dolist (entry free-ins)
        (cl:let ((arg (make-sym :var (car entry))))
           (when (and *series-implicit-map* (series-var-p (cdr entry)))
             (push (car entry) mapped-inputs)
             (setf (series-var-p arg) t))
          (+arg arg frag)
          (+dflow (cdr entry) arg)))
      (dolist (v vars)
        (+ret (make-sym :var v :series-var-p mapped-inputs) frag))
      (when (zerop n)
	(setf (must-run frag) t))
      (if (null mapped-inputs)
          (setf (prolog frag) (makeprolog (list (make-general-setq vars exp))))
	(cl:multiple-value-bind (prolog-exps body-exp new-aux) 
            (map-exp exp mapped-inputs)
          (when prolog-exps 
	    (setf (prolog frag) (makeprolog prolog-exps))
	    (dolist (a new-aux)
	      (add-aux frag a t)))
            (setf (body frag) (list (make-general-setq vars body-exp)))))
      (dolist (entry free-outs)
        (cl:let ((new (make-sym :var (car entry) 
				:series-var-p mapped-inputs))
                 (v (cdr entry)))
          (when (not (find (car entry) (args frag) :key #'var))
	    (add-aux frag (car entry) t))
          (setf (free-out new) v)
          (+ret new frag)
          (rplacd (assoc v *renames*) new)))
      (+frag frag))))

;; FRAGMENTATION
(cl:defun fragify (code type)
  (cl:let* ((expansion (my-macroexpand code))
	    (ret (when (symbolp expansion)
		   (cdr (assoc expansion *renames*))))
	    (types (decode-type-arg type t)))
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
                                                         (car (rets (fragify form t))))
                                                     (cdr expansion))))
                              (when (and (cdr rets) (some #'series-var-p rets))
                                (rrs 7 "~%VALUES returns multiple series:~%" code))
                              (annotate code (pass-through-frag rets))))
                           (t (annotate code (isolate-non-series
                                              (if (listp types)
						  (length types)
						1)
                                              expansion)))))))

;; Have to be careful not to macroexpand things twice. If you did, you
;; could get two copies of some frags on *graph*. Note that a type of
;; '* means any number of arguments.
(cl:defun retify (code &optional (type t))
  (if (sym-p code)
      code ;might have been retified/fragified before.
    (cl:let* ((expansion (my-macroexpand code))
	      (ret (when (symbolp expansion)
		     (cdr (assoc expansion *renames*)))))
      (if (sym-p ret)
	  ret
	(car (rets (fragify expansion type)))))))

;;; What the following is doing with the free variables may not be
;;; quite right.  All in all, it is pretty scary if you refer to local
;;; lexical vars in a fn in a series expression.  HERE Note that for
;;; the moment, Series does not realize that you have used a variable
;;; if this is the only way you use it.
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


;;;                        ---- MACROEXPANSION TEMPLATES ----

;; The following are the fns allowed in templates.

(cl:defun fun (code) (if (not (consp code)) code (process-fn code)))

;; This handles binding lists for FLET.

(cl:defun fbind-list (args)
  (mapcar #'fun args))

;; This handles binding lists for LET.
(cl:defun bind-list (args sequential &aux (pending nil))
  (prog1 (mapcar #'(lambda (arg)
                     (cl:let* ((val-p (and (consp arg) (cdr arg)))
			       (new-val (when val-p
					  (m-&-r1 (cadr arg))))
			       (var (if (consp arg)
					(car arg)
				      arg)))
                       (if sequential
			   (push (list var) *renames*)
			 (push (list var) pending))
                       (if val-p
			   (list (car arg) new-val)
			 arg)))
                 args)
    (setq *renames* (append pending *renames*))))

(cl:defun arg-list (args)
  (mapcar #'(lambda (arg)
              (cl:let* ((vars (vars-of arg))
			(val-p (and (consp arg) (cdr arg)))
			(new-val (when val-p
				   (m-&-r1 (cadr arg)))))
                (setq *renames* (append (mapcar #'list vars) *renames*))
                (if val-p
		    (list* (car arg) new-val (cddr arg))
		  arg)))
          args))

(cl:defun q   (code) code)
(cl:defun e   (code) (m-&-r1 code))
(cl:defun ex  (code)
  (cl:let* ((*not-straight-line-code* *in-series-expr*)
              (*in-series-expr* nil))
    (m-&-r2 code /expr-template/)))
(cl:defun el  (code)
  (cl:let* ((*not-straight-line-code* *in-series-expr*)
              (*in-series-expr* nil))
    (m-&-r1 code)))
(cl:defun elm  (code)
  (if *series-implicit-map*
      (m-&-r1 code)
    (el code)))
(cl:defun s   (code) (cl:let ((*being-setqed* t)) (m-&-r1 code)))
(cl:defun b   (code) (bind-list code nil))
(cl:defun b*  (code) (bind-list code t))
(cl:defun a   (code) (arg-list code))
(cl:defun lab (code) (if (symbolp code) code (el code)))
(cl:defun f   (code) (fbind-list code))

(cl:defun compiler-let-template (form)
  (cl:let ((symbols (mapcar #'(lambda (p) (if (consp p) (car p) p)) (cadr form)))
	   (values (mapcar #'(lambda (p) (when (consp p) (eval (cadr p)))) (cadr form)))
	   (body (cddr form)))
    (progv symbols values
      (e (if (null (cdr body))
	     (car body)
	   (list* 'let nil body))))))

(setf (get 'compiler-let 'scan-template) #'compiler-let-template)

;;; templates for special forms.  Note that the following are not
;;; handled
;;;
;;;   COMPILER-LET FLET LABELS MACROLET but must not macroexpand.
;;;
;;; FLET and DECLARE in particular are macros in lucid and messed
;;; things up by expanding at the wrong time.

(deft                block (q q)  (el))
(deft                catch (q e)  (el))
;;
;; I (RLT) changed this.  I don't think the rest of a declaration
;; should be macro-expanded at all.  Is this right?  Did I do this
;; right?

;;(deft declare (q) (ex)) ;needed by xerox cl
(deft              declare (q)    (q))

(deft            eval-when (q q)  (e))
(deft             function (q fun)())
(deft                   go (q q)  ())
(deft                   if (q e)  (elm))
(deft               cl:let (q b)  (e))
(deft              cl:let* (q b*) (e))
(deft  multiple-value-call (q)    (e))
(deft multiple-value-prog1 (q)    (e))
(deft                progn (q)    (e))
(deft                progv (q)    (e))
(deft                quote (q q)  ())
(deft          return-from (q q)  (e))
(deft                 setq (q)    (s e))
(deft              tagbody (q)    (lab))
(deft                  the (q q)  (e))
(deft                throw (q)    (e))
(deft       unwind-protect (q)    (el))

(deft               lambda (q a)  (e))

(deft                 flet (f)    (e))
(deft         compiler-let (q)    (e))
(deft             macrolet (q)    (e))
(deft               labels (f)    (e))
(deft                 type (q q)  (e))
(deft                  setf (q)   (e))   ;fixes weird interaction with lispm setf 

(deft               locally (q)   (e))

#+symbolics
(cl:eval-when (eval load)
  (cl:defun WSLB (list)
    (prog1 (EX list) (push (list (car list)) *renames*)))
  (deft                LET-IF (Q E B) (E))
  (deft   scl:WITH-STACK-LIST (Q WSLB) (E))
  (deft  scl:WITH-STACK-LIST* (Q WSLB) (E)))

;;;                        ---- TYPE HANDLING ----

;; TYPING
(cl:defun some-series-type-p (type)
  (cl:flet ((s-car-p (typ)
              (eq-car typ 'series)))
    (declare (dynamic-extent #'s-car-p))
    (or (s-car-p type)
        (and (or (eq-car type 'or)
                 (eq-car type 'and))
             (some #'s-car-p (cdr type))))))

;; TYPING
(cl:defun deserialize-type (type)
  (cl:let ((cartyp (car type)))
    (cl:flet ((upgrade-type (typ)
                  (if (eq-car typ 'series)
                      (cadr typ)
                    typ)))
      (declare (dynamic-extent #'upgrade-type))
      (if (eq cartyp 'series)
          (cadr type)
        (if (or (eq cartyp 'or) 
                (eq cartyp 'and))
            (cons cartyp (mapcar #'upgrade-type (cdr type)))
          type)))))

;; TYPING
(cl:defun truefy-type (type)
  (cl:flet ((star2t (typ)
                (if (eq typ '*)
                    t
                  typ)))
      (declare (dynamic-extent #'star2t))
      (typecase type
        (list (cl:let ((cartyp (car type)))
                (if (or (eq cartyp 'or) 
                        (eq cartyp 'and))
                    (cons cartyp (mapcar #'star2t (cdr type)))
                  type)))
        (t (star2t type)))))

;; TYPING
;; this is also used by PROTECT-FROM-SETQ in an odd way.
(cl:defun coerce-to-type (type ret)
  (when (eq type 'series) 
    (setq type '(series t)))
  (when (not (eq type t))
    (when (and (not (some-series-type-p type))
               (series-var-p ret))
      (wrs 30 t "~%Series encountered where not expected."))
    (when (some-series-type-p type)
      (when (not (series-var-p ret))
	(wrs 31 t "~%Non-series value encountered where series expected."))
      (setq type (deserialize-type type))
      (setq type (truefy-type type)))
    (cl:let ((aux (find-aux (var ret) (aux (fr ret)))))
      (when (and aux (not (subtypep (cadr aux) type)))
	(setf (cadr aux) type)))))

;; This is only used by fragify
(cl:defun coerce-to-types (types frag)
  (when (not (eq types '*))
    (cl:let ((n (length types))
	     (current-n (length (rets frag))))
      (cond ((= n current-n))
            ((< n current-n)
             (mapc #'(lambda (r) (when (not (free-out r)) (kill-ret r)))
                   (nthcdr n (rets frag))))
            (t (dolist (v (n-gensyms (- n current-n) (symbol-name '#:xtra-)))
                 (+ret (make-sym :var v) frag)
		 (add-literal-aux frag v t nil))))
      (mapc #'coerce-to-type types (rets frag))))
  frag)


;;;; ---- PHYSICAL REPRESENTATIONS FOR SERIES AND GENERATORS ----

;;; The following structure is used as the physical representation for
;;; a series.  It is a structure so that it can print itself and get
;;; read in.  The only operation on it is to get it to return a a
;;; generator object.  The only operation on a generator object is
;;; NEXT-IN.

;;; Physical series are of two kinds basic and image. A basic series
;;; has three parts
;;;
;;; GEN-FN is a fn that generates new values.  When called with no
;;; args, must either return a list of the next value, or nil
;;; indicating no more values to return.  (If there is an alter
;;; function, then each value is actually a list of the fundamental
;;; value and any additional information needed by the alter function.
;;; NEXT-IN only returns the fundamental value in any case.)
;;;
;;; DATA-SO-FAR cons of NIL and data generated by GEN-FN so far.  The
;;; last cdr is a flag that tells you whether the end has been
;;; reached.  If it is t there is still more to get, if it is NIL you
;;; are done.  (The NIL car is needed so that new elements can allways
;;; be added by side effect.)
;;;
;;; ALTER-FN is a fn that alters elements (or NIL if none).  Must be a
;;; function that when called with a new item as its first arg and any
;;; additional information computed by the GEN-FN as its other
;;; arguments does the alteration.
;;;
;;; Image series compute one series from another without requiring any
;;; mutable internal state.  The also have four parts.
;;;
;;; BASE-SERIES the series the image series is based on.  The elements
;;; of the image are some simple function of the elements of the base.
;;; (The base can be another image series.)
;;;
;;; IMAGE-FN is a function with no changing internal state that will
;;; get the next full item of the series given a generator of the base
;;; series.  It must behave the same as BASIC-DO-NEXT-IN.
;;;
;;; IMAGE-DATUM Some non-null value that can be used by the IMAGE-FN
;;; when deciding what to do.  This often saves having to have the
;;; IMAGE-FN be a closure. It is passed as the second argument to the
;;; IMAGE-FN.
;;;
;;; ALTER-FN same as for a basic series.

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defmacro make-phys (&key (gen-fn nil) (alter-fn nil) (data-list t))
  `(make-basic-series :gen-fn ,gen-fn :alter-fn ,alter-fn
                      :data-so-far (cons nil ,data-list)))

(cl:defun series-p (x)
  (or (basic-series-p x) (image-series-p x)))

(deftype series (&rest type)
  (declare (ignore type))
  `(satisfies series-p))

(deftype series-element-type (var)
  (declare (ignore var))
  t)

;;; A generator is a data structure with the following parts.  For
;;; speed in symbolics lisp, it is implement as a list.
;;;
;;; GEN-BASE is the series the generator is generating the elements
;;; of.
;;;
;;; GEN-BASE is one of two things depending on what kind of series the
;;; GEN-BASE is.
;;;
;;;   For basic-series, it starts out as the data-so-far and is cdr'ed
;;;   down as the elemnts are used.  Also additional elements are
;;;   tagged on to the end as needed.  Sharing is used so that when
;;;   elements are added here, they are automaticaly added onto the
;;;   data-so-far of the series itself and to any other generators for
;;;   this series as well.
;;;
;;;   For image-series, it is a generator for the the IMAGE-BASE.
;;;
;;;   In either of the cases above, the GEN-BASE becomes NIL when the
;;;   generator is exhausted.
;;;
;;; CURRENT-ALTER-INFO is the list of information that is needed to
;;; alter the last element generated.  (If the base has no alter-fn,
;;; then this is nil.)

(defstruct (generator (:conc-name nil) (:type list))
  gen-state gen-base (current-alter-info nil))

#+(or :lispworks :cmu :scl :excl :sbcl)
(deftype generator () 'cons)

(cl:defun generator (s)
  (make-generator
    :gen-base s
    :gen-state
     (cond ((image-series-p s) (generator (image-base s)))
           ((basic-series-p s) (data-so-far s))
           (t (ers 60 "~%GENERATOR applied to something that is not a series.")))))

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
           (t (ers 60 "~%GENERATOR applied to something that is not a series.")))))

;; This function interfaces to generators.  No optimization ever
;; happens to generators except in the function PRODUCING.  The next
;; element of the generator is returned each time DO-NEXT-IN is
;; called.  If there are no more elements, the functional argument is
;; funcalled.  It is an error to call DO-NEXT-IN again later.

;; This returns the next full entry in a generator, or sets the
;; gen-state to NIL indicating the generator is exhausted.
(cl:defun basic-do-next-in (g)
  (when (gen-state g)
    (cl:let ((full-current
                 (cond ((image-series-p (gen-base g))
                        (prog1 (cl:funcall (image-fn (gen-base g))
                                             (gen-state g)
                                             (image-datum (gen-base g)))
                               (when (null (gen-state (gen-state g)))
                                 (setf (gen-state g) nil))))
                       (t (when (eq (cdr (gen-state g)) t)
                            (setf (cdr (gen-state g))
                                  (cl:funcall (gen-fn (gen-base g)))))
                          (pop (gen-state g))
                          (car (gen-state g))))))
      (cond ((alter-fn (gen-base g))
             (setf (current-alter-info g) (cdr full-current))
             (car full-current))
            (t full-current)))))

(cl:defun do-next-in (g at-end &optional (alter nil alterp))
  (cl:let ((current (basic-do-next-in g)))
    (cond ((null (gen-state g)) (cl:funcall at-end))
          (alterp
           (apply (cond ((alter-fn (gen-base g)))
                        (t (ers 65 "~%Alter applied to an unalterable form.")))
                  alter (current-alter-info g)))
          (t current))))

(defmacro next-in (generator &rest actions)
  `(do-next-in ,generator #'(lambda () ,@ actions)))



;; The following is an example of an image function.  It selects the
;; datum-th part of the full item of the base series as the item of
;; the image series.
(cl:defun image-of-datum-th (g datum)
  (nth datum (basic-do-next-in g)))

  (cl:defun values-lists (n series-of-lists &optional (alterers nil))
    (multiple-value-polycall
      #'(lambda (i)
	  (make-image-series :alter-fn (pop alterers)
			     :image-fn #'image-of-datum-th
			     :image-datum i
			     :image-base series-of-lists))
      (n-integer-values n)))

#-cmu
(cl:defun print-series (series stream depth)
  (cl:let ((generator (generator series)))
    (write-string "#Z(" stream)
    (do ((first-p t nil)
         (i (cond (*print-length*) (t -1)) (1- i)))
        (nil)
      (cl:let ((element (next-in generator (return nil))))
        (when (not first-p)
	  (write-char #\space stream))
        (when (zerop i)
	  (write-string "..." stream) (return nil))
        (write element :stream stream
               :level (when *print-level*
			(- *print-level* depth)))))
    (write-char #\) stream)))

;; If we have a Common Lisp pretty-printer, we should use that to
;; print out series because that probably does a better job than this
;; routine would.  We basically print out series as if it were a list
;; of items.
#+(or cmu scl)
(cl:defun print-series (series stream depth)
  (cl:let ((generator (generator series))
	   (first-p t)
	   (items 0))
    (pprint-logical-block (stream nil :prefix "#Z(" :suffix ")")
      (loop
	  (cl:let ((element (next-in generator (return nil))))
	    (if first-p
		(setf first-p nil)
		(write-char #\Space stream))
	    (if (and *print-length*
		     (>= items *print-length*))
		(progn
		  (princ "..." stream)
		  (pprint-newline :fill stream)
		  (return))
		(progn
		  (write element :stream stream
			 :level (when *print-level*
				  (- *print-level* depth)))
		  (incf items)
		  (pprint-newline :fill stream))))))))

)					; end of eval-when

;;;;                  ---- TURNING AN EXPRESSION INTO A GRAPH ----

;;; The form below has to be called to set things up right, before
;;; processing of a series expression can proceed.

;;; should have some general error catching thing but common lisp has
;;; none.

(defmacro starting-series-expr (call body)
  `(cl:let ((*renames* nil)
	    (*user-names* nil)
	    (*not-straight-line-code* nil)
	    (*in-series-expr* ,call))
     ,body))

;; assumes opt result cannot be NIL
(defmacro top-starting-series-expr (call opt non-opt)
  (cl:let ((res (gensym)))
  `(cl:let ((,res (catch :series-restriction-violation (starting-series-expr ,call ,opt))))
     (if ,res
	 (values ,res t)
       (values ,non-opt nil)))))

;; FRAGMENTATION
;; Main fragmentation function

;; This is only called from process-top
(cl:defun graphify (code &optional (return-type '*))
  (cl:let ((*graph* nil))
    (fragify code return-type)
    *graph*))


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

(cl:defun vars-of (arg)
  (cond ((member arg lambda-list-keywords) nil)
        ((not (consp arg)) (list arg))
        (t (cons (if (consp (car arg))
		     (cadar arg)
		   (car arg))
                 (copy-list (cddr arg))))))

;; Important that this allows extra args and doesn't check.
(cl:defun apply-frag (frag values)
  (mapc #'(lambda (v a) (+dflow (retify v) a)) values (args frag))
  (+frag frag))

(cl:defun funcall-frag (frag &rest values)
  (apply-frag frag values))

;; Macroexpansion may result in unexpected arcana we should let through.
(defconst-once /allowed-generic-opts/ 
  '(optimize 
    #+:lispworks (CLOS::VARIABLE-REBINDING)
    #+:clisp system::read-only))

;; This takes a list of forms that may have documentation and/or
;; declarations in the initial forms.  It parses the declarations and
;; returns the remaining forms followed by the parsed declarations.
;; The list allowed-dcls specifies what kinds of declarations are
;; allowed. Error messages are given if any other kind of declaration
;; is found. Each allowed-dcl must be one of the symbols declared
;; special below.
(cl:defun decode-dcls (forms allowed-dcls)
  (cl:let ((doc nil) (ignores nil) (types nil) (props nil)
           (opts nil) (off-line-ports nil) (no-complaints nil)
           (generic-opts nil))
      (declare (special doc ignores types props opts off-line-ports no-complaints generic-opts))
    (loop
      (when (and (member 'doc allowed-dcls)
                 (null doc) (stringp (car forms)) (cdr forms))
        (setq doc (pop forms)))
      (when (not (eq-car (car forms) 'declare)) (return nil))
      (dolist (d (cdr (pop forms)))
        (cond ((and (eq (car d) 'type)
                    (member 'types allowed-dcls))
               (dolist (v (cddr d)) (push (cons v (cadr d)) types)))
              ((and (or (member (car d) /short-hand-types/)
                        (and (listp (car d)) (member (caar d) /short-hand-types/)))
                    (member 'types allowed-dcls))
               (dolist (v (cdr d)) (push (cons v (car d)) types)))
              ((and (eq (car d) 'optimizable-series-function)
                    (member 'opts allowed-dcls))
               (setq opts (cond ((cadr d)) (t 1))))
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
               (if (member (car d) /allowed-generic-opts/)
                   (setq generic-opts (nconc generic-opts (list d)))
                 (rrs 1 "~%The declaration " d " blocks optimization.")))
              (t (setq no-complaints (nconc no-complaints (list d)))))))
    (values-list (cons forms (mapcar #'symbol-value allowed-dcls)))))

;;;;                         ---- MERGING A GRAPH ----

;;; This proceeds in several phases
;;;  (1) check for series/non-series type conflicts.  This operates in
;;;      one of two different ways depending on the value of
;;;      *SERIES-IMPLICIT-MAP*.  If this control variable is non-nil then:
;;;      (a) If a frag that does not process any series at all 
;;;          (i.e., came from totally non-series stuff in the source) receives
;;;          a series for any of its inputs, then it was implicitly mapped
;;;          by isolate-non-series. (Note, if the output is not connected to 
;;;          anything, it is marked as being forced to run.)
;;;      (b) If a non-series is supplied where a series is expected, we
;;;          coerce it into an infinite series of the single value.
;;;      Whether or not *SERIES-IMPLICIT-MAP* is non-nil we then:
;;;      (a) if a series is supplied where a non-series is expected,
;;;          issue a restriction violation warning.
;;;          It would not be in the spirit of things to create a physical series.
;;;          And would be very hard to boot.
;;;      (b) if a non-series is supplied where a series is expected,
;;;          assume that this non-series item is really a physical series and
;;;          add a physical interface.  (Note this cannot happen when
;;;          *SERIES-IMPLICIT-MAP* is non-nil.)
;;;  (2) Do substitutions to get rid of trivial frags representing constant values and
;;;      references to variables.
;;;  (2.5) get rid of dead code.
;;;  (3) Scan the graph to find places where the expression can be split because it is
;;;      in disconnected places or there is an isolated dflow touching a non-series
;;;      or off-line port.  If the graph cannot be split, then it consists solely of
;;;      dflow connecting on-line ports.  A list structure is created showing all of
;;;      the split points that will be merged in the next step.
;;;  (4) The structure created above is evaluated doing a sequence of merge steps
;;;      that reduces the whole expression to a single frag.
;;;
;;; since implicit mapping is a bit tricky, but quite possibly the
;;; must useful single part of the series macro package, it deserves a
;;; few words.  To understand what happens, some initial definitions
;;; are necessary.  First, a compile-time-known series function is one
;;; of the predefiend series functions or a function DEFUNed with an
;;; optimizable-series-function declaration. (There may be lots of
;;; other functions around manipulating series, but that is not
;;; relevant to the implicit mapping that is going on here.) A
;;; compile-time-known series value is a series output of a
;;; compile-time-known series function or such an output bound to a
;;; variable by one of the forms below.  A compile-time-known series
;;; input is a series input of a compile-time-known series function.
;;;
;;; Every non-compile-time-known function that receives a
;;; compile-time-known series value as an input is mapped.  Note that
;;; once a non-compile-time-known function is mapped, the result is a
;;; compile-time-known series function this may cause ;more mapping to
;;; occur. Special forms are never mapped.  This is flagged as an
;;; error if it appears that it needs to be done.  Note that
;;; non-series functions that appear in a context where their value is
;;; not used, are flagged to indicate that they must be run anyway.
;;; This carries through it they are mapped.
;;;
;;; In addition to the above, any non-series value that appears where
;;; a series is expected is automatically converted into an infinite
;;; series of that value.  If you side-effects are involved, you might
;;; want multiple evaluation.  However, you will have to specifically
;;; indicate this using map-fn or something.  (This may not be the
;;; best default in many ways, but it is the only way to make things
;;; come out the same without depending on the exact syntactic form of
;;; the input.  For instance note that INCF expands into a let in some
;;; lisps and this would force the let to be in a separate expression
;;; even though it does not look like it at first glance.)

;;; As an example of all the above consider the following.
#|
(let* ((x (car (scan '((1) (2) (3)))))
       (y (1+ x))
       (z (collect-sum (* x y))))
  (print (list x y 4))
  (print z)
  (collect (list x (catenate #Z(a) (gensym)))))
|#
;;; is equivalent to
#|
(let* ((x (#Mcar (scan '((1) (2) (3)))))
       (y (#M1+ x))
       (z (collect-sum (#M* x y))))
  (collect-last (#Mprint (#Mlist x y (series 4))))
  (print z)
  (collect (#Mlist x (catenate #Z(a) (series (gensym))))))
|#

;;; Note that compile-time-known series functions are never
;;; mapped. Therefore (collect (collect (scan (scan x)))) is not
;;; equivalent to (collect (mapping ((y (scan x))) (collect (scan
;;; y)))).  You have to write the latter if you want it.  Also while
;;; series/non-series conflicts are less likely to arise, there is no
;;; guarantee that the restrictions will be satisfied after implicit
;;; mapping is applied.


;;;                        UTILITIES

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (cl:defun do-alter-prop (value gen)
    (if (alter-fn (gen-base gen))
        (cons value (current-alter-info gen))
       value))

  ;; alter-info has priority
  (cl:defun out-value (ret alter-prop flag-off-line?)
    (cl:let* ((var (var ret))
              (alter-info (cdr (assoc var (alterable (fr ret))))))
      (values (cond (alter-info `(list ,var ,@ (cdr alter-info)))
                    (alter-prop `(do-alter-prop ,var ,(cdr alter-prop)))
                    ((and flag-off-line? (off-line-spot ret)) `(list ,var))
                    (t var))
              (cond (alter-info
                     (cl:let ((alter (new-var 'alter)))
                       `#'(lambda (,alter ,@(cdr alter-info))
                            ,(subst alter '*alt* (car alter-info)))))
                    (alter-prop `(alter-fn ,(car alter-prop)))))))

  ;; This is used to allow a fragment to accept a physical series in
  ;; lieu of one computed be another frag.
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
      (add-aux frag var t)
      (add-nonliteral-aux frag generator 'generator `(generator ,series))
      (if (not off-line-spot)
          (push `(setq ,var (next-in ,generator (go ,end))) (body frag))
        (setf (body frag)
              (nsubst-inline
               `((setq ,var (next-in ,generator
                                     (go ,(cond (off-line-exit) (t end))))))
               off-line-spot (body frag))))
      generator))


  ;; This turns a series output into a non-series output returning a
  ;; physical series. (Note this assumes that if alterability is being
  ;; propogated, the corresponding input has already been changed
  ;; using add-physical-interface.  Alter-prop is a cons of the new
  ;; input var (a physical series) and the var holding the generator.)

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
	  (add-literal-aux frag new-list 'list '())
 	  (add-aux frag new-out 'series)
          (if (not off-line-spot)
              (setf (body frag) (nconc (body frag) new-body-code))
            (setf (body frag)
                  (nsubst-inline new-body-code off-line-spot (body frag))))
          (push new-epilog-code (epilog frag))
          frag))))
) ; end of eval-when

;;;;               (1) CHECK-FOR SERIES/NON-SERIES CONFLICTS.

;; This is only called from do-coercion
(cl:defun series-coerce (a)
  (when (off-line-spot a)
    (nsubst nil (off-line-spot a) (fr a)))
  (setf (series-var-p a) nil))

;; This is only called from do-coercion
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

;; This is only called from mergify
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
              (setf (series-var-p ret) t)
              (dolist (aa (nxts ret))
                (when (not (series-var-p aa))
                  (rrs 14 "~%The optimizable series function input "
                       (code (fr ret))
                       " used as a series value by~%" (code (fr a))
                       "~%and as a non-series value by~%"
                       (code (fr aa)))))))
          (when (and (not (series-var-p ret)) (series-var-p a))
            (cond (*series-implicit-map* (series-coerce a))
                  (t (wrs 28 nil "~%Non-series to series data flow from:~%"
                          (code (fr ret)) "~%to:~%" (code (fr a)))
                     (add-physical-interface a))))))
      ;;might have to de-series if a physical interface was required for
      ;;every series input.
      (maybe-de-series f))
    (dolist (f lambda-arg-frags)
      (when (and (series-var-p (car (rets f)))
                 (cdr (nxts (car (rets f)))))
        (add-dummy-source-frag f)))))

;;;;                     (2) DO SUBSTITUTIONS

(eval-when (:compile-toplevel :load-toplevel :execute)

(cl:defun not-contained (items &rest things)
  (cl:let ((found-once nil))
    (cl:labels ((look-at (tree)
                 (cond ((symbolp tree)
			(when-bind (found (car (member tree items)))
			  (push found found-once)))
                       (t (do ((tt tree (cdr tt)))
			      ((not (consp tt)) nil)
			    (look-at (car tt)))))))
      (declare (dynamic-extent #'look-at))
      (dolist (thing things)
        (look-at thing)))
    (set-difference items found-once)))

(cl:defun not-contained-twice (items &rest things)
  (cl:let ((found-once nil) (found-twice nil))
    (cl:labels ((look-at (tree)
                 (cond ((symbolp tree)
			(when-bind (found (car (member tree items)))
			  (if (member found found-once)
			      (pushnew found found-twice)
			    (push found found-once))))
                       (t (do ((tt tree (cdr tt)))
			      ((not (consp tt)) nil)
			    (look-at (car tt)))))))
      (declare (dynamic-extent #'look-at))
      (dolist (thing things)
        (look-at thing)))
    (values (set-difference items found-twice) (set-difference items found-once))))

) ; end of eval-when

;; This is VERY conservative.  Note if you substitute variables too
;; freely, you can run into troubles with binding scopes and setqs of
;; the variables in other places, but just using temporary vars is
;; guaranteed to have the right semantics all of the time.  Any decent
;; compiler will then minimize the number of variables actually used
;; at run time.

;; This is only called from mergify
(cl:defun do-substitution (&aux code ret killable)
  (dofrags (f)
    (when (and (= (length (rets f)) 1)
               (not (off-line-spot (car (rets f))))
               (null (args f))
               (null (epilog f))
               (= 1 (prolog-length (setq code (prolog-append (prolog f) (body f)))))
               (eq (var (setq ret (car (rets f)))) (setq-p (setq code (car (first-prolog-block code)))))
               (or (constantp (setq code (caddr code)))
                   (and (eq-car code 'function) (symbolp (cadr code)))))
      (setq killable (not (null (nxts ret))))
      (dolist (arg (nxts ret))
        (cond ((and (not (off-line-spot arg))
                    (not (contains-p (var arg) (rets (fr arg))))
                    (cond ((or (and (eq-car code 'function) (symbolp (cadr code)))
			       (and (symbolp code) (null (symbol-package code))) ;gensyms
			       (keywordp code) ;keywords
			       (characterp code)
                               (numberp code) (null code) (eq code t)) t)
                          ((constantp code)
                           (and (null (cdr (nxts (car (rets f)))))
                                (not-contained-twice (list (var arg))
                                                     (list (prolog (fr arg))
                                                           (body (fr arg))
                                                           (epilog (fr arg))))))))
               (nsubst code (var arg) (fr arg))
               (-dflow ret arg)
               (-arg arg))
              (t (setq killable nil))))
      (when killable
	(-frag f)))))

;;;;                     (2.5) KILL DEAD CODE

;; This is only called from kill-dead-code
(cl:defun reap-frag (frag)
  (dolist (a (args frag))
    (cl:let ((r (prv a)))
      (-dflow r a)
      (when (null (nxts r)) (kill-ret r))))
  (setq *graph* (delete frag *graph*)))

;; This is only called from mergify
(cl:defun kill-dead-code ()
  (setq *graph* (nreverse *graph*))
  (dofrags (f)
    (dolist (r (rets f))
      (when (and (free-out r) (null (nxts r)))
	(kill-ret r)))
    (when (not (or (rets f) (must-run f)))
      (reap-frag f)))
  (setq *graph* (nreverse *graph*)))


;;;;                      --- PORT HANDLING ---

(cl:defun find-on-line (syms)
  (do ((s syms (cdr s)) (r nil))
      ((null s) (nreverse r))
    (when (and (series-var-p (car s)) (null (off-line-spot (car s))))
      (push (car s) r))))

(cl:defun make-inputs-off-line (frag off-line-exit)
  (dolist (in (find-on-line (args frag)))
    (when (not (marked-p 1 (fr (prv in)))) ;needed by check-termination
      (cl:let ((-X- (new-var '-xx-)))
        (setf (off-line-spot in) -X-)
        (setf (off-line-exit in) off-line-exit)
        (setf (body frag) `(,-X- ,@(body frag)))))))

(cl:defun make-outputs-off-line (frag)
  (dolist (out (find-on-line (rets frag)))
    (when (or (null (nxts out))
              (some #'(lambda (in)
                        (not (marked-p 1 (fr in)))) ;needed by check-termination
                    (nxts out)))
      (cl:let ((-X- (new-var '-x-)))
        (setf (off-line-spot out) -X-)
        (setf (body frag) `(,@(body frag) ,-X-))))))

(cl:defun make-ports-off-line (frag off-line-exit)
  (make-inputs-off-line frag off-line-exit)
  (make-outputs-off-line frag))

;;;;                      --- CODE GENERATION ---

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; This tries to get a correct init in situations where NIL won't do.

;; This function converts a type to a "canonical" type.  Mainly meant
;; to handle things that have been deftype'd.  We want to convert that
;; deftype'd thing to the underlying Lisp type.  
#+(or cmu scl)
(cl:defun canonical-type (type)
  (kernel:type-specifier (c::specifier-type (if (and (not (atom type))
                                                     (eq 'quote (first type)))
                                                (cdr type)
					      type))))
#+CLISP
(cl:defun canonical-type (type)
  (ext:type-expand type))

#-(or cmu scl CLISP)
(cl:defun canonical-type (type)
  type)

;; toy@rtp.ericsson.se:
;; Actually, to be correct, we need to be more careful about how we
;; init things because CLtL2 says it's wrong and CMU Lisp complains
;; and fails if we don't init things correctly.  In particular, we
;; need to handle the case of arrays, strings, and "(member t)" that
;; is used in a few places.  I think all cases that occur in the test
;; suite are handled here.

;; fdmm: LOCALLY makes this unnecessary. Return NIL whenever not sure it works.

(cl:defun init-elem (type)
  (cl:let ((var-type (canonical-type type)))
    (cond ((subtypep var-type 'complex)
           (cond ((atom var-type) ;; Plain old complex
                  #C(0.0 0.0))
                 ((and (eq-car var-type 'complex)
                       (cdr var-type))
                  ;; Can find elem-type.
                  (complex (coerce-maybe-fold 0 (cadr var-type))))
                 (t
		  nil
                  ;; BUG: Can't find elem-type, hope COERCE knows better.
		  #+:ignore
                  (coerce-maybe-fold 0 var-type))))
          ((subtypep var-type 'number)
           (coerce-maybe-fold 0 var-type))
          ((subtypep var-type 'cons)
	   nil
	   #+:ignore
           (cond ((atom var-type)
                  ''(nil))
                 ((and (eq-car var-type 'cons) (cdr var-type))
                  `',(cons (init-elem (cadr var-type))
                        (init-elem (caddr var-type))))
                 (t
                  ;; BUG: Can't find elem-type, try random value.
                  ''(nil))))
          ((typep nil var-type)
           ;; Use NIL as the initializer if the resulting type would
           ;; be t for the given implementation.
           nil)
          ((subtypep var-type 'sequence)
	   nil
	   ;;#+:ignore ; I can't ignore (at least in CMUCL)!! Why?
           (cl:multiple-value-bind (arr len elem-type)
               (decode-seq-type `',var-type)
             (declare (ignore arr elem-type))
             ;; BUG: Only as good as DECODE-SEQ-TYPE.
             (make-sequence var-type (or len 0))))
          ((subtypep var-type 'array)
	   nil
	   ;; THIS IS PLAINLY WRONG FOR NON-SEQUENCE ARRAYS!!!
	   ;; KEEPING CODE JUST FOR EVENTUAL SPECIAL-CASING	   
           ;; Heuristic: assume they mean vector.
           ;; BUG: fails if DECODE-SEQ-TYPE fails to find the right elem type!
	   #+:ignore
           (cl:multiple-value-bind (arr len elem-type)
               (decode-seq-type `',var-type)
             (declare (ignore arr))
             ;; Probably no length, as that case is caught by previous branch
             (make-sequence `(vector ,elem-type ,(or len 0)) (or len 0))))
          ((eq t (upgraded-array-element-type var-type))
           ;; Use NIL as the initializer if the resulting type would
           ;; be t for the given implementation.
           nil)
          (t
	   nil
           ;; BUG: Try to hack it through MAKE-SEQUENCE.  This could fail.
	   #+:ignore	
           (aref (make-sequence `(vector ,var-type) 1) 0)))))

(cl:defun aux-init (aux)
  (destructuring-bind (var-name &optional (var-type t) (var-value nil value-provided-p))
      aux
    (list var-name (if value-provided-p
		       var-value
		     (init-elem var-type)))))

;; This is only called from clean-code
(cl:defun clean-code1 (suspicious prologs code)
  (cl:let ((dead nil))
    (cl:labels ((clean-code2 (prev-parent parent code &aux var)
                 (tagbody
		  R (when (setq var (car (member (setq-p code) suspicious)))
		      (push var dead)
		      (rplaca parent (setq code (caddr code)))
		      (when (or (symbolp code) ; Symbol macros should have already expanded
				(constantp code))
			(cond ((consp (cdr parent))
			       (rplaca parent (cadr parent))
			       (rplacd parent (cddr parent))
			       (setq code (car parent))
			       (go R))	;do would skip the next element
			      (prev-parent (pop (cdr prev-parent)))))))
		 (when (consp code)
		   (clean-code2 nil code (car code)) 
		   (do ((tt code (cdr tt)))
		       ((not (and (consp tt) (consp (cdr tt)))) nil)
		     (clean-code2 tt (cdr tt) (cadr tt))))))
      (declare (dynamic-extent #'clean-code2))
      #+:series-plain
      (clean-code2 nil nil prologs)
      #-:series-plain
      (dolist (p prologs)
        (clean-code2 nil nil p))      
      (clean-code2 nil nil code) ;depends on code not being setq at top.
      dead)))

(cl:defun clean-code (aux prologs code)
  (cl:multiple-value-bind (goes aux) (segregate-aux #'(lambda (v) (eq (cadr v) 'null)) aux)
    ;; Let's get rid of silly constant NIL variables first
    (when goes
      (setq goes (mapauxn #'(lambda (v) (cons (car v) nil)) goes))
      (clean-code1 (mapcar #'car goes) prologs code)
      (when prologs
	(setq prologs (nsublis goes prologs)))
      (when code
	(setq code (nsublis goes code))))

    #+:series-plain
    (cl:let ((suspicious (not-contained (mapauxn #'car aux) prologs code)))
      (when suspicious 
        (setq aux (delete-aux-if #'(lambda (v) (member (car v) suspicious)) aux)))
      (multiple-value-setq (suspicious goes)
	(not-contained-twice (mapauxn #'car aux) prologs code))
      (setq suspicious (set-difference suspicious goes))
      (when suspicious
	(setq suspicious (clean-code1 suspicious prologs code))
	(setq goes (nconc goes suspicious)))
      (values (delete-aux-if #'(lambda (v) (member (car v) goes)) aux) prologs code))
    
    #-:series-plain
    (do ((unfinished t))
	((not unfinished) (values aux prologs code))
      (cl:multiple-value-bind (bound unbound) (segregate-aux #'cddr aux)
        (setq unbound (delete nil unbound))
        (cl:let ((suspicious (delete-aux-if-not #'(lambda (v)
						    (cl:let ((val (caddr v)))
						      (or (symbolp val) ; Symbol macros should have already expanded
							  (constantp val))))
						bound)))
          (setq goes (not-contained (mapauxn #'car suspicious)
				    (mapauxn #'caddr bound) prologs code))
	  (setq unfinished goes)
	  (setq aux (delete-aux-if #'(lambda (v) (member (car v) goes)) aux))
	  (multiple-value-setq (suspicious goes)
	    (not-contained-twice (mapauxn #'car unbound)
				 (mapauxn #'caddr aux) prologs code))
	  (setq suspicious (set-difference suspicious goes))
	  (when suspicious
	    (setq suspicious (clean-code1 suspicious prologs code))
	    (setq goes (nconc goes suspicious)))  
	  (when goes
	    (setq unfinished goes)
	    (setq aux (delete-aux-if #'(lambda (v) (member (car v) goes)) aux))))))))

(cl:defun propagate-types (expr aux &optional (input-info nil))
  (do ((tt expr (cdr tt)))
      ((not (consp tt)) nil)
    (do () ((not (eq-car (car tt) 'series-element-type)))
      (when-bind (info (cdr (assoc (cadar tt) input-info)))
        (rplaca tt info)
        (return nil))
      (setf (car tt)
	    (cond ((cadr (find-aux (cadar tt) aux)))
		  (t t))))
    (when (consp (car tt)) (propagate-types (car tt) aux))))

(cl:defun compute-binds-and-decls (aux)
  (doaux (v aux)
    (propagate-types (cdr v) aux))
  (3mapaux #'(lambda (v)
	      (if (atom v)
		  (values v nil nil)
		(destructuring-bind (var-name &optional (typ t) (var-value nil value-provided-p))
		    v
		  ;; Sometimes the desired type is quoted.  Remove the
                  ;; quote.  (Is this right?)		
		  (when (and (listp typ)
			     (eq 'quote (car typ)))
		    (setq typ (cadr typ)))
		  (if (eq typ t)
		      (values
		       (if value-provided-p
			   `(,var-name ,var-value)
			 var-name)
		       nil
		       nil)
		    (cl:let ((localtyp nil))
		      (or value-provided-p
			  (setq var-value (init-elem typ)))
		      (or var-value
			  ;; That the ones allowing nil show no explicit initializers should
			  ;; be enough to indicate they should be considered unititialized.
			  ;; Let's not penalize implementations not so good at LOCALLY,
			  ;; nor generate gratuitous compilation overhead.
			  (typep nil typ) ;(and (typep nil typ) value-provided-p)
			  (if value-provided-p
			      (setq typ `(null-or ,typ))
			    (setq localtyp `(type ,typ ,var-name) typ nil)))
		      (values (if (or value-provided-p var-value)
				  `(,var-name ,var-value)
				var-name)
			      (when typ `(type ,typ ,var-name))
			      localtyp))))))
          aux))

  (cl:defun codify-2 (aux prologs code)
    (cl:multiple-value-bind (binds decls localdecls) (compute-binds-and-decls aux)
      #+:series-plain
      (cl:multiple-value-bind (body wrapped-p)
        (declarize #'identity (delete nil decls) (delete nil localdecls) prologs code t nil)
	(if binds
	    (cl:funcall (lister wrapped-p) 'cl:let binds body)
	  (if wrapped-p
	      (prognize body)
	    body)))
      #-:series-plain
      (destarrify 'cl:let binds decls localdecls prologs code #'identity t)))

  (cl:defun codify-1 (aux prologs code)
    (cl:multiple-value-call #'codify-2 (clean-code aux prologs code)))

) ; end of eval-when

;;;;                         (4) DO MERGING

;;; The merging of frags into a single frag follows the pattern of
;;; splitting determined above.  At the leaves of the tree of splits
;;; are subexpressions where some number of frags are connected solely
;;; by on-line series data flow.  All these frags are combined into a
;;; single frag in one step.  As long as every termination point is
;;; connected to every output point, this is a trivial operation.  If
;;; not, flags and such have to be inserted to ensure that the result
;;; will have the property that all of the outputs are produced as
;;; soon as ANY input runs out of elements.
;;;
;;; After this is done, things proceed by doing two different kinds of
;;; mergings based on the two different types of splitting.  One case
;;; is particularly simple.  Merging frags connect by non-series data
;;; flow or no data flow at all, is trivial.
;;;
;;; Merging frags connected by series dflow touching at least one
;;; off-line port is where the key difficulties lie.  There are three
;;; areas of trouble. First, things have to be carefully arranged so
;;; that termination will work out right in situations where not every
;;; termination point is connected to every output.  Second, if an
;;; off-line output is connected to an off-line input, one of the
;;; frags has to be turned inside out.
;;;
;;; Third, operations concerning these issues and even the simple
;;; cases of off-line merging can convert extraneous on-line ports
;;; (one not directly participating in the merging) into off-line
;;; ports.
;;;
;;; (One issue here is that we must be sure that this port is
;;; isolated. We know it is, because it cannot be an extraneous port
;;; on the frag unless it is either a port on the expression as a
;;; whole (and therefore touched by no dflow) or touched by isolated
;;; dflow.)
;;;
;;; When conversion to off-line happens as part of the internal course
;;; of events, it indicates that the code is going to get messy, but
;;; need not concern the user.  (In fact, the code may even be quite
;;; efficient, it will just look like a real mess.)  Note that if you
;;; are just writing a simple series expression that neither reads nor
;;; writes a series as a whole, there will be no externally visible
;;; series ports, and you need not worry about this issue.
;;;
;;; However, when you are defining a new series functions, there are
;;; external series ports.  Given that on-line ports are much more
;;; usable than off-line ones, it is unfortunate that doing odd things
;;; with the termination (for example) can make all your ports be
;;; off-line.
;;;
;;; Two cases are always simple.  If an extraneous input or output
;;; carries a non-series value, then there is never a problem.  If it
;;; is an input than it must be available from the very start of
;;; computation and therefore will always be readable no matter how
;;; the frags are combined.  If the port is an output, then it does
;;; not need to be available until after everything is done, and the
;;; strongly connected check insures that it will be eventually
;;; computed.
;;;
;;; Things are also basically simple if an extraneous input or output
;;; is off-line.  In this situation, a specific marker says exactly
;;; where connected computation should be put, and this marker will
;;; always end up in an appropriate place no matter how the fragments
;;; are combined. The only thing which requires care is making sure
;;; that these markers stay at top level.
;;;
;;; One problem case, however, is that it is possible for an off-line
;;; output to be used by an off-line input.  This can cause a splitting
;;; to happen that ends up in a situation where an off-line output is used
;;; both internally and externally.  If so, the output has to be
;;; preserved the first time it is used so that it can be used again.
;;;
;;; On the other hand, if an extraneous input or output is on-line,
;;; significant complexities can arise.  If an extraneous port is
;;; on-line then it may have to be changed into an off-line
;;; port. Fortunately, things are arranged so that a graph is never
;;; split by breaking an on-line to on-line data flow.  However, an
;;; on-line port can be on one end of a broken data flow.
;;; Nevertheless, most instances of extraneous on-line ports come from
;;; weird lambda-series bodies. Except in simple situations extraneous
;;; on-line ports are not supported unless they come from complete
;;; expressions.
;;;
;;; Consider the simplest mergings first.
;;;
;;; Two frags are connected by non-series dflow (or no dflow). (When
;;; processing complete series expressions it will always be the case
;;; that both frags are non-series frags.  Further, the way splitting
;;; happens guarantees that any series ports are direct ports of the
;;; expression as a whole.)
;;;
;;; Merging is trivial as long as at least one of the frags is
;;; non-series.  If one has series ports, it can be left totally
;;; alone.  The other can be placed entirely in the prolog (if it is
;;; first) or epilog.
;;;
;;; If both frags are series frags, things are complex.  You must
;;; evaluate the first one first and completely to get the non-series
;;; output(s) (if any) that are used by the second.  This will force
;;; the first frag to make all its outputs normally.  Then you have to
;;; evaluate the other one.  To do this, one frag or the other has to
;;; be severely distorted.  This process will make all of the series
;;; ports on the modified frag be off-line.
;;;
;;; The program below converts the first frag into a tight loop that
;;; runs in the beginning of the body.  (This is essential to preserve
;;; the invariant that off-line-spots are only in bodies, but it makes
;;; a real mess and forces all the series inputs off-line.  Also note
;;; the way the prolog of the other frag has to be moved.)  (Note that
;;; the off-line ports created are isolated, because they are on the
;;; outside of the expression as a whole.)


;; This is used for the variable renaming part of all kinds of dflow.
;; Rets must be saved either if they have no dflow from them (they are
;; outputs of the whole top level expression) or if there is a dflow
;; to a frag that is not currently being dealt with.  The functional
;; argument specifies which dflow are which.

(cl:defun handle-dflow (source handle-this-dflow &optional (kill-dflow t))
  (cl:let ((killable nil)
	   (deadargs nil)
	   (deadflows nil))	  
    (dolist (ret (rets source))
      (cl:let ((ret-killable (not (null (nxts ret)))))
        (dolist (arg (nxts ret))
	  (cond ((not (cl:funcall handle-this-dflow ret arg))
		 (setq ret-killable nil))
		(t (nsubst (var ret) (var arg) (fr arg))
		   (if kill-dflow
		       (-dflow ret arg)
		     (push (cons ret arg) deadflows))
		   (-arg arg)
		   (push arg deadargs)
		   )))
	(when ret-killable 
          (if kill-dflow
	      (-ret ret))
	  (push ret killable))))
    (values killable deadargs deadflows)))


;; This is only called from non-series-merge
(cl:defun implicit-epilog (frag)
  (setf (epilog frag) (flatten-prolog (merge-prologs (aux->prolog (aux frag))
						     (prolog frag))))
  (setf (prolog frag) nil)
  (setf (aux frag) (simplify-aux (aux frag)))
  frag)

;; This is only called from non-series-merge
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
    (nsubst b end frag)
    (setf (body frag)
	  `((if ,flag (go ,c))
	    ,@(flatten-prolog (merge-prologs (aux->prolog (aux frag)) (prolog frag)))
	    ,lab ,@(body frag) (go ,lab)
	    ,b ,@(epilog frag) (setq ,flag t)
	    ,@(flatten-prolog (merge-prologs (aux->prolog (aux arg-frag))
					     (prolog arg-frag)))
	    ,c))
    (setf (aux arg-frag) (simplify-aux (aux arg-frag)))
    (setf (aux frag)     (simplify-aux (aux frag)))
    (add-aux frag flag 'boolean nil)

    (setf (prolog frag) nil)
    (setf (prolog arg-frag) nil)
    (setf (epilog frag) nil)))

;; This is only called from non-series-merge-list
(cl:defun non-series-merge (ret-frag arg-frag)
  (cl:multiple-value-bind (killable deadargs deadflows)
	    (handle-dflow ret-frag
			  #'(lambda (r a) (declare (ignore r)) (eq (fr a) arg-frag))
			  nil) ; We'll kill rets after merging
    (when (not (non-series-p ret-frag))
      (if (non-series-p arg-frag)
	  (implicit-epilog arg-frag)
	(eval-on-first-cycle ret-frag arg-frag)))
    (dolist (a deadargs)
      (pusharg a (fr a)))
    (cl:let ((result (merge-frags ret-frag arg-frag)))
      (dolist (r killable)
	(-ret r))
      (dolist (f deadflows)
	(-dflow (car f) (cdr f)))
      (dolist (a deadargs)
	(-arg a))
      result)))

(cl:defun non-series-merge-list (&rest frags)
  (declare (dynamic-extent frags))	  
  (cl:let ((frag (pop frags)))
    (loop (when (null frags)
	    (return frag))
          (setq frag (non-series-merge frag (pop frags))))))

;;; A graph of many frags is connected solely by on-line data
;;; flow. (Here, even when operating on complete series expressions,
;;; it is expected that there are extraneous series inputs and
;;; outputs, and that internally used series ports can be used outside
;;; as well.  However, every external use must be isolated.)
;;;
;;; Here things are in general simple, and everything can just be
;;; merged together in an order compatible with the dflow and
;;; everything will be fine and all of the extraneous ports will be
;;; left alone.
;;;
;;; However, if there are any termination points that are not
;;; connected to every output point, we have a problem.  Things have
;;; to be altered so that these termination points don't prematurely
;;; stop things they should not stop.  This is done by inserting flags
;;; that delay termination until the correct time.  This is done as
;;; follows.
;;;
;;; (1) find each termination point and output point.  Test each
;;; termination point to see whether it is total (i.e., is connected
;;; to every output and therefore calls for stopping everything.)
;;; Total termination points can act by simply branching to END when
;;; they trigger.
;;;
;;; If a termination point is not total, then a flag has to be
;;; gensymed corresponding to it and the point has to be changed so
;;; that it sets the flag (which starts with a value of NIL) to t
;;; instead of branching when exit occures.  For non-total termination
;;; points that are series inputs, this means that the input will have
;;; to become an off-line port that catches termination.
;;;
;;; (2) for each non-total termination point we have to figure out
;;; what frags it controls.  First, frags the termination point has
;;; data flow to are forced to stop when it stops.  Second, once EVERY
;;; output a frag has data flow to has been completed, the frag can
;;; stop too.
;;;
;;; (In addition, we must note that once every frag has stopped, the
;;; loop as a whole should stop.  If there is at least one total
;;; termination point and there is at least one output point that is
;;; controlled only by total termination points, then we don't have to
;;; do anything special.  When a total termination point stops
;;; everything stops and we always have to continue computing as long
;;; as none of the total termination points have stopped.  However, if
;;; the above is not the case, we have to add a new termination test
;;; that checks to see if all of the outputs have completed, and stop
;;; everything.)
;;;
;;; (2a) follow the dflow from each non-total termination point and
;;; note that the termination point itself, and every frag you reach
;;; must stop as soon as the termination point does.  This is done by
;;; adding FLAG into the list of control flags for each frag.  (This
;;; list is an implicit OR that specifies when to STOP executing the
;;; frag)
;;;
;;; (2b) Start at each output point and get the set of flags that
;;; control it. Follow the dflow backward from each output point and
;;; note what frags feed into it.  Once this is done, create a new
;;; entry (AND (OR . output-flag-set1) (OR . output-flag-set2) ...) in
;;; the list of control flags.
;;;
;;; The above can be done in two highly efficient marking sweeps.  The
;;; first of which also determines whether there are any non-total
;;; termination points we have to worry about.
;;;
;;; Finally, we simplify each of the control expressions and do the
;;; merging inserting the correct tests of flags.  (I could think
;;; about sorting the frags as much as possible consistent with dflow
;;; so that adjacent frags have the same expressions, however, this
;;; might be bad with respect to side-effects.)

;;; flag meanings
;;; 1- marks region of interest.
;;; 2- marks places to start output point sweep.
;;; 4- marks places to start termination point sweep.
;;; 4- mark individual output points and termination points.

;; This is only called from check-termination
(cl:defun make-set-flag-rather-than-terminate (frag)
  (cl:let* ((B (new-var 'bb))
	    (C (new-var 'cc))
	    (flag (new-var 'terminated)))
    (make-ports-off-line frag nil)
    (dolist (a (args frag))
      (when (and (series-var-p a) (not (off-line-exit a)))
        (setf (off-line-exit a) B)))
    (nsubst B end (body frag))
    (add-literal-aux frag flag 'boolean nil)
    (setf (body frag) (nconc (body frag) `((go ,C) ,B (setq ,flag t) ,C)))
    flag))

;; the challenge here is making as simple a test as possible
(cl:defun make-test (and-of-ors)
  (if (null and-of-ors)
      t
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
                        (if (cdr or)
			    `(or . ,or)
			  (car or)))
                    residual-and-of-ors))
      (when residual-and-of-ors
        (push `(and . ,(nreverse residual-and-of-ors)) top-level-or))
      (cond ((null top-level-or) nil)
            ((null (cdr top-level-or)) (car top-level-or))
            (t `(or . ,(nreverse top-level-or)))))))

;; this function assumes that on-line-merge will merge frags in the
;; order they are on *graph*.

;; This is only called from on-line-merge
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

;;; first sweep to test connection of terms to outputs.
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
      (when (null problem-terminations)
	(return nil))

;;; make the flags and get them initialized
     (dolist (tentry problem-terminations)
       (cl:let ((flag (make-set-flag-rather-than-terminate (cadr tentry))))
         (dolist (oentry outputs)
           (when (marked-p (car tentry) (cadr oentry))
             (push flag (cddr oentry))))))

;;; second sweep to test connection of everything to outputs.
      (cl:let ((*graph* (reverse *graph*)))
        (dofrags (f 3)                                       ; 3 = 1+2
          (cl:let ((current-marks (logandc1 4 (marks f)))) ;strips out 4 bit
            (dolist (a (all-prvs f))
              (when (marked-p 1 (fr a))
                (mark current-marks (fr a)))))))
      (setq all-terminated (make-test (mapcar #'cddr outputs)))
      (when all-terminated
        (push `(if ,all-terminated (go ,end)) (body (car *graph*))))

;;; add conditionalization to each frag
      (setq conditions
            (mapcar #'(lambda (f)
                        (make-test
                          (mapcar #'cddr
                                  (remove-if-not #'(lambda (e)
                                                     (marked-p (car e) f))
                                                 outputs))))
                    *graph*))
      ;; could do some sorting here based on similarity between
      ;; conditions.
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

(cl:defun on-line-merge (*graph*) ;merge everything, all dflow is on-line.
  (if (null (cdr *graph*))
      (car *graph*)
    (cl:let ((frag nil))
      (reset-marks 1)
      (check-termination *graph*)
      (dofrags (f)
	(handle-dflow f
          #'(lambda (r a) (declare (ignore r)) (marked-p 1 (fr a))))
	(if (null frag)
	    (setq frag f)
	  (setq frag (merge-frags frag f))))
      (reset-marks 0)
      (maybe-de-series frag))))

;;; Two frags are connected by dflow touching at least one off-line
;;; port. (Even in complete expressions, there can be extraneous
;;; series ports.  (e.g., going to other subexpressions created in
;;; other splits.) However, any dflow touching these ports must be
;;; isolated.) Note that if the output port is on-line there may be
;;; other dflow starting on it other than the one in question.
;;;
;;; The first difficulty in this case involves termination.  With
;;; regard to the second frag, there is no problem.  If the second
;;; frag is the first to stop, then it must have produced all its
;;; outputs.  If the first frag is the first to stop, then it must
;;; have produced all its outputs which either means that the second
;;; must also stop, or the second will catch the termination of the
;;; first.
;;;
;;; Further there is no trouble with the first frag as long as either
;;; (1) the second frag has no termination points other than the one
;;; in question (i.e., has no series inputs without off-line-exits
;;; other than possibly the one in question and cannot by itself
;;; terminate) or (2) the first frag does not have any output points
;;; other than the one in question (i.e., has no other outputs, and
;;; does not have the must-run flag set) and this output is not used
;;; anywhere other than by the input in question.  In case (1) running
;;; the second frag forces the complete running of the first frag.  In
;;; case (2), it does not matter if the first frag is run completely
;;; or not.
;;;
;;; If neither of the cases above applies, we have to do some hard
;;; work.  We know that the destination frag is a termination point
;;; and either, (a) the source frag has a non-series output or has the
;;; must-run flag set or (b) there is data flow from series outputs of
;;; the source frag to more than one place (i.e., either fan out from
;;; one, or dflow from two different ones).
;;;
;;; In case (a) things are simple, we just have to change the
;;; destination frag so that it always reads all of the elements of
;;; the input in question. This can be done by catching the
;;; termination of the frag caused by other things, and using a flag
;;; to force execution to continue until the input runs out.  This
;;; transformation causes all the other series ports to become
;;; off-line.
;;;
;;; Case (b) is more complex, the source frag might not be able to
;;; terminate at all, and even if it can, it might not terminate soon
;;; enough.  We must look at all of the destinations of dflow from it
;;; (not just the one we are looking at now) and see which ones of
;;; them are termination points.  What we want is for the source to
;;; terminate exactly when all of the destinations terminate (if
;;; ever).  If at least one of the destinations is not a termination
;;; point, then we can proceed exactly as in case (a).  If none of
;;; them are, then we can still proceed the same, but we have to add a
;;; test to the first frag that causes termination as soon as all of
;;; the destinations have stopped.  This requires flags to be set in
;;; the destinations.
;;;
;;; (Note that we could probably use simpler frags and things if we
;;; figured out all the places where we were going to have to do this
;;; before merging the on-line subexpressions in the first place.
;;; However, this would make the code more complex and is not worth
;;; doing given that it is rather unlikely for series expressions to
;;; have more than one output in any case.  Note that the prior
;;; version of this macro package just outlawed every problematical
;;; case.  Doing things with more efficiency is a possible future
;;; research direction.)
;;;
;;; The second difficulty involves actually doing the merging.
;;;
;;;   A- The ret is off-line and the arg is on-line There are two
;;;   basic ways in which this can be handled.
;;;
;;;  A1- The most straightforward way is to insert the arg frag into
;;;  the off-line-spot in the ret-frag.  This is very simple and
;;;  allows on-line inputs and outputs of the ret-frag to remain
;;;  unchanged.  However, on-line inputs and outputs of the arg-frag
;;;  are forced to become off-line.
;;;
;;;  A2- The ret-frag is turned inside out and converted into an
;;;  enumerator, which has on-line data flow to the arg-frag.  This
;;;  requires the use of a flag variable, and the making off-line of
;;;  any on-line inputs or outputs of the ret-frag.  However, it
;;;  allows any extraneous inputs and outputs of the arg-frag to
;;;  remain unchanged.
;;;
;;;  If either of the two frags has no extraneous on-line ports, then
;;;  the appropriate combination method above is used and everything
;;;  works out great. If they both have extraneous on-line ports, then
;;;  which every one has fewer of these ports has them changed to
;;;  off-line ports and the appropriate process above is then applied.
;;;
;;; In either case, special care has to be taken to insure that the
;;; off-line output will still exist if it is used some place other
;;; than in the arg-frag. (It is possible that it will exist, but will
;;; get changed to on-line.  This does not cause confusion since the
;;; input it is connected to must be off-line--otherwise there would
;;; be only one dflow from the output.)
;;;
;;;   B- The ret is on-line and the arg is off-line.  This case is
;;;   closely analogous to the one above.  Again, there are two basic
;;;   ways to proceed.
;;;
;;;  B1- The most straightforward way is to insert the ret frag into
;;;  the off-line-spot in the arg-frag.  This has the feature that it
;;;  is very simple and allows all on-line inputs and outputs of the
;;;  arg-frag to remain unchanged. However, on-line inputs and outputs
;;;  of the ret-frag are forced to become off-line.
;;;
;;;  B2- The arg-frag is turned inside out and converted into a
;;;  reducer which receives on-line data flow from the ret-frag.  This
;;;  requires the use of a flag variable, and it forces off-line any
;;;  extraneous on-line inputs or outputs of the arg-frag.  However,
;;;  it allows any extraneous inputs and outputs of the ret-frag to
;;;  remain unchanged.
;;;
;;; If either of the two frags has no extraneous ports, then the
;;; appropriate combination method above is used and everything works
;;; out great.  If the both have extraneous ports then whichever has
;;; fewer has them changed to off-line and things proceed as above.
;;;
;;;   C- the ret and arg are both off-line.  Here it is not possible
;;;   to simultaneously substitute the frags into each other.
;;;   However, it is possible to combine them after A2 is applied to
;;;   the ret-frag or B2 is applied to the arg-frag.  Again this
;;;   presents two options and it is possible to preserve either the
;;;   extraneous ports of the ret-frag or the arg-frag, but not both.
;;;
;;; Note we have to be prepared for the general case more often than
;;; you might expect, because the combination process can cause ports
;;; to become off-line.

(cl:defun some-other-termination (arg)
  (or (active-terminator-p (fr arg))
      (plusp (count-if #'(lambda (a)
                           (and (not (eq a arg))
                                (series-var-p a)
                                (not (off-line-exit a))))
                       (args (fr arg))))))

(cl:defun count-on-line (frag)
  (+ (length (find-on-line (args frag))) (length (find-on-line (rets frag)))))

(cl:defun make-read-arg-completely (arg &optional (cnt nil))
  (cl:let* ((frag (fr arg))
	    (terminates-p (branches-to end (body frag)))
	    (B (when terminates-p (new-var 'bbb)))
	    (C (new-var 'ccc))
	    (flag (new-var 'ready-to-terminate)))
    (make-ports-off-line frag nil)
    (when terminates-p
      (dolist (a (args frag))
	(when (and (not (eq a arg)) (not (off-line-exit a)))
	  (setf (off-line-exit a) B)))
      (nsubst B end (body frag)))
    (add-literal-aux frag flag 'boolean nil)
    (setf (body frag)
          (nsubst-inline (if (not (off-line-exit arg))
			     `(,@(when terminates-p
				   `((go ,C) ,B ,@(if cnt `((if (null ,flag) (decf ,cnt))))
				     (setq ,flag t)))
			       ,C ,(off-line-spot arg) (if ,flag (go ,C)))
                           (cl:let ((CF (new-var 'CF))
				    (CD (new-var 'CD))
				    (exit (off-line-exit arg)))
                             (setf (off-line-exit arg) CF)
                             `(,@(when terminates-p `((go ,C) ,B (if ,flag (go ,end)) (setq ,flag t)))
                               ,C  ,(off-line-spot arg) (go ,CD)
                               ,CF (if ,flag (go ,end)) (setq ,flag t) (go ,exit)
                               ,CD (if ,flag (go ,C)))))
                         (off-line-spot arg) (body frag)))))

(cl:defun convert-to-enumerator (ret off-line-exit)
  (cl:let ((frag (fr ret)))
    (make-ports-off-line frag off-line-exit)
    (cl:let* ((tail (member (off-line-spot ret) (body frag)))
	      (head (ldiff (body frag) tail))
	      (flag (new-var 'flg))
	      (e (new-var 'e)))
      (setf (off-line-spot ret) nil)
      (add-literal-aux frag flag 'boolean nil)
      (setf (body frag)
            `((when (null ,flag) (setq ,flag t) (go ,e))
              ,@(cdr tail)
              ,e ,@ head)))
    frag))

(cl:defun convert-to-reducer (arg)
  (cl:let ((frag (fr arg)))
    (make-outputs-off-line frag)
    (cl:let* ((tail (member (off-line-spot arg) (body frag)))
                (head (ldiff (body frag) tail))
                (flag (new-var 'fl))
                (M (new-var 'm))
                (N (new-var 'n)))
      (add-literal-aux frag flag 'boolean nil)	     
      (setf (body frag)
            `((if (null ,flag) (go ,M))
          ,N ,@(cdr tail)
          ,M ,@ head
              (when (null ,flag) (setq ,flag t) (go ,N)))))
    frag))  

(cl:defun substitute-in-output (ret arg)
  (cl:let ((ret-frag (fr ret))
           (arg-frag (fr arg)))
    (make-ports-off-line arg-frag (off-line-exit arg))
    (setf (body ret-frag)
          (nsubst-inline (body arg-frag) (off-line-spot ret) (body ret-frag)
                         (nxts ret)))
    (setf (body arg-frag) nil)))

(cl:defun substitute-in-input (ret arg)
  (cl:let ((ret-frag (fr ret))
           (arg-frag (fr arg))
           (ex (off-line-exit arg)))
    (make-ports-off-line ret-frag ex)
    (when ex
      (dolist (a (args (fr ret)))
        (when (and (series-var-p a) (not (off-line-exit a)))
	  (setf (off-line-exit a) ex)))
      (nsubst ex end (body ret-frag)))
    (setf (body arg-frag)
          (nsubst-inline (body ret-frag) (off-line-spot arg) (body arg-frag)))
    (setf (body ret-frag) nil)))

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
	  (add-literal-aux ret-frag cnt 'fixnum (length destinations))
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
          (t (cond ((and (> ret-rating arg-rating) (null (off-line-exit arg)))
                    (convert-to-reducer arg)
                    (substitute-in-output ret arg))
                   (t (convert-to-enumerator ret (off-line-exit arg))
                      (substitute-in-input ret arg))))))
  (maybe-de-series (merge-frags ret-frag arg-frag)))


;;;;                           (3) DO SPLITTING

;;; Splitting cuts up the graph at all of the correct places, and
;;; creates a lisp expression which, when evaluated will merge
;;; everything together. Things area done this way so that all of the
;;; splitting will happen before any of the merging.  This makes error
;;; messages better and allows all the right code motion to happen
;;; easily.

;; This splits the graph by dividing it into two parts (part1 and
;; part2) so that to-follow is in part1, there is no data flow from
;; part2 to part1 and all of the data flow from part1 to part2
;; satisfies the predicate CROSSABLE.
;;
;; The splitting is done by marker propagation (using the marker
;; 2). The algorithm used has the effect of minimizing part1, which
;; among other things, guarantees that it is fully connected.
(cl:defun split-after (frag crossable)
  (mark 2 frag)
  (cl:let ((to-follow (list frag)))
    (loop (when (null to-follow)
	    (return nil))
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
      (if (marked-p 2 f)
	  (push f part1)
	(push f part2)))
    (reset-marks 0)
    (values (nreverse part1) (nreverse part2))))

;;; This finds internal non-series dflows and splits the graph at that
;;; point. It may be necessary to cut more than one dflow when
;;; splitting.  Therefore, no matter how we do things, it will always
;;; be possible that either of the parts will have more non-series
;;; dflow in it.  To see this, note the following example:
#|(let ((e (scan x)))
    (values (foo (reverse (collect e)))
            (collect-last e (car (bar y))))) |#
;;; The order of frags on the graph is going to be scan, collect,
;;; reverse, foo, bar, car, collect-last.  If you start on either the
;;; first frag, or the first non-series dflow, or the last frag, or
;;; the last dflow, there are going to be another non-series dflow in
;;; each half.  (Note starting from the front, the non-series dflow
;;; from car to collect-last is going to be pulled into the first
;;; part.  And in general, starting from the front puts lots of
;;; non-series dflow in the second part.)
;;;
;;; The best we can do is construct one part so that it is known that
;;; that part is connected.  The method used here ensures that the
;;; first part is connected by minimizing it.
;;;
;;; Note there is an implicit assumption here that making a cut
;;; through a bundle of isolated non-series dflows cannot converted a
;;; non-isolated one into an isolated one.  If this could happen, we
;;; would fail to detect some problems, and the overall theory would
;;; be overly strict.

;; This is only called from non-series-dflow-split
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
      `(dflow ,@(if (eq-car part1 'dflow)
		    (cdr part1)
		  (list part1))
              ,@(if (eq-car part2 'dflow)
		    (cdr part2)
		  (list part2))))))

;; HELPER
(defmacro doing-splitting (&body body)
  `(cond ((null (cdr *graph*)) (list 'quote (car *graph*)))
         (t (reset-marks 1) (prog1 (progn ,@ body) (reset-marks 0)))))

;; HELPER
(defmacro doing-splitting1 (&body body)
  `(cond ((null (cdr *graph*)) *graph*)
         (t (reset-marks 1) (prog1 (progn ,@ body) (reset-marks 0)))))

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


;; We have to do non-dflow splitting and non-series-dflow-splitting
;; separately in order to get error messages about non-isolated
;; non-series dflow right. This breaks the expression up at points
;; where there is no data flow between the subexpressions.  Since the
;; size of part1 is minimized it is known that part1 must be fully
;; connected.

(cl:defun disconnected-split (*graph*)
  (doing-splitting1
    (cl:multiple-value-bind (part1 part2)
        (split-after (car *graph*) #'(lambda (r a) (declare (ignore r a)) nil))
      (cond ((null part2) (non-series-dflow-split part1))
            (t (setq part1 (non-series-dflow-split part1))
               (setq part2 (disconnected-split part2))
               `(no-dflow ,part1
                          ,@(if (eq-car part2 'no-dflow)
                                (cdr part2)
			      (list part2))))))))

;; The following breaks the expression up at all the points where
;; there is no series data flow between the subexpressions.
;; Non-series port isolation guarantees that this split is possible,
;; cutting only non-series dflows. (If there is no data flow, you
;; might not have to cut any data flow.)  If *graph* is a complete
;; expression (i.e., one that does not have any series inputs or
;; outputs overall), then the subexpressions cannot have external
;; series inputs or outputs.
;;
;; Non-series splitting typically breaks the expression up into a
;; large number of fragments.  Great care is taken to make sure that
;; these fragments will be reassembled without changing their order.
;; This is important so that the user's side-effects will look
;; reasonable.  Careful attention has to be paid to the dflow
;; constraints when figuring out where to put the series
;; subexpressions.  They are put where the last fn in them suggests,
;; within the limits of dflow.
;;
;; Note that the only way the user can write something that has some
;; side-effects is to write a side-effect expression that turns into a
;; non-series-computation (via isolate-non-series) or to write
;; something in a functional argument to a higher-order series
;; function.  the functions here make things come out pretty well in
;; the first case; there is not much anybody could do about the second
;; case.

(definline order-num (frags)
  (position (car (last frags)) *graph*))

(cl:defun reorder-frags (form)
  (cond ((eq-car form 'dflow) (mapcan #'reorder-frags (cdr form)))
        ((eq-car form 'no-dflow)
         (cl:let ((sublists (mapcar #'reorder-frags (cdr form)))
                    (result nil) min-num min-sublist)
           (setq sublists
                 (mapcar #'(lambda (l) (cons (order-num (car l)) l)) sublists))
           (loop (when (null (cdr sublists))
                   (return (nreconc result (cdr (car sublists)))))
                 (setq min-num (car (car sublists)) min-sublist (car sublists))
                 (dolist (sub (cdr sublists))
                   (when (< (car sub) min-num)
                     (setq min-num (car sub) min-sublist sub)))
                 (push (pop (cdr min-sublist)) result)
                 (if (null (cdr min-sublist))
		     (setq sublists (delete min-sublist sublists))
                   (setf (car min-sublist) (order-num (cadr min-sublist)))))))
        (t (list form))))


;; At this next stage, we split based on off-line ports.  (Note that all
;; non-series splitting must be totally complete at this time.)  Several
;; other things are important to keep in mind.  First, whenever we split on
;; an off-line output that has more than one dflow from it to on-line ports,
;; we insert a dummy identity frag so that there will be only one dflow from
;; the off-line port to on-line ports (the multiple dflows come from the
;; output of the dummy frag).  There are three benefits to this.  First,
;; doing this allows us to make the split cutting only one dflow arc.  This
;; guarantees that both parts remain connected and therefore we don't have to
;; call disconnected-split again.
;;
;; Second, when checking for isolation when doing splitting at the
;; same time, we need to have the property that doing a split cannot
;; cause a non-isolated arc to become isolated.  If we cut more than
;; one series dflow when splitting we could make something else be
;; isolated. Consider the program below.
#|(let ((e (split #'plusp (scan x))))
    (collect (#M+ e (f (g e))))) |#
;; Note that the offline output of split is isolated, but neither the
;; input of f or the output of g is isolated.  If you cut both dflows
;; from the split when doing a split, these two ports look isolated in
;; the part they are in.
;;
;; Third, the dummy frag helps keep things straight during later
;; merging.  The key problem is that if there is more than one on-line
;; destination port, then we must make sure that they stay on-line,
;; because they may not be isolated.  The dummy frag essentially
;; records the requirement that the destinations must keep in
;; synchrony.
;;
;; Note that when splitting, things will come out exactly the same no
;; matter which part is minimized, because the whole expression is
;; connected and there is no non-series dflow.  As a result, there
;; cannot be more than one way to split the expression---Every
;; function must be forced to one half or the other.
;;
;; By the same argument used with regard to non-series dflow, either
;; part can still have off-line ports in it that have not been split
;; on.
;;
;; Note that even if the whole does not have any external series
;; ports, the two pieces can.  At least one will be off-line, the
;; other can be on-line.  Note that if the splitting is being done
;; based on an off-line input, then the output in part one can be used
;; in more than one place.  In particular, it can be used by another
;; off-line input which is now still in part1.  This forces complex
;; merging cases to be handled.

;; This is only called from off-line-split
(cl:defun insert-off-line-dummy-frag (ret args)
  (cl:let* ((var (new-var 'oo))
	    (dummy-ret (make-sym :var var :series-var-p t))
	    (dummy-arg (make-sym :var var :series-var-p t))
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

;; This is only called from off-line-split
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

;; This is only called from do-splitting
(cl:defun non-series-split (*graph*)
  (cl:let ((subexprs (disconnected-split *graph*)))
    (setq subexprs (reorder-frags subexprs))
    (cons 'non-series-merge-list
          (mapcar #'off-line-split subexprs))))

;; Main splitting entry point

;; This is only called from mergify
(cl:defun do-splitting (*graph*)
  (reset-marks 0)
  (non-series-split *graph*))

;;;;                        TURNING A FRAG INTO CODE

;;; this takes a non-series frag and makes it into a garden variety
;;; chunk of code. It assumes that it will never be called on a frag
;;; with any inputs.

(eval-when (:compile-toplevel :load-toplevel :execute)

(cl:defun aux-ordering (a b)
  (when (consp a) (setq a (car a)))
  (when (consp b) (setq b (car b)))
  (string-lessp (string a) (string b)))

(cl:defun use-user-names (aux loop)
  (cl:let ((alist nil))
    (doaux (v-info aux)
      (cl:let* ((v (car v-info))
                (u (cdr (assoc v *user-names*))))
        (when (and u (not (contains-p u loop)) (not (rassoc u alist)))
          (push (cons v u) alist))))
    (when alist
      (nsublis alist loop))))

(cl:defun liftable-var-p (var)
  ;; Is VAR the name of a variable we can lift?  This says yes to
  ;; everything except #:SEQ-nnn variables.
  (if (symbol-package var)
      var
      (cl:let ((var-name (symbol-name var)))
	(when (>= (length var-name) 4)
	  (not (string-equal "SEQ-" (subseq var-name 0 4)))))))

(cl:defun find-out-vars (code)
  ;; If code looks something like (let (bindings) ...) we peek at the
  ;; bindings.  Return a list of the variables being defined in the
  ;; bindings.  (Ignore any intializations being done in the binding.)
  (when (member (first code) '(let cl:let))
    (destructuring-bind (let (&rest bindings) &rest body)
	code
      (declare (ignore let body))
      (remove-if-not #'liftable-var-p
		     (mapcar #'(lambda (x)
				 (if (listp x)
				     (first x)
				     x))
			     bindings)))))

(cl:defun find-initializers (vars-to-init code)
  ;; We try to find initializers to the variables in VARS-TO-INIT.  We
  ;; assume CODE looks like
  ;;
  ;; (let (#:out-1 #:out-2 ...)
  ;;   (setq #:out-1 <init-1>)
  ;;   (setq #:out-2 <init-2>)
  ;;   <other stuff>
  ;; )
  ;;
  ;; We return an alist of the variable and the initializer from the
  ;; setq expression.
  (do ((inits nil)
       (list (cddr code) (rest list)))
      ((not (and (listp (first list))
		 (member (first (first list)) '(setq cl:setq declare cl:declare))))
       ;; Exit the loop if we've gone past the last setq, ignoring any
       ;; declare expressions
       inits)
    ;;(format t "setq = ~A~%" (first list))
    (cl:let ((name (find (second (first list)) vars-to-init)))
      ;;(format t "name = ~A~%" name)
      (when name
	;;(format t "init = ~%" (third (first list)))
	(push (list name (third (first list)))
	      inits)))))

(cl:defun remove-initializers (inits code)
  (do ((new-code '())
       (list (cddr code) (rest list)))
      ((not (and (listp (first list))
		 (member (first (first list)) '(setq cl:setq declare cl:declare))))
       (append new-code list))
    ;;(format t "looking at ~A~%" (first list))
    ;;(format t "new-code = ~A~%" new-code)
    (unless (member (second (first list)) inits :key #'first)
      ;;(format t "saving ~A~%" (first list))
      (setf new-code (append new-code (list (first list)))))))

;; Try to lift the initialization of out vars to the let.  This is a
;; heuristic that I (toy@rtp.ericsson.se) hope works.
(cl:defun lift-out-vars (code)
  ;; We look for something like
  ;; (let (out-1 out-2)
  ;;   (setq out-1 <init-1>)
  ;;   (setq out-2 <init-2>)
  ;;   <stuff>)
  ;; and try to convert that to
  ;;
  ;; (let ((out-1 <init-1>) (out-2 <init-2>))
  ;;   <stuff>)
  ;;
  ;; This assumes that nothing but setq's of out variables occurs
  ;; between the binding and <stuff>.
  (cl:let ((bindings (second code))
	   (out-vars (find-out-vars code)))
    (when out-vars
      ;; There are output vars.  Find the initializers associated with
      ;; those output vars.
      (cl:let* ((inits (find-initializers out-vars code))
		(new-bindings
		 (mapcar #'(lambda (v)
			     ;; Create a new bindings
			     ;; list that initializes the
			     ;; variables appropriately.
			     (cl:let ((var (if (listp v) (first v) v)))
			       (or (assoc var inits :key #'(lambda (x)
							     (if (listp x)
								 (first x)
								 x)))
				   v)))
			 bindings)))
	`(cl:let* ,new-bindings ,@(remove-initializers inits code))))))

;; Set this to non-NIL to activate LIFT-OUT-VARS when generating the
;; series expansion.
(defvar *lift-out-vars-p* t)

(cl:defun codify (frag)
  (dolist (r (rets frag))
    (when (series-var-p r)
      (rrs 10 "~%Series value returned by~%" (code frag))))
  (maybe-de-series frag nil)
  (cl:let ((rets (mapcan #'(lambda (r)
			     (when (not (free-out r))
			       (list (var r))))
                           (rets frag)))
           (aux  (aux frag))
           (prologs (prolog frag))
	   (code (body frag))
	   (wrps (wrappers frag)))
    #+:series-plain
    (progn
      (setq code (prolog-append prologs code))				  
      (setq prologs nil))
    #+:series-plain
    (when wrps
      (setq code (wrap-code wrps code)))
    (cl:let ((last-form (car (last (if code code (last-prolog-block prologs))))))
      (if (and rets (null (cdr rets)))
	  (cl:let ((r (car rets)))
	    (cond ((and (eq-car last-form 'setq)
			(eq-car (cdr last-form) r))
		   (if code
		       (setq code (delete last-form code))
		     (setq prologs (delete-last-prolog prologs)))
		   (when (and (not (contains-p r code))
			      #-:series-plain (not (contains-p r prologs))
			      )
		     (setq aux (delete-aux r aux)))
		   (setq rets (caddr last-form)))
		  (t (setq rets r))))
	(setq rets `(values ,@ rets))))
    (setq code (codify-1 aux prologs (nconc code (list rets))))
    #-:series-plain
    (when wrps
      (setq code (car (wrap-code wrps (list code)))))
    (use-user-names aux code)
    (when *lift-out-vars-p*
      (setf code (lift-out-vars code)))
    (setq *last-series-loop* code)))

); end of eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is used when optimization is not possible.
  ;;
  ;; It makes one main physical frag that computes the series returned
  ;; by frag. (If there is more than one output, then several
  ;; subsidiary frags have to be created to pick the right values
  ;; out.)
  ;;
  ;; It assumes that actual-args must be a list of variables.
  
  ;; This is only called from frag->physical
  (cl:defun precompute-frag->physical (frag alter-prop-alist)
    (dolist (r (rets frag))
      (when (series-var-p r)
        (add-physical-out-interface r (cdr (assoc (var r) alter-prop-alist)))))
    (cl:let ((*last-series-loop* nil) (*user-names* nil))
      (declare (special *last-series-loop* *user-names*))
      (codify frag)))

  
  ;; This is only called from series-frag->physical
  (cl:defun f->p-off-line (i frag new-out out-values done flag)
    (cl:let* ((ret (nth i (rets frag)))
              (off-line-spot (off-line-spot ret))
              (restart (new-var 'restart))
              (out-value (if (null (cdr out-values))
			     (car out-values)
                           `(list ,@(mapcar #'(lambda (r o)
                                                (when (eq r ret) o))
                                            (rets frag) out-values))))
              (new-body-code `((setq ,new-out (cons ,out-value t))
                               (setq ,flag ,i)
                               (go ,done)
                               ,restart)))
      (push `(if (= ,flag ,i) (go ,restart)) (body frag))
      (setf (body frag) (nsubst-inline new-body-code off-line-spot (body frag)))))

  
  ;; This is only called from series-frag->physical
  (cl:defun f->p-on-line (frag new-out out-values done flag)
    (cl:let* ((out-value (if (null (cdr out-values))
			     (car out-values)
                           `(list ,@(mapcar #'(lambda (r o)
                                                (when (not (off-line-spot r)) o))
                                            (rets frag) out-values))))
              (new-body-code `((setq ,new-out (cons ,out-value t))
                               ,@(when flag
				   `((setq ,flag -1)))
                               (go ,done))))
      (setf (body frag) (nconc (body frag) new-body-code))))

  (cl:defun image-of-non-null-datum-th (g datum)
    (cl:let (item)
      (loop (setq item (nth datum (basic-do-next-in g)))
        (when (or (null (gen-state g)) (not (null item)))
	  (return item)))))

  (cl:defun car-image-of-non-null-datum-th (g datum)
    (car (image-of-non-null-datum-th g datum)))

;; This is only called from frag->physical
(cl:defun series-frag->physical (frag alter-prop-alist)
    (cl:let* ((out-values nil)
              (alterers nil)
              (done-on-line nil)
              (new-out (new-var 'item))
              (n (length (rets frag)))
              (done (new-var 'done))
              (rts (rets frag))
              (flag (when (some #'off-line-spot rts)
		      (new-var 'flag)))
              (label (when flag
		       (new-var 'l))))
      (dolist (r (reverse rts))
        (cl:multiple-value-bind (out-value alterer)
          (out-value r (cdr (assoc (var r) alter-prop-alist)) (not (= n 1)))
          (push out-value out-values)
          (push alterer alterers)))
      (when flag
	(add-literal-aux frag flag 'fixnum -1)
        (push label (body frag)))
      (dotimes (i n)
        (cond ((off-line-spot (nth i (rets frag)))
               (f->p-off-line i frag new-out out-values done flag))
              ((not done-on-line)
               (setq done-on-line t)
               (f->p-on-line frag new-out out-values done flag))))
      (cl:let* ((basic-out (new-var 'series-of-lists))
                (code `(make-phys
                        :alter-fn ,(when (= n 1) (car alterers))
                        :gen-fn #'(lambda ()
                                    (cl:let (,new-out)
                                      (tagbody ,@(body frag)
                                               ,@(when (and flag (not done-on-line))
						   `((go ,label)))
                                               ,@(when (branches-to end (body frag)) `(,end))
					       ,@(epilog frag)
                                               (setq ,new-out nil)
                                               ,done)
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
                                              (t '#'image-of-non-null-datum-th))))
                       (n-integers n) (rets frag) alterers)))))
        (codify-1 (aux frag) (prolog frag) (list code)))))

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
) ; end of eval-when

;; Main graph merging function
(cl:defun mergify (*graph*)
  (reset-marks)
  (do-coercion)
  (do-substitution)
  (kill-dead-code)
  (cl:let ((splits (do-splitting *graph*)))
 (eval splits))) 

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
                                                             :initial-element t))))
			     (mapc #'(lambda (f) (fragify f '(values t)))
				   (cdr last-form)))
                           (mergify *graph*)))
              (input-info nil))
    (setf (args frag)  ;get into the right order.  Discard unused args.
          (mapcan #'(lambda (ret a)
                      (cl:let ((arg (car (nxts ret))))
                        (cond ((null arg) ;input never used
                               (cond ((member a ignore-vars) nil)
                                     (t #| ;HERE can get false positives.
                                        (wrs 50 t "~%The input " a " never used.")|#
                                        (list ret)))) ;assume was used anyway.
                              (t ;here probably want to pretend was not declared ignore.
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
                 (not (eq (cadr (cdr e)) t)))
        (push (cons (car e) (cadr (cdr e))) input-info)))
    (doaux (v (aux frag))
       (propagate-types (cdr v) (aux frag) input-info))
    frag))

(cl:defun compute-optimizable-series-fn (definer name lambda-list expr-list)
  "Defines a series function, see lambda-series."
  (cl:let ((call (list* definer name lambda-list expr-list))
           (*optimize-series-expressions* t)
           (*suppress-series-warnings* nil))
    (dolist (v lambda-list)
      (when (and (symbolp v) (not (eq v '&optional))
		 (member v lambda-list-keywords))
        (ers 71 "~%Unsupported &-keyword " v " in OPTIMIZABLE-SERIES-FN arglist.")))
    (top-starting-series-expr call
      (cl:let ((vars nil) (rev-arglist nil))
        (dolist (a lambda-list)
          (cond ((not (member '&optional rev-arglist))
                 (push a rev-arglist)
                 (when (not (eq a '&optional))
		   (push a vars)))
                (t (setq a (iterative-copy-tree a))
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
                                             (when (not (member v ignore-vars))
					       (list v)))
                                         vars))
                      (series-p (some #'(lambda (r) (series-var-p r)) (rets frag)))
                      (frag-list (frag->list frag))
                      (dcls (when ignore-vars
			      `((ignore ,@ ignore-vars)))))
            (check-off-line-ports frag vars off-line-ports)
            (when (and (not series-p) (notany #'series-var-p (args frag)))
              (wrs 44 t
                   "~%OPTIMIZABLE-SERIES-FUNCTION neither uses nor returns a series."))
            `(,name ,(reverse rev-arglist)
               ,(if (not dcls) doc (cons doc `(declare . ,dcls)))
               ,(frag->physical frag used-vars)
               :optimizer
               (apply-frag (list->frag1 ',frag-list) (list ,@ used-vars))
               :trigger ,(not series-p)))))
      (cl:multiple-value-bind (forms decls doc)
          (decode-dcls expr-list '(no-complaints doc opts))
        `(,name ,lambda-list
           ,@(when doc (list doc))
           ,@(when decls `((declare ,@ decls)))
           (compiler-let ((*optimize-series-expressions* nil)) ,@ forms))))))

(cl:defun define-optimizable-series-fn (name lambda-list expr-list)
  (cl:multiple-value-bind (form opt-p)
			 (compute-optimizable-series-fn 'defun name lambda-list expr-list)
    (if opt-p
	(cons 'defS form)
      (cons 'cl:defun form))))
		       
     
(cl:defun undefine-optimizable-series-fn (name)
  (when (symbolp name)
    (remprop name 'series-optimizer)
    (remprop name 'returns-series))
  name)

;; EXTENSION
(defmacro defun (name lambda-list &environment *env* &body body)
  (if (dolist (form body)
        (cond ((and (stringp form) (eq form (car body))))
              ((and (consp form) (eq-car form 'declare))
               (if (assoc 'optimizable-series-function (cdr form)) (return t)))
              (t (return nil))))
      (define-optimizable-series-fn name lambda-list body)
    (progn (undefine-optimizable-series-fn name)
           `(cl:defun ,name ,lambda-list
              . ,body))))

#+symbolics(setf (gethash 'defun zwei:*lisp-indentation-offset-hash-table*)
                 '(2 1))
#+Symbolics
(setf (get 'defun 'zwei:definition-function-spec-parser)
      (get 'cl:defun 'zwei:definition-function-spec-parser))


;;;;                          ---- DEFS ----
;;;; Macro to define arbitrary SERIES functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
	   
  (cl:defun compute-prologing-macform (name body-code)
    `(,name (&rest stuff) 
        (cons ',body-code  stuff)))

  (cl:defun compute-series-funform (name arglist doc dcl body-code opt-p)
    `(,name ,arglist
	,@(when doc (list doc))
	,@(when dcl (list dcl))
	(compiler-let ((*optimize-series-expressions* ,opt-p)) 
          ,body-code)))

  (cl:defun compute-series-macform-2 (name arglist doc body-code trigger 
					   local-p disc-expr opt-expr
					   unopt-expansion)
   #-:symbolics (declare (ignore arglist))
   (cl:let ((unopt (if (symbolp body-code)
		       `(cons ',body-code stuff)
		     unopt-expansion)))
     `(,name (&whole call &rest stuff &environment *env*)
	#+:symbolics (declare (zl:arglist ,@(copy-list arglist)))
	,@(when doc (list doc))
	,(if trigger
	   `(if ,(if (eq trigger t)
		     '*optimize-series-expressions*
		   `(and *optimize-series-expressions* ,trigger))
		,(if local-p
		     (cl:let ((retprop (gensym))
			      (optprop (gensym))
			      (result  (gensym)))
		       `(cl:let ((,retprop (get ',name 'returns-series))
				 (,optprop (get ',name 'series-optimizer)))
		          (setf (get ',name 'returns-series)  (function ,disc-expr))
			  (setf (get ',name 'series-optimizer) (function ,opt-expr))
			  (setq ,result (process-top call))
			  (setf (get ',name 'series-optimizer) ,optprop)
			  (setf (get ',name 'returns-series) ,retprop)
			  ,result))
		   `(process-top call))
	      ,unopt)
	   unopt))))

  (cl:defun compute-series-macform-1 (name arglist body-code body-fn trigger
					   local-p disc-expr opt-expr)
    (compute-series-macform-2 name arglist nil body-code trigger
			      local-p disc-expr opt-expr
      `(list* 'cl:funcall ',body-fn stuff)))

  ;; It's a macro and body can refer to it
  (cl:defun compute-series-macform (name arglist doc dcl body-code body-fn trigger
					 local-p disc-expr opt-expr)
    (compute-series-macform-2 name arglist doc body-code trigger
			      local-p disc-expr opt-expr
      `(macrolet (,(compute-series-macform-1
		    name arglist body-code body-fn trigger
		    local-p disc-expr opt-expr))
	 `(cl:flet (,',(compute-series-funform
		       body-fn arglist doc dcl body-code nil))
	    (,',body-fn ,@stuff)))))

  ;;The body runs when optimization is not happening.
  ;;The optimizer runs when optimization is happening.
  ;;The trigger says whether or not a series expression is beginning.
  ;;  it forces NAME to be a macro instead of a function.
  ;;The discriminator says whether or not series are being returned.
  (cl:defun defS-1 (name arglist doc body optimizer trigger discriminator local-p)
    (cl:let* ((body-code body)
              (dcl (when (consp doc)
		     (prog1 (cdr doc) (setq doc (car doc)))))
              (opt-code (or optimizer body))
              (body-fn (cond ((symbolp body-code) body-code)
                             (trigger (new-var (string name)))))
              (desc-fn (cond (discriminator nil)
                             (trigger 'no)
                             (t 'yes)))
              (opt-arglist      ;makes up for extra level of evaluation.
                (mapcar #'(lambda (a)
                            (if (and (listp a) (listp (cdr a)))
                                (list* (car a) `(copy-tree ',(cadr a)) (cddr a))
			      a))
                        arglist))
	      (disc-expr (if discriminator
			     `(lambda (call) ,discriminator)
			   desc-fn))
	      (opt-expr `(lambda ,@(cdr (compute-series-funform
					   nil opt-arglist nil dcl opt-code t)))))
	(values (cond (trigger 
		       (cons 'defmacro (compute-series-macform
					 name arglist doc dcl body-code body-fn trigger
					 local-p disc-expr opt-expr)))
		      ((symbolp body-code)
		       (cons 'defmacro
			     (compute-prologing-macform name body-code)))
		      ((not (symbolp body-code))
		       (cons 'cl:defun
			      (compute-series-funform
				name arglist doc dcl body-code nil))))
		disc-expr
		opt-expr)))
       

    (defmacro defS (name arglist doc body &key optimizer trigger discriminator)
      (cl:multiple-value-bind (topdef
			       discriminator-expr
			       optimizer-expr)
	  (defS-1 name arglist doc body optimizer trigger discriminator nil)
	`(eval-when (:compile-toplevel :load-toplevel :execute)          
	   ,topdef
	   (setf (get ',name 'returns-series) (cl:function ,discriminator-expr))
	   (setf (get ',name 'series-optimizer) (cl:function ,optimizer-expr))
	   ',name)))

    ;; PROTOTYPE VERSION ONLY - DO NOT USE YET
    (defmacro slet1 (def &body forms)
      (if def
	  (destructuring-bind (name arglist doc body &key optimizer trigger discriminator) def
	    (cl:multiple-value-bind (topdef
				     discriminator-expr
				     optimizer-funform)
		(defS-1 name arglist doc body optimizer trigger discriminator t)
	      (declare (ignore discriminator-expr optimizer-funform))	 		    	    
	      (list* (case (car topdef)
		       ((defmacro) 'macrolet)
		       ((cl:defun) 'cl:flet))
		     (list (cdr topdef))
		     forms)))
	`(progn ,@forms)))

    ;; DO NOT USE YET
    (defmacro slet* (defs &body forms)
      (destarrify 'slet1 defs nil nil nil forms))
    
) ;end of eval-when for defS

;;;;                          ---- fragL ----

(cl:defun apply-literal-frag (frag-and-values)
  (apply-frag (literal-frag (car frag-and-values)) (cdr frag-and-values)))

(eval-when (:compile-toplevel :load-toplevel :execute)

;; this forms are useful for making code that comes out one way in the
;; body and another way in the optimizer

(defmacro optif (f1 f2)
  `(if *optimize-series-expressions* ,f1 ,f2))

(defmacro eoptif (f1 f2)
  (if *optimize-series-expressions* f1 f2))

(defmacro eoptif-q (f1 f2)
  (optif `,f1 `,f2))

(defmacro optif-q (f1 f2)
  `(optif ',f1 ',f2))

(defmacro non-optq (x) `(eoptif ,x (list 'quote ,x)))

(defmacro optq (x) `(eoptif ',x ,x))

(cl:defun apply-physical-frag (stuff args)
  (frag->physical (literal-frag stuff)
		  args))

(cl:defun funcall-physical-frag (stuff &rest args)
  (frag->physical (literal-frag stuff)
		  args))

(declaim (inline unopt-fragl))
(cl:defun unopt-fragl (stuff)
  (apply-physical-frag stuff (mapcar #'car (car stuff))))	  

(cl:defun opt-fragl (stuff inputs)
  `(apply-literal-frag
     (list ,stuff
	   ,@inputs)))

(cl:defun fragl-2 (stuff args)
  (optif
      (opt-fragl `(quote ,stuff) args)
    (apply-physical-frag stuff args)))

#|
(cl:defun fragl-2 (stuff args)
  (optif
      `(apply-literal-frag (list (quote ,stuff) ,args))
    (apply-physical-frag stuff args)))
|#

(cl:defmacro efragl (a stuff)
  (optif	  
      `(funcall-frag (literal-frag (cons ',a ,stuff)) ,@(mapcar #'car a))
    (apply-physical-frag (cons a (eval stuff)) (mapcar #'car a))))

(declaim (inline fragl-1))
(cl:defun fragl-1 (stuff)
  (fragl-2 stuff (mapcar #'car (car stuff))))

(cl:defun sublis-limits (tree)
  (sublis `((*limit*                . ,most-positive-fixnum)
	    (most-positive-fixnum   . ,most-positive-fixnum)
	    (array-total-size-limit . ,array-total-size-limit)
	    (array-dimension-limit  . ,array-dimension-limit))
	  tree))

(cl:defun *fragl-1 (stuff)
  (optif
      (opt-fragl (if (not (contains-p '*type* stuff))
		     (if (not (contains-p '*limit* stuff))
		         `(quote ,stuff)
		       `(subst *limit* '*limit* ',stuff))
		   (if (not (contains-p '*limit* stuff))
		       `(subst *type* '*type* ',stuff)
		     `(sublis `((*type* . ,*type*)
				(*limit* . ,*limit*))
			      ',stuff)))
		 (mapcar #'car (car stuff)))
    (unopt-fragl (list* (car stuff)
		   (cadr stuff)
		   (mapcar #'(lambda (data)
			       (if (or (eq (cadr data) '*type*)
				       (eq-car (cadr data)
					       'series-element-type))
				   (list* (car data) t (cddr data))
				 (list* (car data)
					(sublis-limits (cadr data))
					(cddr data))))
			   (caddr stuff))
		   (sublis-limits (cdddr stuff))))))

) ;end of eval-when for fragl-1

(defmacro fragl (&rest stuff)
  #+symbolics (declare (scl:arglist args rets aux alt prolog body epilog wraprs))
  #-:series-plain
  (fragl-1 stuff)
  #+:series-plain
  (cl:let ((a (caddr stuff)))
    (apply #'fragl-1
	   (car stuff) (cadr stuff) (simplify-aux a)  (cadddr stuff)
	   (nconc (aux->prolog a) (car (cddddr stuff)))
	   (cdr (cddddr stuff))	   
	   )))

(defmacro *fragl (&rest stuff)
  #+symbolics (declare (scl:arglist args rets aux alt prolog body epilog wraprs))
  #-:series-plain
  (*fragl-1 stuff)
  #+:series-plain
  (cl:let ((a (caddr stuff)))
    (apply #'*fragl-1
	   (car stuff) (cadr stuff) (simplify-aux a)  (cadddr stuff)
	   (nconc (aux->prolog a) (car (cddddr stuff)))
	   (cdr (cddddr stuff))	   
	   )))

;;;;                          ---- COLLECT ----

;; We put this SERIES function here so the compiler doesn't think it's
;; a function while compiling gatherers.

;; seq-type must be a subtype of SEQUENCE or BAG.

;; API
(defS collect (seq-type &optional (items nil items-p))
    "(collect [type] series)

Creates a sequence containing the elements of SERIES.  The TYPE
argument specifies the type of sequence to be created.  This type must
be a proper subtype of sequence.  If omitted, TYPE defaults to LIST. "
  (cl:let (*type* limit el-type)
    (unless items-p ;it is actually seq-type that is optional
      (setq items seq-type)
      (setq seq-type (optq 'list)))
    (multiple-value-setq (*type* limit el-type)
      (decode-seq-type (non-optq seq-type)))
    (cond ((eq *type* 'list)
	   (or (matching-scan-p items #'lister-p)
	       (fragl ((items t)) ((lst))
		      ((lastcons cons (list nil))
		       (lst list))
		      ()
		      ((setq lst lastcons))
		      ((setq lastcons (setf (cdr lastcons) (cons items nil))))
		      ((setq lst (cdr lst)))
		      ()
		      nil
		      )))
          ((eq *type* 'bag)
	   (or (matching-scan-p items #'lister-p)
	       (fragl ((items t)) ((lst))
		      ((lst list nil))
		      ()
		      ()
		      ((setq lst (cons items lst)))
		      ()
		      ()
		      nil)))
	  ((eq *type* 'set)
	   (fragl ((items t)) ((lst))
		  ((table t (make-hash-table))
		   (lst list nil))
		  ()
		  ()
		  ((setf (gethash items table) t))
		  ((with-hash-table-iterator (next-entry table)
                     (loop (cl:multiple-value-bind (more key) (next-entry)
                             (unless more (return lst))
                             (push key lst)))))
		  () nil))
	  ((eq *type* 'ordered-set)
	   (fragl ((items t)) ((lst))
		  ((table t (make-hash-table))
		   (lastcons cons (list nil))
		   (lst list))
		  ()
		  ((setq lst lastcons))
		  ((cl:multiple-value-bind (val found)
		       (gethash items table)
		     (declare (ignore val))
		     (unless found
		       (setf (gethash items table) t)
		       (setq lastcons (setf (cdr lastcons) (cons items nil))))))
		  ((setq lst (cdr lst)))
		  () nil))
          (limit
           ;; It's good to have the type exactly right so CMUCL can
           ;; optimize better.
	   (setq *type* (if (consp (cadr seq-type))
			    (cadr seq-type)
			  seq-type))
	   (efragl
	       ((seq-type) (items t) (limit))
	       `(((seq))
	         ((seq ,(optif *type* 'sequence))
		  (index vector-index+ 0))
		 ()              
		 (#-(or :cmu :scl)
		  (setq seq (make-sequence seq-type limit))
		  ;; For some reason seq isn't initialized when
		  ;; *optimize-series-expressions* is nil and this
		  ;; errors out in CMUCL.  This makes sure seq is
		  ;; initialized to something.
		  #+(or :cmu :scl)
		  (setq seq (if seq
				seq
			      (make-sequence seq-type limit)))
		  )
		 ((setf (aref seq (the vector-index index)) items) (incf index))
		 ()
		 ()
		 nil
		 )))
          ((not (eq *type* 'sequence)) ;some kind of array with no dimension
            ;; It's good to have the type exactly right so CMUCL can
	   ;; optimize better.
	   (setq *type* (if (eq *type* 'simple-array)
			    (list *type* el-type '(*))
			  (list *type* el-type)))
	   (bind-if* (l (matching-scan-p items #'lister-p))
		     (apply-literal-frag
		       `((()
			  ((seq)) 
			  ((seq (null-or ,*type*)))
			  ()
			  ()
			  ()
			  ((cl:let* ((lst ,l)
				     (num (length lst)))
			     (declare (type nonnegative-integer num))
			     (setq seq (make-sequence ,seq-type num))
			     (dotimes (i num)
			       (setf (aref seq i) (pop lst)))))
			  ()
			  nil
			  )
			 ))
             (setq *type* (make-nullable *type*))
	     (efragl ((seq-type) (items t))
		     `(((seq)) 
		       ((seq ,(optif *type* 'sequence) nil)
			(lst list))
		       ()
		       ()
		       ((setq lst (cons items lst)))
		       ((cl:let ((num (length lst)))
				(declare (type nonnegative-integer num))
				(setq seq (make-sequence seq-type num))
				(do ((i (1- num) (1- i))) ((minusp i))
				  (setf (aref seq i) (pop lst)))))
		       ()
		       nil
		       ))))
	  
          (t
	     (fragl ((seq-type) (items t)) ((seq))
                    ((seq t)
		     (limit (null-or nonnegative-integer)
			    (cl:multiple-value-bind (x y)
                              (decode-seq-type (list 'quote seq-type))
			      (declare (ignore x))
			      y)) ; y is not restricted to fixnum!
		     (lst list nil))
		    ()
                    ()
                    ((setq lst (cons items lst)))
                    ((cl:let ((num (length lst)))
		       (declare (type nonnegative-integer num))    
                       (setq seq (make-sequence seq-type (or limit num)))
                       (do ((i (1- num) (1- i))) ((minusp i))
                         (setf (elt seq i) (pop lst)))))
		    ()
		    nil
		    ))))
  :trigger t)

;;;;                          ---- GATHERERS ----

;;; The following functions support gatherers. No optimization ever
;;; applies to gatherers except in PRODUCING and GATHERING.  A
;;; gatherer is a function of two arguments.  If the second argument
;;; is NIL, the first argument is added into the accumulator of the
;;; gatherer.  If the second argument is not NIL, the accumulated
;;; result is returned.  It is an error to call the gatherer again
;;; after the accumulated result has been returned.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro gather-next (gatherer item)
    `(cl:funcall ,gatherer ,item nil))

  (defmacro gather-result (gatherer)
    `(cl:funcall ,gatherer nil t))
  	   
) ; end of eval-when

(cl:defun next-out (gatherer item)
  (gather-next gatherer item)
  nil)

(cl:defun result-of (gatherer)
  (gather-result gatherer))

;; this assumes the frag is a one-in one-out collector and that if
;; there are wrappers, they are only relevant to the epilog.
(cl:defun gathererify (frag)
  (when (off-line-spot (car (args frag)))
    (convert-to-reducer (car (args frag))))
  (cl:let ((code `(tagbody ,@(body frag)))
	   (ecode `(progn ,@(epilog frag)))
	   (wrps (wrappers frag)))
   (setq ecode (apply-wrappers wrps ecode #'epilog-wrapper-p))
   (setf (wrappers frag) (delete-if #'epilog-wrapper-p wrps))
   (codify-1 (aux frag)
	     (prolog frag)
	     `((function (lambda (,(var (car (args frag))) result-p)
			     (cond ((null result-p) ,code)
				   (t ,ecode ,(var (car (rets frag)))))))))))

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


(cl:defun gather-sanitize (collector)
  (cl:let ((x (new-var 'gather)))
    `#'(lambda (,x) (funcall ,collector (scan (collect ,x))))))

(cl:defun gatherlet-1 (binder bindifier decls var-collector-pairs *env* body)
  (cl:let* ((frags (mapcar #'(lambda (p) (frag-for-collector (cadr p) *env*))
			   var-collector-pairs))
	    (stuff (mapcar #'gathererify frags))
	    (fns (mapcar #'(lambda (p s) (cons (car p) (cl:funcall bindifier
								   (cl:let ((f (car (last s))))
								     (if (and (listp f)
									      (eq (car f) 'locally))
									 (car (last f))
								       f)))))
			 var-collector-pairs stuff))
	    (fullbody `(,binder ,fns ,@decls ,@body)))
    (dolist (s (reverse stuff))
      (cl:let ((f (car (last s))))
        (setq fullbody (nconc (butlast s)
			      (list (if (and (listp f)
					     (eq (car f) 'locally))
					(nconc (butlast f) (list fullbody))
				      fullbody))))))
    (cl:let ((wrps (mapcan #'(lambda (f)
			       (prog1 (frag-wrappers f) (setf (wrappers f) nil)))
			   frags)))
      (setq fullbody (apply-wrappers wrps fullbody)))
    fullbody))

(cl:defun gathering-1 (binder bindifier  result-op decls var-collector-pairs *env* body)
  (cl:let ((returns (mapcar #'(lambda (p) `(,result-op ,(car p)))
			     var-collector-pairs)))
    (gatherlet-1 binder bindifier decls var-collector-pairs *env*
		  `(,@body ,(if (= (length returns) 1)
				(car returns)
			      `(values ,@returns))))))

(eval-when (:load-toplevel :execute)
	   
  (defmacro fgather-next (gatherer item)
    `(,gatherer ,item nil))

  (defmacro fgather-result (gatherer)
    `(,gatherer nil t))

  (defmacro gatherer (collector &environment *env*)
    (unless (or (eq-car collector 'function)
		   (eq-car collector 'lambda))
    (cl:let ((x (new-var 'gather)))
      (setq collector
            `#'(lambda (,x)
                 (cl:funcall ,collector (cl:funcall #'scan (collect ,x)))))))
    (cl:let ((frag (frag-for-collector (if (eq-car collector 'function)
					   (cadr collector)
					 collector) *env*)))
    (when (wrappers frag)
      (cl:let ((x (new-var 'gather)))
        (setq frag (frag-for-collector
                     `(lambda (,x) (funcall ,collector (scan (collect ,x))))
                     *env*))))
    (gathererify frag)))

(defmacro gatherlet (var-collector-pairs &environment *env* &body body)
  (gatherlet-1 'cl:let #'list nil var-collector-pairs *env* body))

(defmacro fgatherlet (var-collector-pairs &environment *env* &body body)
  (gatherlet-1 'cl:flet #'cdadr nil var-collector-pairs *env* body))

(defmacro gathering (var-collector-pairs &environment *env* &body forms)
  (cl:multiple-value-bind (decls body)
      #+:cltl2-series (values nil forms)
      #-:cltl2-series (dynamize-vars (mapcar #'car var-collector-pairs) forms #'eq)
    (gathering-1 'cl:let #'list 'gather-result decls var-collector-pairs *env* body)))

(defmacro fgathering (var-collector-pairs &environment *env* &body forms)
  (cl:multiple-value-bind (decls body)
      #+:cltl2-series (values nil forms)			  
      #-:cltl2-series (dynamize-vars (mapcar #'(lambda (x)
						 `(function ,(car x)))
						     var-collector-pairs)
				     forms
				     #'equal)
    (gathering-1 'cl:flet #'cdadr 'fgather-result decls var-collector-pairs *env* body)))
) ; end of eval-when
;;;;                  ---- SERIES FUNCTION LIBRARY ----

(cl:defun process-top (call)
  (when (and *series-expression-cache*
             (not (hash-table-p *series-expression-cache*)))
    (setq *series-expression-cache* (make-hash-table :test #'eq)))
  (cl:let ((cached-value (and *series-expression-cache*
                           (gethash call *series-expression-cache*))))
    (cond (cached-value)
          (t (setq cached-value
                   (top-starting-series-expr call
		      (codify (mergify (graphify call)))	     
                      `(compiler-let ((*optimize-series-expressions* nil)) ,call)))
             (when *series-expression-cache*
               (setf (gethash call *series-expression-cache*) cached-value))
             cached-value))))

;; The next few things are optimizers that hang on standard symbols.
;;
;; Note the cludging we have to do when the first var is a let-series
;; var. This is necessary in case this value is going to have to be a
;; return value as well. We really should have done something better
;; about specifing free variable outputs so that this mess would not
;; be necessary.
(cl:defun my-multi-setq (vars value form)
  (cl:let* ((type (if (null (cdr vars))
		      t
		    `(values ,@(make-list (length vars) :initial-element t))))
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
		   (add-aux frag v t)
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
                         (t (append-prolog frag assignment))))))
              ((series-var-p out)
               (rrs 11 "~%series value assigned to free variable~%" form))
              (t (if (or (body frag) (epilog frag))
		     (setf (epilog frag)
			   (append (epilog frag) `((setq ,v ,(var out)))))
                   (append-prolog frag `((setq ,v ,(var out)))))
                 (when (not (eq out (car (rets frag))))
                   (kill-ret out))))))
    frag))

(cl:defun setq-opt (var exp)
  (my-multi-setq (list var) exp `(setq ,var ,exp)))

(setf (get 'setq 'series-optimizer) #'setq-opt)
(setf (get 'setq 'returns-series) #'no) ;here should be better than this

(cl:defun multiple-value-setq-opt (vars exp)
  (my-multi-setq vars exp `(multiple-value-setq ,vars ,exp)))

(setf (get 'multiple-value-setq 'series-optimizer) #'multiple-value-setq-opt)
(setf (get 'multiple-value-setq 'returns-series) #'no)  ;here should be better than this


(cl:defun produces-optimizable-series (original-code)
  (cl:let ((flag t) pred (code original-code))
    (loop
      (cond ((not (and flag (consp code) (symbolp (car code))))
	     (return nil))
	    ((eq (car code) 'values)
	     (return (some #'produces-optimizable-series (cdr code))))
	    ((eq (car code) 'the)
	     (return (produces-optimizable-series (caddr code))))
	    (t
	     (setq pred (get (car code) 'returns-series))
	     (cond (pred
		    (return (cl:funcall pred code)))
		   ((not-expr-like-special-form-p (car code))
		    (return nil))
		   ((not (macro-function (car code))) 
		    (return (some #'produces-optimizable-series (cdr code))))
		   (t
		    (when (eq code original-code)
		      (setq code (iterative-copy-tree code)))
		    (multiple-value-setq (code flag) (macroexpand-1 code *env*)))))))))

;; EXTENSION
(defS funcall (function &rest expr-list) "" cl:funcall
 :optimizer
  (cond ((and (eq-car function 'function) (symbolp (cadr function))
              (get (cadr function) 'series-optimizer))
         (cons (cadr function) expr-list))
        ((not (simple-quoted-lambda function))
         (list* 'cl:funcall function expr-list))
        ((not (= (length expr-list)
		 (length (simple-quoted-lambda-arguments function))))
         (ers 67 "~%Wrong number of args to funcall:~%" (cons function expr-list)))
        (t `(let ,(mapcar #'list (simple-quoted-lambda-arguments function) expr-list)
	      ,@(simple-quoted-lambda-body function))))
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
             (produces-optimizable-series
	      (car (last (simple-quoted-lambda-body function))))))))

; Binding forms processing

;; HELPER
(cl:defun normalize-pair (p allow-multiple-vars)
  (cond ((variable-p p) (list (list p) nil))
        ((and (consp p) (variable-p (car p)) (= (length p) 2))
         (list (list (car p)) (cadr p)))
        ((and (consp p) (variable-p (car p)) (= (length p) 1))
         (list (list (car p)) nil))
        ((and allow-multiple-vars (consp p) (consp (car p))
              (every #'variable-p (car p))
              (= (length p) 2)) p)
        (t (ers 66 "~%Malformed binding pair " p "."))))

(cl:defun process-let-series-pair (p type-alist allow-multiple-vars)
  (setq p (normalize-pair p allow-multiple-vars))
  (cl:let* ((vars (car p))
              (types (mapcar #'(lambda (v) (or (cdr (assoc v type-alist)) t))
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

(cl:defun process-let-forms (forms)
  (mapc #'(lambda (f) (fragify f '(values))) (butlast forms))
  (fragify (car (last forms)) '*)) ;forces NIL if no forms.

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

;; EXTENSION
(defS multiple-value-bind (vars values &rest body) "" cl:multiple-value-bind
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars opt-decls)
      (decode-dcls body '(types ignores generic-opts))
    (declare (ignore opt-decls)) 
    (cl:let* ((bindings (process-let-series-pair (list vars values)
                                                        type-alist t))
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

;; EXTENSION
(defS let (pairs &rest body) "" cl:let
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars opt-decls)
      (decode-dcls body '(types ignores generic-opts))
    (declare (ignore opt-decls))
    (cl:let* ((bindings (mapcan #'(lambda (p)
                                      (process-let-series-pair p type-alist nil))
                                  pairs))
                (*renames* (revappend bindings *renames*)))
      (process-let-series-body ignore-vars forms bindings)))
 :trigger
  (dolist (pair (cadr call) nil)
    (when (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
      (return t)))
 :discriminator (produces-optimizable-series (car (last call))))

#+symbolics(setf (gethash 'let zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))

(setf (get 'cl:let 'series-optimizer) (get 'let 'series-optimizer))
(setf (get 'cl:let 'returns-series) (get 'let 'returns-series))

;; EXTENSION
(defS let* (pairs &rest body) "" cl:let*
 :optimizer
  (cl:multiple-value-bind (forms type-alist ignore-vars opt-decls)
      (decode-dcls body '(types ignores generic-opts))
    (declare (ignore opt-decls))
    (cl:let* ((old-top *renames*)
	      (*renames* *renames*))
      (dolist (p pairs)
        (setq *renames*
              (nconc (process-let-series-pair p type-alist nil) *renames*)))
      (process-let-series-body ignore-vars forms (ldiff *renames* old-top))))
 :trigger
  (dolist (pair (cadr call) nil)
    (when (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
      (return t)))
 :discriminator (produces-optimizable-series (car (last call))))

#+symbolics(setf (gethash 'let* zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))

(setf (get 'cl:let* 'series-optimizer) (get 'let* 'series-optimizer))
(setf (get 'cl:let* 'returns-series) (get 'let* 'returns-series))

;; Next we have the definitions of the basic higher order functions.

(declaim (special *state*))

;; Helping functions

(cl:defun list-of-next (at-end list-of-generators)
 (mapcar #'(lambda (g) (do-next-in g at-end)) list-of-generators))

(cl:defun values-of-next (at-end list-of-generators)
 (polyapply #'(lambda (g) (do-next-in g at-end)) list-of-generators))

;; HELPER
;;
;; If function is not a simple quoted function, then a non-series
;; input is added to frag, and a parameter is added to params so that
;; the function will get processed right.
(cl:defun handle-fn-arg (frag function params)
  (unless (or (and (eq-car function 'function)
		   (or (symbolp (cadr function))
		       (and (eq-car (cadr function) 'lambda)
			    (every #'(lambda (a)
				       (and (symbolp a)
					    (not (member a lambda-list-keywords))))
				   (cadr (cadr function))))))
	      (and (eq-car function 'lambda)
		   (every #'(lambda (a)
			      (and (symbolp a)
				   (not (member a lambda-list-keywords))))
			  (cadr function))))
    (cl:let ((fn-var (new-var 'function)))
      (+arg (make-sym :var fn-var) frag)
      (setq params (nconc params (list function)))
      (setq function fn-var)))
  (values function params))

;; HELPER
;;
;; This makes code for `(multiple-value-setq ,out-vars (funcall ,fn ,@
;; in-vars)). It always returns a list of a single statement.  Also,
;; any free references to series::let vars are made non-series inputs
;; of frag and hooked up to the right things.  (Note macro expansion
;; has to be done in a nested context so that nested series
;; expressions will be ok.) (Note also that this has to bypass what
;; usually happens when macroexpanding function quoted things.  Things
;; should be sructured differently so that this is not necessary.)
(cl:defun handle-fn-call (frag out-vars fn in-vars &optional (last? nil))
  (declare (type list out-vars)
           (type list in-vars))
  (cl:let ((*in-series-expr* nil) (*not-straight-line-code* nil)
	   (*user-names* nil) (*renames* *renames*) (fn-quoted? nil))
    (when (eq-car fn 'lambda)
      (setq fn-quoted? t))
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
	      (add-aux frag (car entry) t))
            (setf (free-out new) v)
            (+ret new frag)
            (rplacd (assoc v *renames*) new))))
      (setq fn (if (not fn-quoted?)
		   `(cl:funcall ,fn)
		 `(,fn)))
      (cond ((null out-vars) `((,@ fn ,@ in-vars)))
            ((= (length out-vars) 1)
             `((setq ,(car out-vars) (,@ fn ,@ in-vars))))
            (t `((multiple-value-setq ,out-vars (,@ fn ,@ in-vars))))))))

;; HELPER
(cl:defun must-be-quoted (type)
  (declare (type (or list symbol) type))
  (cond ((eq-car type 'quote) (cadr type))
        ((member type '(t nil)) type)
        (t (rrs 2 "~%Non-quoted type " type "."))))


;; API
(defS map-fn (type function &rest args)
    "(map-fn type function &rest series-inputs)

The higher-order function map-fn supports the general concept of
mapping. The TYPE argument is a type specifier indicating the type of
values returned by FUNCTION. The values construct can be used to
indicate multiple types; however, TYPE cannot indicate zero values. If
TYPE indicates m types , then map-fn returns m series T1, ..., Tm,
where Ti has the type (series ). The argument FUNCTION is a
function. The remaining arguments (if any) are all series. "
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1)
           (fragl ((function) (args)) ((items t))
                  ((items t)
		   (list-of-generators list (mapcar #'generator args)))
		  ()
		  ()
                  ((setq items (cl:multiple-value-call function
						       (values-of-next #'(lambda () (go end))
								       list-of-generators))))
		  ()
		  ()
		  :fun ; Assumes impure function for now
		  ))
          (t (values-lists n (apply #'map-fn t
                                    #'(lambda (&rest vals)
                                        (multiple-value-list
                                          (apply function vals)))
                                    args)))))
 :optimizer
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	    (params nil)
	    (frag (make-frag :impure :fun))
	    (in-vars (n-gensyms (length args) (symbol-name '#:m-)))
	    (out-vars (n-gensyms (length types) (symbol-name '#:items-)))
	    (*state* nil))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p t) frag))
    (setf (aux frag)
	  (makeaux (mapcar #'list out-vars types)))
    (multiple-value-setq (function params)
      (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p t) frag))
    (setf (body frag) (handle-fn-call frag out-vars function in-vars t))
    (apply-frag frag params)))

;; OPTIMIZER
(cl:defun scan-fn-opt (wrap-fn inclusive-p type init step
                                 &optional (test nil test-p))
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
            (params nil)
            (frag (make-frag :impure :fun))
            (state-vars (n-gensyms (length types) (symbol-name '#:state-)))
            (out-vars (n-gensyms (length types) (symbol-name '#:items-)))
            (*state* nil))
    (when wrap-fn
      (add-wrapper frag wrap-fn))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p t) frag))
    (setf (aux frag)
          (makeaux (append (mapcar #'list state-vars types)
                           (mapcar #'list out-vars types))))
    (multiple-value-setq (init params) (handle-fn-arg frag init params))
    (multiple-value-setq (step params) (handle-fn-arg frag step params))
    (when test-p
      (multiple-value-setq (test params)
        (handle-fn-arg frag test params)))
    (setq params (mapcar #'retify params))
    (setf (prolog frag) (makeprolog (handle-fn-call frag state-vars init
nil)))
    (cl:let ((output-expr `(setq ,@(mapcan #'list out-vars state-vars))))
      (if (not inclusive-p)
          (setf (body frag)
                `(,@(if test-p
                        `((if ,(car (handle-fn-call frag nil test
state-vars))
                              (go ,end))))
                    ,output-expr
                    ,(car (handle-fn-call frag state-vars step state-vars
t))))
        (cl:let ((done (new-var 'd)))
          (add-literal-aux frag done 'boolean nil)
          (setf (body frag)
                `((if ,done (go ,end))
                  ,(car (handle-fn-call frag (list done) test state-vars))
                  ,output-expr
                  (if (not ,done)
                      ,(car (handle-fn-call frag state-vars step state-vars
t))))))))
    (apply-frag frag params)))

;; OPTIMIZER
(cl:defun collect-fn-opt (wrap-fn type inits function &rest args)
  (declare (type list args))
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	    (params nil)
	    (frag (make-frag :impure :fun))
	    (in-vars (n-gensyms (length args) (symbol-name '#:items-)))
	    (out-vars (n-gensyms (length types) (symbol-name '#:c-)))
	    (*state* nil))
    (when wrap-fn
      (add-wrapper frag wrap-fn))
    (dolist (var out-vars)
      (+ret (make-sym :var var) frag))
    (setf (aux frag)
	  (makeaux (mapcar #'list out-vars types)))
    (multiple-value-setq (inits params) (handle-fn-arg frag inits params))
    (multiple-value-setq (function params) (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p t)
            frag)) ;must be before other possible args
    (setf (prolog frag) (makeprolog (handle-fn-call frag out-vars inits nil)))
    (setf (body frag)
          (handle-fn-call frag out-vars function (append out-vars in-vars) t))
    (apply-frag frag params)))

;; needed because collect is a macro
(cl:defun basic-collect-list (items)
  (compiler-let ((*optimize-series-expressions* nil))
    (fragl ((items t)) ((lst))
	   ((lastcons cons (list nil))
	    (lst list))
	   ()
	   ((setq lst lastcons))
	   ((setq lastcons (setf (cdr lastcons) (cons items nil))))	   
           ((setq lst (cdr lst)))
	   ()
	   nil
	   )))

(cl:defun scan-multi-out->scan-list-out (fn type init step test)
  (compiler-let ((*optimize-series-expressions* nil))
    (cl:let ((n (length (decode-type-arg type))))
      (cl:flet ((new-init () (forceL n (multiple-value-list (cl:funcall init))))
		(new-step (state) (forceL n (multiple-value-list (apply step state))))
		(new-test (state) (apply test state)))
	(declare (indefinite-extent #'new-init #'new-step #'new-test))
        (cl:funcall fn t #'new-init #'new-step #'new-test)))))

(defmacro encapsulated-macro (encapsulating-fn scanner-or-collector)
  (unless (or (eq-car encapsulating-fn 'function)
	      (eq-car encapsulating-fn 'lambda))
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
        (t (ers 69 "~%Malformed second arg to ENCAPSULATING arg "
                scanner-or-collector "."))))

;; API
(defS encapsulated (encapsulating-fn scanner-or-collector)
    "Specifies an encapsulating form to be used with a scanner or collector."
  encapsulated-macro
 :optimizer
  (progn
    (unless (or (eq-car encapsulating-fn 'function)
		(eq-car encapsulating-fn 'lambda))
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
          (t (ers 69 "~%Malformed second arg to ENCAPSULATING arg "
                  scanner-or-collector "."))))

 :trigger t
 :discriminator (or (eq-car (caddr call) 'scan-fn)
                    (eq-car (caddr call) 'scan-fn-inclusive)))

;;needed because collect-fn is macro
(cl:defun basic-collect-fn (inits function &rest args)
  (declare (dynamic-extent args))	  
  (declare (type list args))
  (cl:flet ((gen-unopt ()
	      (compiler-let ((*optimize-series-expressions* nil))
	        (fragl ((inits) (function) (args))
		       ((result))
		       ((result t (cl:funcall inits))
			(list-of-generators list (mapcar #'generator args)))
		       ()
		       ()
		       ((setq result (cl:multiple-value-call function
							     result
							     (values-of-next #'(lambda () (go end))
									     list-of-generators))))
		       ()
		       ()
		       :fun		; assumes function is impure for now
		       ))))
    (eoptif-q
        (funcase (fun inits)
	  ((name anonymous)
	   (efragl ((function) (args))
		   `(((result))
		     ((result t (,(if (symbolp fun) fun (process-fn fun))))
		      (list-of-generators list (mapcar #'generator args)))
		     ()
		     ()
		     ((setq result (cl:multiple-value-call function
							   result
							   (values-of-next #'(lambda () (go end))
									   list-of-generators))))
		     ()
		     ()
		     :fun		; assumes function is impure for now
		     )))
            (t
	     (funcase (f function)
	       ((name anonymous)
		(efragl ((inits) (args)) 
			`(((result))
			  ((result t (cl:funcall inits))
			   (list-of-generators list (mapcar #'generator args)))
			  ()
			  ()
			  ((setq result (cl:multiple-value-call (function ,f)
								result
								(values-of-next #'(lambda () (go end))
										list-of-generators))))
			  ()
			  ()
			  :fun		; assumes function is impure for now
			  )))
	       (t
		(gen-unopt)))))
      (gen-unopt))))

;; API
(defS collect-fn (type inits function &rest args)
   "(collect-fn type init-function function &rest args)

Computes a cumulative value by applying FUNCTION to the elements of ITEMS.
INIT-FUNCTION specifies the initial value(s).  Like COLLECTING-FN, but
only the last element of each series is returned."
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1) (apply #'basic-collect-fn inits function args))
          (t (values-list
               (apply #'basic-collect-fn
                      #'(lambda ()
                          (forceL n (multiple-value-list (cl:funcall inits))))
                      #'(lambda (state &rest args)
			  (declare (dynamic-extent args))	  
                          (forceL n (multiple-value-list
                                      (apply function (nconc state args)))))
                      args)))))
 :optimizer
  (apply #'collect-fn-opt nil type inits function args)
 :trigger t)

;;; Hint to users: to avoid inits, add an extra init that acts like a flag.

;; API
(defS collecting-fn (type inits function &rest args)
  "(collecting-fn type init function &rest series-inputs)

Computes cumulative values by applying FUNCTION to the elements of
ITEMS.  TYPE specifies the type of values returned by FUNCTION.  INIT
is a function that returns the initial values for the series.  The
output, t1,..., tm, are computed as follows:

 (values t1[0] ... tm[0] =
	(multiple-value-call function (funcall init) s1[0] ... sn[0])
	
 (values t1[j] ... tm[j] =
	(funcall function t1[j-1] ... tm[j-1] s1[j] ... sn[j])

where s1[j],...,sn[j] are the j'th elements of the n input series.	
"
  (cl:let ((n (length (decode-type-arg type))))
    (setq args (copy-list args))
    (cond ((= n 1)
           (fragl ((inits) (function) (args)) ((result t))
                  ((result t (cl:funcall inits))
		   (list-of-generators list (mapcar #'generator args)))
		  ()
		  ()
                  ((setq result (cl:multiple-value-call function
							result
							(values-of-next #'(lambda () (go end))
									list-of-generators))))
		  ()
		  ()
		  :fun ; assumes function is impure for now
		  ))
          (t (values-lists n
                           (apply #'collecting-fn t
                                  #'(lambda ()
                                      (forceL n (multiple-value-list (cl:funcall inits))))
                                  #'(lambda (state &rest args)
				      (declare (dynamic-extent args))	  
                                      (forceL n (multiple-value-list
                                                 (apply function (append state args)))))
                                  args)))))
  :optimizer
  (cl:let* ((types (decode-type-arg (must-be-quoted type)))
	    (params nil)
	    (frag (make-frag :impure :fun))
	    (in-vars (n-gensyms (length args) (symbol-name '#:items-)))
	    (out-vars (n-gensyms (length types) (symbol-name '#:c-)))
	    (*state* nil))
    (dolist (var out-vars)
      (+ret (make-sym :var var :series-var-p t) frag))
    (setf (aux frag)
	  (makeaux (mapcar #'list out-vars types)))
    (multiple-value-setq (inits params) (handle-fn-arg frag inits params))
    (multiple-value-setq (function params) (handle-fn-arg frag function params))
    (setq params (mapcar #'retify (nconc params args)))
    (dolist (var in-vars)
      (+arg (make-sym :var var :series-var-p t) frag))
    (setf (prolog frag) (makeprolog (handle-fn-call frag out-vars inits nil)))
    (setf (body frag)
          (handle-fn-call frag out-vars function (append out-vars in-vars) t))
    (apply-frag frag params)))

;; API
(defS scan-fn (type init step &optional (test nil test-p))
  "(scan-fn type init step &optional test)

Enumerates a series.  TYPE specifies the type of the value(s) returned
by INIT and STEP.  The elements are computed as follows:

 (values t1[0] ... tm[0]) = (funcall init)
 (values t1[j] ... tm[j]) = (funcall step t1[j-1] ... tm[j-1])

where t1, ..., tm are the output series.

If TEST is not specified, the output is unbounded.  If TEST is
specified, the output is terminated when (funcall tests t1[j]
... tm[j]) is non-NIL.  The elements that cause termination are 
not part of the output."
  (cl:let ((n (length (decode-type-arg type))))
    (when (not test-p)
      (setq test #'never))
    (cond ((= n 1)
           (fragl ((init) (step) (test)) ((prior-state t))
                  ((state t (cl:funcall init))
		   (prior-state t))
		  ()
                  ()
                  ((if (cl:funcall test state) (go end))
                   (prog1 (setq prior-state state)
                     (setq state (cl:funcall step state))))
		  ()
		  ()
		  :fun ; assumes init step and test are impure for now
		  ))
          (t (cl:let ((data (scan-multi-out->scan-list-out
                                #'scan-fn type init step test)))
               (values-lists n data)))))
  :optimizer
  (if test-p
      (scan-fn-opt nil nil type init step test)
    (scan-fn-opt nil nil type init step)))

;; API
(defS scan-fn-inclusive (type init step test)
  "(scan-fn-inclusive type init step &optional test)

Enumerates a series.  TYPE specifies the type of the value(s) returned
by INIT and STEP.  The elements are computed as follows:

 (values t1[0] ... tm[0]) = (funcall init)
 (values t1[j] ... tm[j]) = (funcall step t1[j-1] ... tm[j-1])

where t1, ..., tm are the output series.

If TEST is not specified, the output is unbounded.  If TEST is
specified, the output is terminated when (funcall tests t1[j]
... tm[j]) is non-NIL.  The elements that cause termination are 
part of the output."
  (cl:let ((n (length (decode-type-arg type))))
    (cond ((= n 1)
           (fragl ((init) (step) (test)) ((prior-state t))
                  ((state t (cl:funcall init))
		   (prior-state t) (done t nil))
		  ()
                  ()
                  ((if done (go end))
                   (setq done (cl:funcall test state))
                   (prog1 (setq prior-state state)
                     (if (not done) (setq state (cl:funcall step state)))))
                  ()
		  ()
		  :fun ; assumes init step and test are impure for now
		  ))
          (t (cl:let ((data (scan-multi-out->scan-list-out
                                #'scan-fn-inclusive type init step test)))
               (values-lists n data)))))
  :optimizer
  (scan-fn-opt nil t type init step test))

;;; various easy ways of doing mapping

;; CODEGEN
(cl:defun mapit (type fn args)
  (if (not (symbolp fn))
      `(map-fn ',type (function ,fn) ,@ args)
    (cl:let ((vars (do ((a args (cdr a))
                        (l nil (cons (gensym "V-") l)))
                       ((null a) (return l)))))
      `(map-fn ',type (function (lambda ,vars (,fn ,@ vars))) ,@ args))))

;; put on #M
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

; (set-dispatch-macro-character #\# #\M (cl:function abbreviated-map-fn-reader))

(defmacro \#M (fn &rest args) (mapit t fn args))
(defmacro \#1M (fn &rest args) (mapit t fn args))
(defmacro \#2M (fn &rest args) (mapit '(values t t) fn args))
(defmacro \#3M (fn &rest args) (mapit '(values t t t) fn args))
(defmacro \#4M (fn &rest args) (mapit '(values t t t t) fn args))
(defmacro \#5M (fn &rest args) (mapit '(values t t t t t) fn args))

;; API
(defS collect-ignore (items)
    "Reads input and returns NIL."
  (fragl ((items t)) (nil) () () () () () () nil)
 :trigger t)

(defmacro iterate-mac (var-value-list &rest body)
    "Applies BODY to each element of the series"
  `(collect-ignore (mapping ,var-value-list ,@ body)))

;; API
(defS iterate (var-value-list &rest body)
  "(iterate ({({var | ({var}*)} value)}*) {declaration}* {form}*)

Applies BODY to each element of the series, like MAPPING, but results
are discarded."
  iterate-mac
 :optimizer
  `(iterate-mac ,var-value-list ,@ body)
 :trigger t)

#+symbolics(setf (gethash 'iterate zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))

;; API
(defS mapping (var-value-list &rest body)
    "(mapping ({({var | ({var}*)} value)}*) {declaration}* {form}*)

The macro MAPPING makes it easy to specify uses of MAP-FN where TYPE
is t and the FUNCTION is a literal lambda. The syntax of mapping is
analogous to that of let. The binding list specifies zero or more
variables that are bound in parallel to successive values of
series. The value part of each pair is an expression that must produce
a series. The declarations and forms are treated as the body of a
lambda expression that is mapped over the series values. A series of
the first values returned by this lambda expression is returned as the
result of mapping."
  mapping-mac
 :optimizer
  (cl:let* ((bindings (mapcan #'(lambda (p) (process-let-series-pair p nil t))
                                var-value-list))
              (*renames* (revappend bindings *renames*)))
    (process-let-series-body nil
       `((map-fn t #'(lambda ,(mapcar #'car bindings) ,@ body)
               ,@(mapcar #'car bindings)))
       bindings)))

#+symbolics
(setf (gethash 'mapping zwei:*lisp-indentation-offset-hash-table*)
      '(1 1))

;; only used when optimization not possible.
(defmacro mapping-mac (var-value-list &body body)
  (setq var-value-list (mapcar #'(lambda (p) (normalize-pair p t)) var-value-list))
  (cond ((every #'(lambda (p) (null (cdar p))) var-value-list)
         `(map-fn t
                  #'(lambda ,(mapcar #'caar var-value-list) ,@ body)
                  ,@(mapcar #'cadr var-value-list)))
        ((null (cdr var-value-list))
         `(multiple-value-bind ,@(car var-value-list)
            (map-fn t #'(lambda ,(copy-list (caar var-value-list)) ,@ body)
                    ,@(copy-list (caar var-value-list)))))
        (t `(apply #'map-fn t
                   #'(lambda ,(apply #'append (mapcar #'car var-value-list))
                       ,@ body)
                   (nconc ,@(mapcar #'(lambda (p)
                                        (if (null (cdar p))
					    `(list ,(cadr p))
					  `(forceL ,(length (car p))
						   (multiple-value-list
						    ,(cadr p)))))
                                    var-value-list))))))

;This allows you to specify more or less arbitrary transducers.

;; HELPER

;; This is only called from optimize-producing
(cl:defun protect-from-setq (in type)
  (cl:let ((frag (fr in))
	   (var (var in))
	   (new (new-var 'in)))
    (coerce-to-type type in) ;why am I doing this?
    (if (not (series-var-p in))
	(add-nonliteral-aux (fr in) var type new)
      (progn
	(add-aux (fr in) var type)
	(if (not (off-line-spot in))
	    (push `(setq ,var ,new) (body frag))
	  (nsubst-inline `((setq ,var ,new)) (off-line-spot in) (body frag) t))))
    (setf (var in) new)))

;; CHECKER
(cl:defun validate-producing (output-list input-list body)
  (when (not (and (every #'(lambda (f) (eq-car f 'declare)) (butlast body))
		  (eq-car (car (last body)) 'loop)
		  (eq-car (cadr (car (last body))) 'tagbody)))
    (ers 73 "~%PRODUCING body not of the form ({DECL}* (LOOP (TAGBODY ...)))~%"
	 (list* 'producing output-list input-list body)))
  (when (null output-list)
    (ers 74 "~%PRODUCING fails to have any outputs~%"
	 (list* 'producing output-list input-list body)))
  (mapc #'(lambda (p) (normalize-pair p nil)) output-list)
  (mapc #'(lambda (p) (normalize-pair p nil)) input-list)
  (cl:let ((visited-inputs nil) (visited-outputs nil))
    (dolist (f (cdadar (last body)))
      (cond ((and (eq-car f 'setq)
                  (eq-car (caddr f) 'next-in))
             (when (not (and (null (cdddr f))
                           (cadr (caddr f)) (symbolp (cadr (caddr f)))
                           (find (cadr (caddr f))  input-list
                                 :key #'(lambda (e) (when (consp e) (car e))))
                           (not (member (cadr (caddr f)) visited-inputs))
                           (cddr (caddr f))))
	       (ers 75 "~%Malformed NEXT-IN call: " (caddr f)))
             (push (cadr (caddr f)) visited-inputs))
            ((eq-car f 'next-out)
             (when (not (and (null (cdddr f))
                           (member (cadr f) output-list)
                           (not (member (cadr f) visited-outputs))))
	       (ers 76 "~%Malformed NEXT-OUT call: " f))
             (push (cadr f) visited-outputs))))
    (values visited-inputs visited-outputs)))

;; TRANSLATOR
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
      (cl:flet ((xform-assignment (destination from)
               (list destination
	         (if (eq-car from 'next-in)		       
	             (cl:let* ((source (cadr from))
			       (arg (cdr (assoc source input-alist)))
			       (actions (cddr from))
			       (e (new-var 'ee))
			       (d (new-var 'dd))
			       (-x- (new-var '-xxx-)))
	               (setf (series-var-p arg) t)
		       (cond ((and (eq state :prolog)
				   (equal actions '((terminate-producing)))))
			     ((equal actions '((terminate-producing)))
			      (setf (off-line-spot arg) -x-) (push -x- revbody))
			     (t (setq state :middle)
				(setf (off-line-spot arg) -x-)
				(setf (off-line-exit arg) e)
				(setq revbody
				      (append (reverse `(,-x- (go ,d) ,e ,@ actions ,d))
					      revbody))))
		       (var arg))
		   (progn
		     (setq state :middle)
		     from)))))
	(declare (dynamic-extent #'xform-assignment))
	(cond ((and (consp f) (case (car f) ((setq) t))) ; setf removed for now
	       (cl:multiple-value-bind (vars lastbind binds) (detangle2 (cdr f))
		 (unless (cdr lastbind)
		   (ers 50 "~%missing value in assignment: " f))
		 ;; setf still not supported - need to make caller setf-aware
		 (cl:let ((expr (cons 'setq ; should be setf
				      (mapcan #'xform-assignment vars binds))))
		   ;;(format t "~s" expr)
		   (push expr revbody))))
	      ;; need to first make caller psetf-aware probably
	      #+:ignore 
	      ((and (consp f) (case (car f) ((psetq psetf) t)))
	       (cl:multiple-value-bind (vars lastbind binds) (detangle2 (cdr f))
		 (unless (cdr lastbind)
		   (ers 50 "~%missing value in assignment: " f))
		 (push (cons 'psetf (mapcan #'xform-assignment vars binds))
		       revbody)))
	      ((eq-car f 'next-out)
	       (setq state (if (or (eq state :epilog) (eq f epilog-start))
			       :epilog
			     :middle))
	       (cl:let* ((ret (cdr (assoc (cadr f) series-output-alist)))
			 (-x- (new-var '-xxxx-)))
		 (setf (series-var-p ret) t)
		 (push `(setq ,(var ret) ,(caddr f)) revbody)
		 (when (not (eq state :epilog))
		   (setf (off-line-spot ret) -x-)
		   (push -x- revbody))))
	      (t (setq state :middle)
		 (push f revbody)))))
    (nreverse revbody)))

;; OPTIMIZER
(cl:defun optimize-producing (output-list input-list body)
  (cl:let ((series-ins (validate-producing output-list input-list body)))
    (cl:multiple-value-bind (bod type-alist propagations)
        (decode-dcls body '(types props))
      ;;#+:cmu
      #+nil
      (cl:flet ((fix-types (var)
		  (cons (car var) (make-nullable (cdr var)))))
	(declare (dynamic-extent #'fix-types))
        (setq type-alist (mapcar #'fix-types type-alist)))
      (cl:let* ((forms (cdadar bod))
                (frag (make-frag :impure t))
                (input-alist nil)
                (series-output-alist nil)
                (*renames* *renames*)
                (new-renames *renames*))
        (dolist (p (append (remove-if-not #'consp output-list) input-list))
          (setq p (normalize-pair p nil))
          (cl:let* ((value (retify (cadr p)
                                   (or (cdr (assoc (caar p) type-alist)) t)))
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
            (cl:let ((ret (make-sym :var (gensym (root p)) :series-var-p t))
                     (type (cdr (assoc p type-alist))))
              (cl:flet ((upgrade-type (typ)
                         (if (eq-car typ 'series)
			     (cadr typ)
			   typ)))
		(declare (dynamic-extent #'upgrade-type))
                (if (eq-car type 'series)
                    (setq type (cadr type)) 
                  (if (eq-car type 'or)
                      (setq type (cons 'or (mapcar #'upgrade-type (cdr type))))
                    (setq type t))))
              (+ret ret frag)
	      (add-aux frag (var ret) type)
              (push (cons p (var ret)) *renames*)
              (push (cons p ret) series-output-alist))))
        (cl:let* ((label-alist nil) (new-forms nil))
          (dolist (f forms)
            (cond ((not (symbolp f)) (push f new-forms))
                  (t (cl:let ((new (gensym (root f))))
                       (push (cons `(go ,f) `(go ,new)) label-alist)
                       (push new new-forms)))))
          (when label-alist
            (setq forms (sublis label-alist (nreverse new-forms) :test #'equal))))
        
        (cl:let ((*in-series-expr* nil)
		 (*not-straight-line-code* nil)
		 (body (basic-prod-form->frag-body
			forms input-alist series-output-alist)))
	  ;;(format t "~S" body)
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
		  (add-aux frag (car entry) t))
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
                   t))))
          ;bit of a cludge the way the following bashes things into the old style of stuff.
          (dolist (pair propagations)
            (cl:let ((input (cdr (assoc (car pair) input-alist)))
		     (output (cdr (assoc (cadr pair) series-output-alist))))
              (when (and input output)
                (setf (var output) (var input)))))
          (+frag frag))))))

;; API
(defS producing (output-list input-list &rest body)
  "(producing output-list input-list {declaration}* {form}*

PRODUCING computes and returns a group of series and non-series outputs
given a group of series and non-series inputs.

The OUTPUT-LIST has the same syntax as the binding list of a LET.  The
names of the variables must be distinct from each other and from the names
of the variables in the INPUT-LIST.  If there are N variables in the
OUTPUT-LIST, PRODUCING computes N outputs.  There must be at least one
output variable.  The variables act as the names for the outputs and can be
used in either of two ways.  First, if an output variable has a value
associated with it in the OUTPUT-LIST, then the variable is treated as
holding a non-series value.  The variable is initialized to the indicated
value and can be used in any way desired in the body. The eventual output
value is whatever value is in the variable when the execution of the body
terminates.  Second, if an output variable does not have a value associated
with it in the OUTPUT-LIST, the variable is given as its value a gatherer
that collects elements.  The only valid way to use the variable in the body
is in a call on NEXT-OUT.  The output returned is a series containing these
elements.  If the body never terminates, this series is unbounded.

The INPUT-LIST also has the same syntax as the binding list of a LET.
The names of the variables must be distinct from each other and the names
of the variables in the OUTPUT-LIST.  The values can be series or
non-series.  If the value is not explicitly specified, it defaults to NIL.
The variables act logically both as inputs and state variables and can be
used in one of two ways.  First, if an input variable is associated with a
non-series value, then it is given this value before the evaluation of the
body begins and can be used in any way desired in the body.   Second, if an
input variable is associated with a series, then the variable is given a
generator corresponding to this series as its initial value.  The only
valid way to use the variable in the body is in a call on NEXT-IN.
"
  non-opt-producing
 :optimizer
   (optimize-producing output-list input-list body)
 :trigger
   (dolist (pair (caddr call) nil)
     (when (and (consp pair) (cdr pair) (produces-optimizable-series (cadr pair)))
       (return t)))
 :discriminator
   (dolist (pair (cadr call) nil)
     (when (not (consp pair))
       (return t))))

#+symbolics
(setf (gethash 'producing zwei:*lisp-indentation-offset-hash-table*)
      '(2 1))

(defmacro terminate-producing ()
    "Causes the containing call on producing to terminate."
  `(go ,end))


;; This turns a producing form into a frag that fits the requirements
;; of a frag well enough that we can call FRAG->PHYSICAL on it.  The
;; key to this is that we only keep the series inputs and outputs, and
;; we know exactly where they can be used.

(defmacro non-opt-producing (output-list input-list &body body)
  (cl:let ((series-inputs (validate-producing output-list input-list body)))
    (cl:multiple-value-bind (bod props) (decode-dcls body '(props no-complaints))
      (setq input-list
            (mapcar #'(lambda (p)
			(if (consp p)
			    p
			  (list p nil)))
		    input-list))
      (cl:let* ((forms (cdadar bod))
		(frag (make-frag :code `(producing ,output-list ,input-list
			                  ,@ body)
				 :impure t))
		(input-alist nil)
		(series-output-alist nil))
        (dolist (in series-inputs)
          (cl:let* ((v (new-var 'inpt))
		    (arg (make-sym :var v :series-var-p t)))
            (+arg arg frag)
            (push (cons in arg) input-alist)))
        (dolist (out output-list)
          (cl:let* ((prop-input (dolist (p props nil)
				  (when (eq (cadr p) out)
				    (return (car p)))))
		    (v (cond (prop-input
			      (var (cdr (assoc prop-input input-alist))))
			     ((consp out) (car out))
			     (t (new-var 'out))))
                      (ret (make-sym :var v :series-var-p (not (consp out)))))
            (+ret ret frag)
            (unless prop-input
	      (add-aux frag v t))
            (if (not (consp out))
		(push (cons out ret) series-output-alist)
              (add-prolog frag `(setq ,(car out) ,(cadr out))))))
        (setf (body frag)
              (basic-prod-form->frag-body forms input-alist series-output-alist))
        `(cl:let ,input-list
           ,(frag->physical frag series-inputs (some #'consp output-list)))))))

;; The alter form found probably refers to vars which are not OLD
;; itself. For this to be OK, we must be sure that these variables
;; must never be renamed.  To ensure that, we must ensure that none of
;; these variables are ever inputs.  (Aux variables are not renamed
;; and return variables are not renamed as long as they are not also
;; inputs.)  This requires care on the part of all standard functions
;; that are alterable (i.e. scan) and particularly in to-alter which
;; would be much easier to write if it just passed the inputs through.
;;
;; This is ok because outputs never get renamed.  Also the input old
;; to the frag most likely never gets used, but this makes sure that
;; the dflow is logically correct.

(cl:defun find-alter-form (ret)
  (cl:let* ((v (var ret))
            (form (cadr (assoc v (alterable (fr ret))))))
    (if form
	form
      (dolist (a (args (fr ret)))
	(when (or (eq v (var a))
		  (equal (prolog (fr ret)) (makeprolog `((setq ,v ,(var a))))))
	  (return (find-alter-form (prv a))))))))

;; I'm leaving the above as a reference.  It turns out that in some
;; cases such as the following:
;;    (defstruct vec x y z)
;;    (defun scan-vec (vec)
;;      (declare (optimizable-series-function))
;;      (to-alter (make-series (vec-x vec) (vec-y vec) (vec-z vec))
;; 		#'(lambda (new-value index v)
;; 		    (ecase index
;; 		      (0 (setf (vec-x v) new-value))
;; 		      (1 (setf (vec-y v) new-value))
;; 		      (2 (setf (vec-z v) new-value))))
;; 		(scan-range :from 0)
;; 		(make-series vec vec vec)))
;;     (let ((vec (make-vec :x 1 :y 2 :z 3)))
;;       (alter (scan-vec vec) (series 0))
;;       vec)
;;
;; that there are several forms in the alterable slot of the frag.  So
;; we remove all forms that aren't for the current var and return all
;; forms so ALTER can drop them into the code.
;;
;; I'm not really sure this is all right, but we definitely need the
;; more than just the first match, as FIND-ALTER-FORM does.
(cl:defun find-alter-forms (ret)
  (cl:let* ((v (var ret))
	    (forms (remove-if-not #'(lambda (item)
				      (eql v (car item)))
				  (alterable (fr ret)))))
    (if forms
	forms
	(dolist (a (args (fr ret)))
	  (when (or (eq v (var a))
		    (equal (prolog (fr ret)) (makeprolog `((setq ,v ,(var a))))))
	    (return (find-alter-forms (prv a))))))))

;; API
#+nil
(defS alter (destinations items)
  "Alters the values in DESTINATIONS to be ITEMS."
  (fragl ((destinations) (items t)) ((result))
         ((gen generator (generator destinations))
          (result null nil))
	 ()
         ()
         ((do-next-in gen #'(lambda () (go end)) items))
	 ()
	 ()
	 nil ; series dataflow constraint takes care
	 )
  :optimizer
  (cl:let ((ret (retify destinations)))
    (when (not (series-var-p ret))
      (rrs 5 "~%Alter applied to a series that is not known at compile time:~%"
           *call*))
    (format t "All alter forms:~%")
    (write (find-alter-forms ret) :circle t)
    (cl:let ((form (find-alter-form ret))
             (frag (literal-frag '(((old t) (items t)) ((result)) ((result null))
                                   ()
				   ((setq result nil))
				   ()
				   ()
				   ()
				   nil ; series dataflow constraint takes care
				   ))))
      (unless form
        (ers 65 "~%Alter applied to an unalterable series:~%" *call*)) 
      (setf (body frag) (list (subst (var (cadr (args frag))) '*alt* form)))
      (apply-frag frag (list ret items))))
  :trigger t)

;; The old version above is left for a reference.
(defS alter (destinations items)
  "Alters the values in DESTINATIONS to be ITEMS."
  (fragl ((destinations) (items t)) ((result))
         ((gen generator (generator destinations))
          (result null nil))
	 ()
         ()
         ((do-next-in gen #'(lambda () (go end)) items))
	 ()
	 ()
	 nil ; series dataflow constraint takes care
	 )
  :optimizer
  (cl:let ((ret (retify destinations)))
    (when (not (series-var-p ret))
      (rrs 5 "~%Alter applied to a series that is not known at compile time:~%"
           *call*))
    (cl:let ((all-forms (find-alter-forms ret))
	     (frag (literal-frag '(((old t) (items t)) ((result)) ((result null))
				   ()
				   ((setq result nil))
				   ()
				   ()
				   ()
				   nil ; series dataflow constraint takes care
				   ))))
      (unless all-forms
	(ers 65 "~%Alter applied to an unalterable series:~%" *call*))
      ;; Look through all forms that match and do the appropriate
      ;; thing so that the body has all alterable forms we need.  See
      ;; comments for FIND-ALTER-FORMS.
      (setf (body frag) (mapcar #'(lambda (f)
				    (subst (var (cadr (args frag))) '*alt* (cadr f)))
				all-forms))
      (apply-frag frag (list ret items))))
  :trigger t)

;; API
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
	    (input-vars (n-gensyms (length other-inputs) (symbol-name '#:state-in-)))
	    (state-vars (n-gensyms (length other-inputs) (symbol-name '#:state-)))
	    (var (new-var 'items)))
    (+ret (make-sym :var var :series-var-p t) frag)
    (+arg (make-sym :var var :series-var-p t) frag)
    (multiple-value-setq (alter-function params)
                         (handle-fn-arg frag alter-function params))
    (mapc #'(lambda (in-var state-var)
              (+arg (make-sym :var in-var :series-var-p t) frag)
	      (add-aux frag state-var t)
              (push `(setq ,state-var ,in-var) (body frag)))
          input-vars state-vars)
    (setq params (append params other-inputs))
    (setf (alterable frag)
          `((,var (cl:funcall ,alter-function *alt* ,@ state-vars) ,@ state-vars)))
    (apply-frag frag params)))

;; API
(defS series (expr &rest expr-list)
  "(series arg &rest args)

Creates an infinite series that endlessly repeats the given items in
the order given."
  (cond ((null expr-list)
         (fragl ((expr)) ((expr t)) () () () () () () :args))
        (t (cl:let ((full-expr-list
                        (optif `(,expr ,@ expr-list)
				(cons expr (copy-list expr-list)))))
             (fragl ((full-expr-list)) ((items t))
		    ((items t)
		     (lst list (copy-list full-expr-list)))
		    ()
                    ((setq lst (nconc lst lst)))
                    ((setq items (car lst)) (setq lst (cdr lst)))
		    ()
		    ()
		    :args
		    ))))
  :optimizer
  ;; This is essentially identical to the above code except that
  ;; FULL-EXPR-LIST is (LIST <items>).  Not 100% sure about this, but
  ;; unexpected things happen if we don't.
  ;;
  ;; Some examples (noted on comp.lang.lisp by Szymon 'tichy' on Sat,
  ;; Jul 7, 2007):
  ;; (positions (series t nil)) -> #z(0 1 2 3 4 6 7 10 12 13 ...)
  ;; (series t nil) -> #z(list t nil list t nil ...)
  ;;
  ;; I believe these are wrong.  (How can I add such tests to the test
  ;; scripts?)  The first test should return #z(0 2 4 6 8 10 ...) and
  ;; the second should return #z(t nil t nil ...)
  ;;
  (cond ((null expr-list)
         (fragl ((expr)) ((expr t)) () () () () () () :args))
        (t (cl:let ((full-expr-list
                        (optif `(list ,expr ,@ expr-list)
				(cons expr (copy-list expr-list)))))
             (fragl ((full-expr-list)) ((items t))
		    ((items t)
		     (lst list (copy-list full-expr-list)))
		    ()
                    ((setq lst (nconc lst lst)))
                    ((setq items (car lst)) (setq lst (cdr lst)))
		    ()
		    ()
		    :args
		    )))))

;; API
(defS literal-series (seq) ""
  (make-phys :data-list seq)
  :optimizer
  (+frag
   (cl:let ((frag (literal-frag
                      '(() ((elements t)) ((listptr list) (elements t)) ()
                        () ((if (endp listptr) (go end))
                            (setq elements (car listptr))
                            (setq listptr (cdr listptr)))
			()
			()
			nil)))) ; pure because seq is literal
     (add-prolog frag `(setq ,(car (first-aux (aux frag))) ,seq))
     frag)))

;; put on #Z
#+nil
(cl:defun series-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(literal-series ',(read stream t nil t)))

(cl:defun series-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (cl:let ((items (read stream t nil t)))
    (cl:unless (list-length items)
      ;; We have some kind of infinite list.
      (ers 100 "~&Infinite literal series not supported"))
    `(literal-series ',items)))

;; API
(defS make-series (item &rest item-list)
    "Creates a series of the items."
  (make-phys :data-list (cons item (copy-list item-list)))
  :optimizer
    (macroexpand `(scan (list ,item ,@ (copy-list item-list)))))


#-:old-scan-range
(progn
;; This deftype is used to appease the compiler (CMUCL) about an
;; unknown type when compiling scan-range.  If someone has a better
;; way of getting scan-range to generate the correct initial values
;; without this hack, I'd love to have it.
(deftype *type* () t)

;; API
;; This version of scan-range fixes a long-standing bug that occurs
;; when :type is not 'number.  Say :type is 'single-float.  Then the
;; looping variables are declared to be single-float's, but the
;; variables end up being initialized with fixnums.  This loses.  To
;; fix this, the initializers are coerced to the correct type.
;; However, you need a reasonably smart compiler to do constant
;; folding so that you don't do the coercion every time through the
;; loop.
(defS scan-range (&key (from 0) (by 1)
                       (upto nil) (below nil)
                       (downto nil) (above nil)
                       (length nil) (type 'number))
    "(scan-range &key (:start 0) (:by 1) (:type 'number) :upto :below
                      :downto :above :length)

The function SCAN-RANGE returns a series of numbers starting with the
:start argument (default integer 0) and counting up by the :by
argument (default integer 1). The :type argument (default number) is a
type specifier indicating the type of numbers in the series
produced. The :type argument must be a (not necessarily proper)
subtype of number. The :start and :by arguments must be of that
type.

One of the last five arguments may be used to specify the kind of end
test to be used; these are called termination arguments. If :upto is
specified, counting continues only so long as the numbers generated
are less than or equal to :upto. If :below is specified, counting
continues only so long as the numbers generated are less than
:below. If :downto is specified, counting continues only so long as
the numbers generated are greater than or equal to :downto. If :above
is specified, counting continues only so long as the numbers generated
are greater than :above. If :length is specified, it must be a
non-negative integer and the output series has this length.

If none of the termination arguments are specified, the output has
unbounded length. If more than one termination argument is specified,
it is an error.  "
  (cl:let ((*type* (optif (if (eq-car type 'quote)
			      (cadr type)
			    'number)
			  type)))
    (when (> (length (delete nil (list upto below downto above length))) 1)
      (ers 77 "~%Too many keywords specified in a call on SCAN-RANGE."))
    (cond (upto
           (*fragl ((from) (upto) (by)) ((numbers t))
		   ((numbers *type* (coerce-maybe-fold (- from by) '*type*)))
		   ()
		   ()
		   ((setq numbers (+ numbers (coerce-maybe-fold by '*type*)))
		    (if (> numbers upto) (go end)))
		   ()
		   ()
		   :args))
          (below
           (*fragl ((from) (below) (by)) ((numbers t))
		   ((numbers *type* (coerce-maybe-fold (- from by) '*type*)))
		   ()
		   ()
		   ((setq numbers (+ numbers (coerce-maybe-fold by '*type*)))
		    (if (not (< numbers below)) (go end)))
		   ()
		   ()
		   :args))
          (downto
           (*fragl ((from) (downto) (by)) ((numbers t))
		   ((numbers *type* (coerce-maybe-fold (- from by) '*type*)))
		   ()
		   ()
		   ((setq numbers (+ numbers (coerce-maybe-fold by '*type*)))
		    (if (< numbers downto) (go end)))
		   ()
		   ()
		   :args))
          (above
           (*fragl ((from) (above) (by)) ((numbers t))
		   ((numbers *type* (coerce-maybe-fold (- from by) '*type*)))
		   ()
		   ()
		   ((setq numbers (+ numbers (coerce-maybe-fold by '*type*)))
		    (if (not (> numbers above)) (go end)))
		   ()
		   ()
		   :args))
          (length
           (*fragl ((from) (length) (by)) ((numbers t))
		   ((numbers *type* (coerce-maybe-fold (- from by) '*type*))
		    (counter fixnum length))
		   ()
		   ()
		   ((setq numbers (+ numbers (coerce-maybe-fold by '*type*)))
		    (if (not (plusp counter)) (go end))
		    (decf counter))
		   ()
		   ()
		   :args))
          (t (*fragl ((from) (by)) ((numbers t))
		     ((numbers *type* (coerce-maybe-fold (- from by) '*type*)))
		     ()
		     ()
		     ((setq numbers (+ numbers (coerce-maybe-fold by '*type*))))
		     ()
		     ()
		     :args)))))
)

;; API
#+:old-scan-range
(defS scan-range (&key (from 0) (by 1)
                       (upto nil) (below nil)
                       (downto nil) (above nil)
                       (length nil) (type 'number))
    "Creates a series of numbers by counting from :FROM by :BY."
  (cl:let ((*type* (optif (if (eq-car type 'quote)
			      (cadr type)
			    'number)
			  type)))
    (when (> (length (delete nil (list upto below downto above length))) 1)
      (ers 77 "~%Too many keywords specified in a call on SCAN-RANGE."))
    (cond (upto
           (*fragl ((from) (upto) (by)) ((numbers t))
		   ((numbers *type* (- from by)))
		   ()
		   ()
		   ((setq numbers (+ numbers by))
		    (if (> numbers upto) (go end)))
		   ()
		   ()
		   :args))
          (below
           (*fragl ((from) (below) (by)) ((numbers t))
		   ((numbers *type* (- from by)))
		   ()
		   ()
		   ((setq numbers (+ numbers by))
		    (if (not (< numbers below)) (go end)))
		   ()
		   ()
		   :args))
          (downto
           (*fragl ((from) (downto) (by)) ((numbers t))
		   ((numbers *type* (- from by)))
		   ()
		   ()
		   ((setq numbers (+ numbers by))
		    (if (< numbers downto) (go end)))
		   ()
		   ()
		   :args))
          (above
           (*fragl ((from) (above) (by)) ((numbers t))
		   ((numbers *type* (- from by)))
		   ()
		   ()
		   ((setq numbers (+ numbers by))
		    (if (not (> numbers above)) (go end)))
		   ()
		   ()
		   :args))
          (length
           (*fragl ((from) (length) (by)) ((numbers t))
		   ((numbers *type* (- from by))
		    (counter fixnum length))
		   ()
		   ()
		   ((setq numbers (+ numbers by))
		    (if (not (plusp counter)) (go end))
		    (decf counter))
		   ()
		   ()
		   :args))
          (t (*fragl ((from) (by)) ((numbers t))
		     ((numbers *type* (- from by)))
		     ()
		     ()
		     ((setq numbers (+ numbers by)))
		     ()
		     ()
		     :args)))))

(defmacro null-scan (type)
  `(efragl ()
	   `(((elements t))
	     ((elements ,,type))
	     ()
	     ()
	     ((go end))
	     ()
	     ()
	     nil)))

(defmacro limited-scan (type unopt-type-limit opt-type-limit unopt-limit &optional opt-limit)
  `(eoptif-q
      (*fragl ((seq) ,@(when opt-limit `((,opt-limit))))
	      ((elements t))
	       ((elements ,type)
		(temp t seq) 
		(index (integer -1 ,@(when opt-type-limit `(,opt-type-limit))) -1))
	       ((elements (setf (row-major-aref temp (the (integer- 0
								    ,@(when opt-type-limit `(,opt-type-limit)))
						       index)) *alt*) temp index))
	       ()
	       ((incf index)
		(locally
		 (declare (type (integer 0 ,opt-type-limit) index))
		 (if (= index ,(or opt-limit opt-type-limit)) (go end))
		 (setq elements (row-major-aref seq (the (integer- 0
								   ,@(when opt-type-limit `(,opt-type-limit)))
						      index)))))
	       ()
	       ()
	       :mutable)
    (*fragl ((seq) (,unopt-limit)) ((elements t))
	     ((elements ,type)
	      (temp t seq) 
	      (index (integer -1 ,@(when unopt-type-limit `(,unopt-type-limit))) -1))
	     ((elements (setf (row-major-aref temp (the (integer- 0
								  ,@(when unopt-type-limit `(,unopt-type-limit)))
						     index)) *alt*) temp index))
	     ()
	     ((incf index)
	      (locally
	       (declare (type (integer 0 ,@(when unopt-type-limit `(,unopt-type-limit))) index))
	       (if (= index ,unopt-limit) (go end))
	       (setq elements (row-major-aref seq (the (integer- 0
								 ,@(when unopt-type-limit `(,unopt-type-limit)))
						    index)))))
	     ()
	     ()
	     :mutable)))


(defmacro list-scan (type)
  `(efragl ((seq))
	   `(((elements t))
	     ((elements ,,type)
	      (listptr list seq)
	      (parent list))
	     ((elements (setf (car parent) *alt*) parent))
	     ()
	     ((if (endp listptr) (go end))
	      (setq parent listptr)
	      (setq elements (car listptr))
	      (setq listptr (cdr listptr)))
	     ()
	     ()
	     :mutable)))

;; API
(defS scan (seq-type &optional (seq nil seq-p))
    "(scan [type] sequence)

SCAN returns a series containing the elements of sequence in
order. The type argument is a type specifier indicating the type of
sequence to be scanned; it must be a (not necessarily proper) subtype
of sequence. If type is omitted, it defaults to list."
  (cl:let (type limit *limit* *type*)
    (when (not seq-p) ;it is actually seq-type that is optional
      (setq seq seq-type)
      (setq seq-type #-:cltl2-series nil #+:cltl2-series (unless (constantp seq) (optq 'list))))
    (multiple-value-setq (type limit *type*) (decode-seq-type (non-optq seq-type)))
    (cond ((member type '(list bag))
	   (if (null seq)
	       (null-scan (eoptif-q *type* t))
	     (list-scan (eoptif-q *type* t))))
          (limit
	   (setq *limit* limit)
	   (if (= *limit* 0)
	       (if (constantp seq)
		   (null-scan (eoptif *type* t))
		 (efragl ((seq))
			 `(((elements t))
			   ((elements ,(eoptif-q *type* t))
			    (temp t seq))
			   ()
			   ()
			   ((go end))
			   ()
			   ()
			   :args)))
	     (limited-scan *type* nil *limit* limit)))
          ((not (eq type 'sequence))	;some kind of array
	   ;; Check to see if SEQ is constant.
	   (if (constantp seq)
	       (cl:let ((thing (if (symbolp seq)
				   (symbol-value seq)
				   seq)))
		 ;; THING should be either the SEQ or if SEQ is a
		 ;; symbol, the value of the symbol.
		 (when (and (consp seq) (eq (car seq) 'quote))
		   (setq thing (cadr seq)))
		 (setq limit (array-total-size thing))
		 (when (eq *type* t)
		   (setq *type* (array-element-type thing)))		 
		 (if (= limit 0)
		     (null-scan (eoptif-q *type* t))
		   (progn
		     (setq *limit* limit)
		     (limited-scan *type* array-total-size-limit *limit* limit))))
	     (efragl ((seq))
		     `(((elements t))
		       ((elements ,(eoptif-q *type* t))
			(temp t seq)
			(limit vector-index+ (array-total-size seq))
			(index -vector-index+ -1))
		       ((elements (setf (row-major-aref temp (the vector-index index)) *alt*) temp index))
		       ()
		       ((incf index)
			(locally
			 (declare (type vector-index+ index))
			 (if (= index limit) (go end))
			 (setq elements (row-major-aref seq (the vector-index index)))))
		       ()
		       ()
		       :mutable))))
          (t
	   (if (constantp seq)
	       (if (null seq)
		   (null-scan (eoptif-q *type* t))
		 (cl:let ((thing seq))   
		   (when (eq-car seq 'quote)
		     (setq thing (cadr seq)))
		   (when (and (eq *type* t) (not (listp thing)))
		     (setq *type* (array-element-type thing)))
		   (setq limit
			 (if (listp thing)
			     (length thing)
			   (array-total-size thing)))
		   (if (= limit 0)
		       (null-scan (eoptif-q *type* t))
		     (if (consp thing)
			 (list-scan (eoptif-q *type* t))
		       (progn
			 (setq *limit* limit)
			 (limited-scan *type* array-total-size-limit *limit* limit))))))
	     #-:cltl2-series
	     (if (lister-p seq)
		 (list-scan (if *optimize-series-expressions* *type* t))
	       (efragl ((seq-type) (seq)) ;dummy type input avoids warn
		       `(((elements t))
		         ((elements ,(eoptif-q *type* t))
			  (parent list)
			  (listptr list)
			  (temp array)
			  (limit vector-index+)
			  (index -vector-index -1)
			  (lstp boolean))
			 ((elements (if lstp
					(setf (car parent) *alt*)
				      (setf (row-major-aref temp (the vector-index index)) *alt*))
				    parent temp index lstp))
			 ((if (setq lstp (listp seq))
			      (setq listptr seq
				    temp #())
			    (locally
			     (declare (type array seq))
			     (setq temp seq)
			     (setq limit (array-total-size seq)))))
			 ((if lstp
			      (progn
				(if (endp listptr) (go end))
				(setq parent listptr)
				(setq elements (car listptr))
				(setq listptr (cdr listptr)))
			    (progn
			      (incf index)
			      (locally			  
			       (declare (type array seq) (type vector-index index))
			       (if (>= index limit) (go end))
			       (setq elements (the *type* (row-major-aref seq index)))))))
			 ()
			 ()
			 :mutable)))
	     #+:cltl2-series
	     (efragl ((seq-type) (seq)) ;dummy type input avoids warn
		     `(((elements t)) 
		       ((elements ,(eoptif-q *type* t))
			(temp sequence seq)
			(limit nonnegative-integer (length seq))
			(index (integer -1) -1))
		       ((elements (setf (elt temp (the nonnegative-integer index)) *alt*)
				  temp index))
		       ()
		       ((incf index)
			(locally
			 (declare (type nonnegative-integer index))
			 (if (>= index limit) (go end))
			 (setq elements (elt seq index))))
		       ()
		       ()
		       :mutable))
	     )))))

;; HELPER
(cl:defun promote-series (series)
  (cond ((not (alter-fn series))
	 (values series nil))
        (t (setq series (if (image-series-p series)
                            (copy-image-series series)
			  (copy-basic-series series)))
           (setf (alter-fn series) nil)
           (values series t))))

;; API
(defS cotruncate (&rest items-list)
  "(cotruncate &rest series)

Truncates the inputs so that they are all no longer than the shortest one."
  (values-lists (length items-list)
                (apply #'map-fn t #'list (mapcar #'promote-series items-list))
                (mapcar #'alter-fn items-list))
 :optimizer
  (cl:let* ((args (copy-list items-list))
	    (vars (n-gensyms (length args) (symbol-name '#:cotrunc-)))
	    (ports (mapcar #'(lambda (v) (list v t)) vars)))
    (apply-frag
      (literal-frag `(,ports ,(copy-list ports) nil nil nil nil nil nil nil))
      args)))

;; API
(defS scan* (seq-type seq)
    "Enumerates a series of the values in SEQ without checking for the end."
  (cl:multiple-value-bind (type limit *type*)
      (decode-seq-type (non-optq seq-type))
      (declare (ignore limit))
    (cond ((member type '(list bag))
           (efragl ((seq))
		   `(((elements t))
		     ((elements ,(eoptif-q *type* t))
		      (listptr list seq)
		      (parent list))
		     ((elements (setf (car parent) *alt*) parent))
		     ()
		     ((setq parent listptr)
		      (setq elements (car listptr))
		      (setq listptr (cdr listptr)))
		     ()
		     ()
		     :mutable)))
          ((not (eq type 'sequence))	;some kind of array
           (efragl ((seq))
		   `(((elements t))
		     ((elements ,(eoptif-q *type* t))
		      (temp array seq)
		      (index -vector-index -1))
		     ((elements (setf (row-major-aref temp (the vector-index index)) *alt*) temp index))
		     ()
		     ((incf index)
		      (setq elements
			    (row-major-aref seq (the vector-index index))))
		     ()
		     ()
		     :mutable)))
          (t (efragl ((seq-type) (seq)) ;dummy type input avoids warn
		     `(((elements t)) 
		       ((elements ,(eoptif-q *type* t))
			(temp t seq)
			(index nonnegative-integer 0))
		       ((elements (setf (elt temp index) *alt*) temp index))
		       ()
		       ((setq elements (elt seq index))
			(incf index))
		       ()
		       ()
		       :mutable))))))

;; HELPER
(cl:defun decode-multiple-types-arg (type n)
  (cond ((or (not (eq-car type 'quote))
             (not (eq-car (cadr type) 'values)))
         (make-list n :initial-element type))
        (t (when (not (= (length (cdadr type)) n))
	     (ers 78 "~%SCAN-MULTIPLE: type and number of sequences conflict."))
	   (mapcar #'(lambda (x) `(quote ,x)) (cdadr type)))))

;; API
(defS scan-multiple (type sequence &rest sequences)
  "(scan-multiple type first-seq &rest more-sequences)

Like several calls to SCAN, but scans multiple sequences in parallel
efficiently."
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

;; API
(defS scan-sublists (lst)
  "(scan-sublists list)

Creates a series of the sublists in a list.

  (SCAN-SUBLISTS '(A B C)) => #Z((A B C) (B C) (C)) "
  (fragl ((lst)) ((sublists t))
	 ((sublists list)
	  (lstptr list lst))
	 ()
         ()
         ((if (endp lstptr) (go end))
          (setq sublists lstptr)
          (setq lstptr (cdr lstptr)))
	 ()
	 ()
	 :mutable))

;; API
(defS scan-alist (alist &optional (test #'eql))
  "(scan-alist alist &optional (test #'eql))

Creates two series containing the keys and values in an alist."
  (fragl ((alist) (test)) ((keys t) (values t))
         ((alistptr list alist)
	  (keys t) (values t) (parent list))
         ((keys (setf (car parent) *alt*) parent)
          (values (setf (cdr parent) *alt*) parent))
         ()
         (L (if (null alistptr) (go end))
            (setq parent (car alistptr))
            (setq alistptr (cdr alistptr))
            (if (or (null parent)
                    (not (eq parent (assoc (car parent) alist :test test))))
                (go L))
            (setq keys (car parent))
            (setq values (cdr parent)))
	 ()
	 ()
	 :mutable))

;; API
(defS scan-plist (plist)
  "(scan-plist plist)

Creates two series containing the indicators and values in a plist."
  (fragl ((plist)) ((indicators t) (values t))
         ((indicators t) (values t)
	  (plistptr list plist)
	  (parent list))
         ((indicators (setf (car parent) *alt*) parent)
          (values (setf (cadr parent) *alt*) parent))
         ()
         (L (if (null plistptr) (go end))
            (setq parent plistptr)
            (setq indicators (car plistptr))
            (setq plistptr (cdr plistptr))
            (setq values (car plistptr))
            (setq plistptr (cdr plistptr))
            (do ((ptr plist (cddr ptr)))
                ((eq (car ptr) indicators)
                 (if (not (eq ptr parent)) (go L)))))
	 ()
	 ()
	 :mutable))

;; API
(defS scan-lists-of-lists (tree &optional (test #'atom test-p))
  "(scan-lists-of-lists lists-of-lists &optional (leaf-test 'atom))

Creates a series of the nodes in a tree, in preorder order.  LEAF-TEST
returns non-NIL if a node is a leaf node."
  (if test-p
      (fragl ((tree) (test)) ((nodes t))
	     ((nodes t)
	      (state list (list tree)))
	     ()
             ()
             ((if (null state) (go end))
              (setq nodes (car state))
              (setq state (cdr state))
              (when (not (or (atom nodes) (cl:funcall test nodes)))
                (do ((ns nodes (cdr ns))
                     (r nil (cons (car ns) r)))
                    ((not (consp ns))
                     (setq state (nreconc r state))))))
	     ()
	     ()
	     :mutable)
    (fragl ((tree)) ((nodes t))
	   ((nodes t)
	    (state list (list tree)))
	   ()
	   ()
	   ((if (null state) (go end))
	    (setq nodes (car state))
	    (setq state (cdr state))
	    (when (not (atom nodes))
	      (do ((ns nodes (cdr ns))
		   (r nil (cons (car ns) r)))
		  ((not (consp ns))
		   (setq state (nreconc r state))))))
	   ()
	   ()
	   :mutable)))

;; API
(defS scan-lists-of-lists-fringe (tree &optional (test #'atom test-p))
  "(scan-lists-of-lists-fringe lists-of-lists &optional (leaf-test 'atom)

Creates a series of the leaves of a tree, in preorder order.  LEAF-TEST
returns non-NIL if a node is a leaf node."
  (if test-p
      (fragl ((tree) (test)) ((leaves t))
             ((leaves t) (parent list)
	      (state list (list (list tree))))
             ((leaves (setf (car parent) *alt*) parent))
             ()
             (L (if (null state) (go end))
                (setq leaves (car state))
                (setq state (cdr state))
                (setq parent leaves)
                (setq leaves (car leaves))
                (when (not (or (atom leaves) (cl:funcall test leaves)))
                  (do ((ns leaves (cdr ns))
                       (r nil (cons ns r)))
                      ((not (consp ns)) (setq state (nreconc r state))))
                  (go L)))
	     ()
	     ()
	     :mutable)
    (fragl ((tree)) ((leaves t))
	   ((leaves t) (parent list)
	    (state list (list (list tree))))
	   ((leaves (setf (car parent) *alt*) parent))
	   ()
	   (L (if (null state) (go end))
	      (setq leaves (car state))
	      (setq state (cdr state))
	      (setq parent leaves)
	      (setq leaves (car leaves))
	      (when (not (atom leaves))
		(do ((ns leaves (cdr ns))
		     (r nil (cons ns r)))
		    ((not (consp ns)) (setq state (nreconc r state))))
		(go L)))
	   ()
	   ()
	   :mutable)))

;; API
#-symbolics
(defS scan-symbols (&optional (package nil))
  "(scan-symbols (&optional (package *package*))

Creates a series of the symbols in PACKAGE (which defaults to *PACKAGE*)."
  (fragl ((package)) ((symbols t))
	 ((symbols symbol)
	  (lst list nil))
	 ()
         ((do-symbols (s (or package *package*)) (push s lst)))
         ((if (null lst) (go end))
          (setq symbols (car lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :context)) ; package can change -
                    ; Movement should only be allowed if no unknown functions
                    ; and constrained by thread sync and package operations

;; API
#+symbolics ;see do-symbols
(defS scan-symbols (&optional (package nil))
    "Creates a series of the symbols in PACKAGE."
  (fragl ((package))
	 ((symbols t))
	 ((index t) (state t) (symbols symbol))
	 ()
         ((multiple-value-setq (index symbols state)
            (si:loop-initialize-mapatoms-state (or package *package*) nil)))
         ((if (multiple-value-setq (nil index symbols state)
                (si:loop-test-and-step-mapatoms index symbols state))
              (go end)))
	 ()
	 ()
	 :context)) ; package can change
                    ; Movement should only be allowed if no unknown functions
                    ; and constrained by thread sync and package operations

;; API
(defS scan-file (name &optional (reader #'read))
    "(scan-file file-name &optional (reader #'read)

SCAN-FILE opens the file named by the string FILE-NAME and applies the
function READER to it repeatedly until the end of the file is
reached. READER must accept the standard input function arguments
input-stream, eof-error-p, and eof-value as its arguments. (For
instance, reader can be read, read-preserving-white-space, read-line,
or read-char.) If omitted, READER defaults to READ. SCAN-FILE returns
a series of the values returned by READER, up to but not including the
value returned when the end of the file is reached. The file is
correctly closed, even if an abort occurs. "
  (fragl ((name) (reader)) ((items t))
	 ((items t)
	  (lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons)
	  (with-open-file (f name :direction :input)
            (cl:let ((done (list nil)))
              (loop              
                (cl:let ((item (cl:funcall reader f nil done)))
                  (when (eq item done)
                    (return nil))
		  (setq lastcons (setf (cdr lastcons) (cons item nil)))))))
	  (setq lst (cdr lst)))
         ((if (null lst) (go end))
          (setq items (car lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :context) ; file can change
                   ; Movement should only be allowed if no unknown functions
                   ; and constrained by sync and file operations
 :optimizer
  (apply-literal-frag
    (cl:let ((file (new-var 'file)))
      `((((reader)) ((items t))
	 ((items t) (done t (list nil)))
	 ()
         ()
         ((if (eq (setq items (cl:funcall reader ,file nil done)) done)
              (go end)))
	 ()
         ((#'(lambda (code)
              (list 'with-open-file
                    '(,file ,name :direction :input)
                    code)) :loop))
	 :context)
	,reader))))

;; API
(defS scan-stream (name &optional (reader #'read))
  "(scan-stream stream &optional (reader #'read))

Creates a series of the forms in the stream STREAM.  Similar to
SCAN-FILE, except we read from an existing stream."
  (fragl ((name) (reader)) ((items t))
	 ((items t)
	  (lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons)
	  (cl:let ((done (list nil)))
            (loop                
                (cl:let ((item (cl:funcall reader name nil done)))
		  (when (eq item done)
                    (return nil))
		  (setq lastcons (setf (cdr lastcons) (cons item nil))))))
	  (setq lst (cdr lst)))
         ((if (null lst) (go end))
          (setq items (car lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :mutable ; stream can change - OK if scan-private stream
	 )
 :optimizer
  (apply-literal-frag
   `((((reader)) ((items t))
      ((items t) (done t (list nil)))
      ()
      ()
      ((if (eq (setq items (cl:funcall reader ,name nil done)) done)
           (go end)))
      ()
      ()
      :mutable ; stream can change - OK if scan-private stream
      )
     ,reader
     )))

;; API
(defS scan-hash (table)
    "(scan-hash table)

Scans the entries of teh hash table and returns two series containing
the keys and their associated values.  The first element of key series
is the key of the first entry in the hash table, and the first element
of the values series is the value of the first entry, and so on.  The
order of scanning the hash table is not specified."

  #-CLISP
  (fragl ((table))
	 ((keys t) (values t))
	 ((keys t) (values t)
	  (lst list nil))
	 ()
         ((maphash #'(lambda (key val) (push (cons key val) lst)) table))
         ((if (null lst) (go end))
          (setq keys (caar lst))
          (setq values (cdar lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :mutable ; table can change - OK if scan-private table
	 )
  #+CLISP
  (fragl ((table))
	 ((keys t) (values t))
	 ((state t (sys::hash-table-iterator table))
	  (nextp t) (keys t) (values t))
	 ()
         ()
         ((multiple-value-setq (nextp keys values) (sys::hash-table-iterate state))
          (unless nextp (go end)))
         ()
	 ()
	 :mutable ; table can change - OK if scan-private table
	 )
#+symbolics :optimizer #+symbolics
  (apply-literal-frag
    `((((table))
       ((keys t) (values t))
       ((state t nil) (keys t) (values t))
       ()
       ()
       ((if (not (multiple-value-setq (state keys values)
                   (si:send table :next-element state)))
            (go end))) ()
       ((#'(lambda (c) `(si:inhibit-gc-flips ,c)) :loop))
       :mutable ; table can change - OK if scan-private table
       )
      ,table))
  )

;; API
(defS previous (items &optional (default nil) (amount 1))
  "(previous items &optional (default nil) (amount 1))

The series returned by PREVIOUS is the same as the input series ITEMS
except that it is shifted to the right by the positive integer AMOUNT.  The
shifting is done by inserting AMOUNT copies of DEFAULT before ITEMS and
discarding AMOUNT elements from the end of ITEMS."
  (cond ((eql amount 1)
         (fragl ((items t) (default)) ((shifted-items t))
                ((shifted-items (series-element-type items))
                 (state (series-element-type items) default))
		()
                ()
                ((setq shifted-items state) (setq state items))
		()
		()
		nil ; series dataflow constraint takes care
		))
        (t (fragl ((items t) (default) (amount)) ((shifted-items t))
                  ((shifted-items (series-element-type items))
		   (ring list (make-list (1+ amount) :initial-element default)))
		  ()
                  ((nconc ring ring))
                  ((rplaca ring items)
		   (setq ring (cdr ring))
                   (setq shifted-items (car ring)))
		  ()
		  ()
		  nil ; series dataflow constraint takes care
		  ))))

;; API
(defS latch (items &key (after nil) (before nil) (pre nil pre-p) (post nil post-p))
  "(latch items &key after before pre post)

The series returned by LATCH is the same as the input series ITEMS except
that some of the elements are replaced by other values.  LATCH acts like a
LATCH electronic circuit component.  Each input element causes the creation
of a corresponding output element.  After a specified number of non-null
input elements have been encountered, the latch is triggered and the output
mode is permanently changed.

The :AFTER and :BEFORE arguments specify the latch point.  The latch point
is just after the :AFTER-th non-null element in ITEMS or just before the
:BEFORE-th non-null element.  If neither :AFTER nor :BEFORE is specified,
an :AFTER of 1 is assumed.  If both are specified, it is an error.

If a :PRE is specified, every element prior to the latch point is replaced
by this value.  If a :POST is specified, every element after the latch
point is replaced by this value.  If neither is specified, a :POST of NIL
is assumed."
  (progn (when (and after before)
           (ers 79 "~%:AFTER and :BEFORE both specified in a call on LATCH."))
         (when (not (or before after))
	   (setq after 1))
         (when (null pre-p)
	   (setq post-p t))
         (cond (after
                (fragl ((items t) (after) (pre) (pre-p) (post) (post-p))
                       ((masked-items t))
		       ((masked-items t)
			(state fixnum after))
		       ()
                       ()
                       ((cond ((plusp state) (if items (decf state))
                               (setq masked-items (if pre-p pre items)))
                              (t (setq masked-items (if post-p post items)))))()
			      ()
			      nil ; series dataflow constraint takes care
			      ))
               (t (fragl ((items t) (before) (pre) (pre-p) (post) (post-p))
                         ((masked-items t))
                         ((masked-items t)
			  (state fixnum before))
			 ()
                         ()
                         ((cond ((and (plusp state)
                                      (or (null items)
                                          (not (zerop (setq state (1- state))))))
                                 (setq masked-items (if pre-p pre items)))
                                (t (setq masked-items
                                         (if post-p post items)))))
			 ()
			 ()
			 nil ; series dataflow constraint takes care
			 )))))

(defS until1 (bools items)
    "Returns ITEMS up to, but not including, the first non-null element of BOOLS."
  (fragl ((bools t) (items t)) ((items t)) () ()
         ()
	 ((if bools (go end)))
	 ()
	 ()
	 nil
	 ))

;; API
(defS until (bools items-1 &rest items-i)
  "(until bools items-1 &rest series-inputs)

Returns ITEMS-I up to, but not including, the first non-null element of BOOLS."
  (if (null items-i)
      (until1 bools items-1)
    (apply #'cotruncate
	   (mapcar #'(lambda (i) (until1 bools i)) (cons items-1 items-i))))
 :optimizer
  (cl:let ((extra-ins (mapcar #'(lambda (x) (declare (ignore x))
                                          (list (gensym "ITEMS") t))
                                items-i)))
    (apply-literal-frag
      (list* `(((bools t) (items t) ,@ extra-ins)
               ((items t) ,@(copy-tree extra-ins))
               () ()
               () ((if bools (go end))) () () nil)
             bools items-1 items-i))))

(defS until-if1 (pred items other-items)
  "Returns ITEMS up to, but not including, the first element which satisfies PRED."
  (fragl ((pred) (items t) (other-items t)) ((other-items t)) () ()
         () ((if (cl:funcall pred items) (go end))) () () nil))

;; API
(defS until-if (pred items-1 &rest items-i)
  "(until-if pred items-1 &rest series-inputs)

Returns ITEMS-i up to, but not including, the first element which
satisfies PRED."
  (if (null items-i)
      (until-if1 pred items-1 items-1)
    (apply #'cotruncate
	   (mapcar #'(lambda (i) (until-if1 pred items-1 i))
		   (cons items-1 items-i))))
 :optimizer
  (cl:let* ((params nil)
              (frag (make-frag :impure nil))
              (item-vars (n-gensyms (1+ (length items-i)) (symbol-name '#:items-)))
              (*state* nil))
    (multiple-value-setq (pred params) (handle-fn-arg frag pred params))
    (setq params (mapcar #'retify (nconc params (cons items-1 items-i))))
    (dolist (var item-vars)
      (+arg (make-sym :var var :series-var-p t) frag)
      (+ret (make-sym :var var :series-var-p t) frag))
    (setf (body frag)
          `((if ,(car (handle-fn-call frag nil pred (list (car item-vars)) t))
                (go ,end))))
    (apply-frag frag params)))

;; API
(defS positions (bools)
  "(positions bools)

Returns a series of the positions of non-null elements in bools.
Positions are counted from 0."
  (fragl ((bools t -X-)) ((index t)) ((index fixnum -1))
	 ()
         ()
         (L -X- (incf index) (if (not bools) (go L))) () () nil))

;; API
(defS mask (monotonic-indices)
  "(mask monotonic-indices)

Creates a series containing T in the indicated positions and NIL
elsewhere.  The positions must be a strictly increasing series of
non-negative integers."
  (fragl ((monotonic-indices t -x- d)) ((bools t))
         ((bools boolean t) (index fixnum 0))
	 ()
         ()
	 #-(and :allegro-version>= (version>= 5 0) (not (version>= 5 1)))
         (  (if (not bools) (go f))
          -x- (go f) d (setq index -1)
          f (setq bools (and (not (minusp index))
                             (= (prog1 index (incf index))
                                monotonic-indices))))
	 #+(and :allegro-version>= (version>= 5 0) (not (version>= 5 1)))
	 ((if (not bools) (go f)) (go forward) d (go e) forward
	  -x- (go f) e (setq index -1)
          f (setq bools (and (not (minusp index))
                             (= (prog1 index (incf index))
                                monotonic-indices))))
	 () () nil))

;; API
(defS choose (bools &optional (items nil items-p))
  "(choose bools &optional (items bools))

Chooses the elements of ITEMS corresponding to non-null elements of
BOOLS.  If ITEMS is not given, then the non-null elements of BOOLS is
returned"
  (cond (items-p
         (fragl ((bools t) (items t)) ((items t -X-)) () ()
                () ((if (not bools) (go F)) -X- F) () () nil))
        (t (fragl ((bools t -X-)) ((bools t)) () ()
                  () (L -X- (if (not bools) (go L))) () () nil))))

;; API
(defS choose-if (pred items)
  "(choose-if pred items)

Chooses the elements of ITEMS for which PRED is non-null."
  (cl:flet ((gen-unopt ()
              (fragl ((pred) (items t -X-)) ((items t)) () ()
		     () (L -X- (if (not (cl:funcall pred items)) (go L))) () () nil)))
    (eoptif-q
	(funcase (fun pred)
          ((name anonymous)
	   (efragl ((items t -X-))
		   `(((items t)) () ()
		     ()
		     (L -X- (if (not (,(if (symbolp fun) fun (process-fn fun)) items))
				(go L)))
		     () () nil)))
	  (t
	   (gen-unopt)))
      (gen-unopt))))

;; API
(defS expand (bools items &optional (default nil))
  "(expand bools items &optional (default nil))

The output contains the elements of the input series ITEMS spread out
into the positions specified by the non-null elements in BOOLS---that
is, ITEMS[j] is in the position occupied by the jth non-null element
in BOOLS.  The other positions in the output are occupied by DEFAULT.
The output stops as soon as BOOLS runs out of elements or a non-null
element in BOOLS is encountered for which there is no corresponding
element in ITEMS."
  (fragl ((bools t) (items t -X-) (default)) ((expanded t))
         ((expanded (series-element-type items)))
	 ()
	 ()
         ((when (not bools) (setq expanded default) (go F))
          -X- (setq expanded items)
          F)
	 () () nil))

;; API
(defS spread (gaps items &optional (default nil))
  "(spread gaps items &optional (default nil)

SPREAD is quite similar to EXPAND, except instead of giving booleans
on where to put the items, GAPS are specified which indicate how far
apart the items should be.  A gap of 0 means the items will be
adjacent."
  (fragl ((gaps t) (items t) (default)) ((expanded t -X-))
         ((expanded (series-element-type items)) (count fixnum))
	 ()
	 ()
         ((setq count gaps)
          L (setq expanded (if (zerop count) items default))
            -X-
            (when (plusp count) (decf count) (go L)))
	 () () nil))

;; API
(defS subseries (items start &optional (below nil below-p))
  "(subseries items start &optional below)

SUBSERIES returns a series containing the elements of the input series
ITEMS indexed by the non-negative integers from START up to, but not
including, BELOW.  If BELOW is omitted or greater than the length of ITEMS,
the output goes all of the way to the end of ITEMS."
  (cond (below-p
         (fragl ((items t -X-) (start) (below)) ((items t))
		((index (integer -1) -1))
		()
                ()
                (LP -X-
                    (incf index)
		    (locally
		      (declare (type nonnegative-integer index))
		      (if (>= index below) (go end))
		      (if (< index start) (go LP))))
		() () nil))
        (t (fragl ((items t -X-) (start)) ((items t))
		  ((index integer (- -1 start)))
		  ()
                  ()
                  (LP -X-
                      (incf index)
                      (if (minusp index) (go LP)))
		  () () nil))))

;; API
(defS mingle (items1 items2 comparator)
  "(mingle items1 items2 comparator)

Merges two series into one, with the same elements and order as ITEMS1
and ITEMS2.

The COMPARATOR must accept two arguments and return non-null if and only if
its first argument is strictly less than its second argument (in some
appropriate sense).  At each step, the COMPARATOR is used to compare the
current elements in the two series.  If the current element from ITEMS2 is
strictly less than the current element from ITEMS1, the current element is
removed from ITEMS2 and transferred to the output.  Otherwise, the next
output element comes from ITEMS1. "
  (fragl ((items1 t -X1- F1) (items2 t -X2- F2) (comparator)) ((items t))
         ((items (or (series-element-type items1)
                     (series-element-type items2)))
          (need1 fixnum 1) (need2 fixnum 1))
	 ()
         ()
         ((if (not (plusp need1)) (go F1))
          (setq need1 -1)
          -X1-
          (setq need1 0)
          F1 (if (not (plusp need2)) (go F2))
          (setq need2 -1)
          -X2-
          (setq need2 0)
          F2 (cond ((and (minusp need1) (minusp need2)) (go end))
                   ((minusp need1) (setq items items2) (setq need2 1))
                   ((minusp need2) (setq items items1) (setq need1 1))
                   ((not (cl:funcall comparator items2 items1))
                    (setq items items1) (setq need1 1))
                   (t (setq items items2) (setq need2 1))))
	 () () nil))

;;; Concatenation

(defS catenate2 (items1 items2) ""
  (fragl ((items1 t -X- F) (items2 t -Y-)) ((items t))
         ((items t) (flag boolean nil))
	 ()
         ()
         (  (if flag (go B))
          -X- (setq items items1) (go D)
          F (setq flag t)
          B -Y- (setq items items2) D)
	 () () nil))

;; API
(defS catenate (items1 items2 &rest more-items)
  "(catenate items1 items2 &rest more-items)

Concatenates two or more series end to end."
  (if more-items
      (catenate2 items1 (apply #'catenate items2 more-items))
    (catenate2 items1 items2))
 :optimizer
  (if more-items
      `(catenate2 ,items1 (catenate ,items2 ,@ more-items))
    `(catenate2 ,items1 ,items2)))

#|
(defS collect-union (&rest items)
  "Collect the union of 0 or more series sets"
  (cond ((cddr items)
	 (collect 'set (apply #'catenate items)))
	(items
	 (collect 'list items))
	(t
	 nil))
  :optimizer
    (cond ((cddr items)
	   `(collect 'set (catenate ,@items)))
	  (items
	   (basic-collect-list items))
	  (t
	   nil))
  :trigger t)
|#

;;; Splitting

;; HELPER
(cl:defun image-of-with-datum (g datum)
  (cl:let (item)
    (loop (setq item (basic-do-next-in g))
          (when (or (null (gen-state g))
		    (eql (car item) datum))
	    (return (cdr item))))))

;; OPTIMIZER
(cl:defun do-split (items stuff bools-p)
  (cl:let ((frag (make-frag :impure nil))
	   (ivar (new-var 'splititems))
	   (D (new-var 'dne)))
    (+arg (make-sym :var ivar :series-var-p t) frag)
    (dotimes (i (length stuff) i)
      (cl:let ((var (new-var 'h))
                 (-X- (new-var '-z-))
                 (S (new-var 'ss)))
        (+arg (make-sym :var var :series-var-p bools-p) frag)
        (+ret (make-sym :var ivar :series-var-p t :off-line-spot -X-) frag)
        (setf (body frag)
              `(,@(body frag)
                  (if (not ,(if bools-p var `(cl:funcall ,var ,ivar))) (go ,S))
                  ,-X-
                  (go ,D)
               ,S ))))
    (cl:let ((-X- (new-var '-Y-)))
      (+ret (make-sym :var ivar :series-var-p t :off-line-spot -X-) frag)
      (setf (body frag)
            `(,@(body frag)
               ,-X- ,D)))
    (apply-frag frag (cons items stuff))))

;; API
(defS split (items bools &rest more-bools)
  "(split items bools &rest more-bools)

Partition the input series ITEMS between several outputs.  If there
are N test inputs following ITEMS, then there are N+1 outputs.  Each
input element is placed in exactly one output series, depending on the
outcome of a sequence of tests.  If the element ITEMS[j] fails the
first K-1 tests and passes the kth test, it is put in the kth output.
If ITEMS[j] fails every test, it is placed in the last output.  In
addition, all output stops as soon as any series input runs out of
elements."
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
  (do-split items (cons bools (copy-list more-bools)) t))

;; API
(defS split-if (items pred &rest more-pred)
  "(split-if items pred &rest more-pred)

Partition the input series ITEMS between several outputs.  If there
are N test inputs following ITEMS, then there are N+1 outputs.  Each
input element is placed in exactly one output series, depending on the
outcome of a sequence of tests.  If the element ITEMS[j] fails the
first K-1 predicate tests and passes the kth test, it is put in the
kth output.  If ITEMS[j] fails every test, it is placed in the last
output.  In addition, all output stops as soon as any series input
runs out of elements."
  (cl:let* ((preds (list* pred (copy-list more-pred))))
    ;; FIXME
    ;;
    ;; This is a really gross hack because I don't know how
    ;; this works.  According to bug 516952, the following
    ;; doesn't work
    ;;
    ;; (split-if (scan '(1 2 1 2 3)) (lambda (item) (evenp item)))
    ;;
    ;; What happens is something like '(1 (1 2 1 2 3) ..) 
    ;; gets passed to pos-if, which is not expecting a list.
    ;;
    ;; However the obvious fix of just taking the car then fails for
    ;;
    ;; (split-if #z(1 2 1 2 3) (lambda (item) (evenp item)))
    ;;
    ;; because pos-if would have been passed the individual items.
    ;;
    ;; So the hack is to see if there is to have promote-series return
    ;; a second value that tells whether we're returning the series as
    ;; is (NIL) or we copied the series (T).  Then, if T, we take the
    ;; car of the item, otherwise just the item.
    ;;
    ;; If I (rtoy) were smarter I'd fix this better, but I'm
    ;; too stupid.
    (multiple-value-bind (ps altered)
	(promote-series items)
      (let ((pos-lists
	     (map-fn t
		     #'(lambda (item)
			 (let ((it (if altered
				       (car item)
				       item)))
			   (cons (apply #'pos-if it preds) item)))
		     ps)))
	(values-list
	 (mapcar #'(lambda (i)
		     (make-image-series :alter-fn (alter-fn items)
					:image-fn #'image-of-with-datum
					:image-datum i
					:image-base pos-lists))
		 (n-integers (+ 2 (length more-pred))))))))
 :optimizer
  (do-split items (cons pred (copy-list more-pred)) nil))

;; API
(defS every-nth (m n items)
    "Returns a series of every Nth element of ITEMS, after skipping M elements."
  (fragl ((m) (n) (items t -X-)) ((items t))
	 ((count fixnum (1- m)))
	 ()
         ()
         (L -X- (cond ((plusp count) (decf count) (go L))
                      (t (setq count (1- n))))) () () :args))

;; API
(defS chunk (m n &optional (items nil items-p))
  "(chunk m n items)

Break up the input series ITEMS into (possibly overlapping) chunks of
length M.  The starting positions of successive chunks differ by N.
The inputs M and N must both be positive integers.

CHUNK produces M output series.  The ith chunk consists of the ith
elements of the M outputs.  Suppose that the length of ITEMS is L.
The length of each output is the floor of 1+(L-M)/N.  The ith element
of the kth output is the (i*n+k)th element of ITEMS (i and k counting
from zero)."
  (progn
    (when (not items-p)        ;it is actually n that is optional
      (setq items n)
      (setq n 1))
    (cond ((not (typep m 'positive-integer))
           (ers 63 "~%M argument " m " to CHUNK fails to be a positive integer."))
          ((not (typep n 'positive-integer))
           (ers 64 "~%N argument " n " to CHUNK fails to be a positive integer."))
          (t (values-list
               (mapcar #'(lambda (i)
                           (every-nth m n
                                      (if (zerop i)
					  items
					(previous items nil i))))
                       (nreverse (n-integers m)))))))
 :optimizer
 (progn
   (when (not items-p)        ;it is actually n that is optional
     (setq items n)
     (setq n 1))
   (cond ((not (typep m 'positive-integer))
          (rrs 3 "~%M argument " m " to CHUNK fails to be a positive integer."))
         ((not (typep n 'positive-integer))
          (rrs 4 "~%N argument " n " to CHUNK fails to be a positive integer."))
         (t (cl:let* ((vars (n-gensyms m (symbol-name '#:chunk-)))
                        (outs (mapcar #'(lambda (v) (list v t)) vars))
                        (auxes (mapcar #'(lambda (v)
                                           `(,v ,(copy-list
                                                   '(series-element-type in))))
                                       vars))
                        (setqs (mapcar #'(lambda (u v) (list 'setq u v))
                                       vars (cdr vars))))
              (apply-frag
               (literal-frag
                `(((in t -X-)) ,outs ((count fixnum) ,@ auxes) ()
                  ((setq count ,(1- m)))
                  (L -X- ,@ setqs (setq ,(car (last vars)) in)
                     (cond ((plusp count) (decf count) (go L))
                           (t (setq count ,(1- n))))) () () :args))
               (list items)))))))

;; API
(defS collect-append (seq-type &optional (items nil items-p))
  "(collect-append [type] items)

Given a series of sequences, COLLECT-APPEND returns a new sequence by
concatenating these sequences together in order.  The TYPE is a type
specifier indicating the type of sequence created and must be a proper
subtype of SEQUENCE.  If TYPE is omitted, it defaults to LIST.  For
example:

  (COLLECT-APPEND #Z((A B) NIL (C D))) => (A B C D)"
  (progn
    (when (not items-p) ;it is actually seq-type that is optional
      (setq items seq-type)
      (setq seq-type (optq 'list)))
    (cond ((equal seq-type (optq 'list))
           (fragl ((items t)) ((lst))
		  ((lst list nil) (list-end list nil))
		  ()
                  ()
                  ((when items
                     (cl:multiple-value-bind (copy lastcons) (copy-list-last items)
		       (when list-end (rplacd list-end copy))
                       (setq list-end lastcons)
                       (when (null lst) (setq lst copy)))))
		  () () nil))
          (t (fragl ((seq-type) (items t)) ((seq))
		    ((seq t nil))
		    ()
                    ()
                    ((setq seq (cons items seq)))
                    ((setq seq (apply #'concatenate seq-type (nreverse seq))))
                    ()
		    nil))))
  :trigger t)

;; API
(defS collect-nconc (items)
  "(collect-nconc items)

COLLECT-NCONC NCONCs the elements of the series LISTS together in
order and returns the result.  This is the same as COLLECT-APPEND
except that the input must be a series of lists, the output is always
a list, the concatenation is done rapidly by destructively modifying
the input elements, and therefore the output shares all of its
structure with the input elements."
  (fragl ((items t)) ((lst))
	 ((lst list nil) (list-end list nil))
	 ()
         ()
         ((when items
            (when list-end (setf (cdr (last list-end)) items))
            (setq list-end items)
            (when (null lst) (setq lst items))))
	 () () nil)
 :trigger t)

;; API
(defS collect-hash (keys values &rest option-plist)
  "(collect-hash keys values :test :size :rehash-size :rehash-threshold)

Combines a series of keys and a series of values together into a hash
table.  The keyword arguments specify the attributes of the hash table
to be produced.  They are used as arguments to MAKE-HASH-TABLE"
  (fragl ((keys t) (values t) (option-plist))
	 ((table))
	 ((table t (apply #'make-hash-table option-plist)))
	 ()
         ()
         ((setf (gethash keys table) values)) () () nil)
 :optimizer
  (apply-literal-frag
    (list '(((keys t) (values t) (table)) ((table)) () ()
            () ((setf (gethash keys table) values)) () () nil)
          keys values `(make-hash-table ,@ option-plist)))
 :trigger t)

;; API
(defS collect-file (name items &optional (printer #'print))
  "(collect-file name items &optional (printer #'print)

This creates a file named FILE-NAME and writes the elements of the
series ITEMS into it using the function PRINTER.  PRINTER must accept
two inputs: an object and an output stream.  (For instance, PRINTER
can be PRINT, PRIN1, PRINC, PPRINT, WRITE-CHAR, WRITE-STRING, or
WRITE-LINE.)  If omitted, PRINTER defaults to PRINT.  The value T is
returned.  The file is correctly closed, even if an abort occurs."
  (fragl ((name) (items t) (printer)) ((out)) 
         ((out boolean t)
	  (lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons))
	 ((setq lastcons (setf (cdr lastcons) (cons items nil))))
         ((with-open-file (f name :direction :output)
            (dolist (item (cdr lst))
              (cl:funcall printer item f))))
	 ()
	 :context
	 )
 :optimizer
  (apply-literal-frag
    (cl:let ((file (new-var 'outfile)))
      `((((items t) (printer)) ((out)) 
         ((out boolean t))
	 ()
         ()
	 ((cl:funcall printer items ,file))
	 ()
         ((#'(lambda (c)
              (list 'with-open-file '(,file ,name :direction :output) c)) :loop))
	 :context
	 )
        ,items ,printer)))
 :trigger t)

;; API
(defS collect-stream (name items &optional (printer #'print))
    "Prints the elements of ITEMS onto the stream NAME."
  (fragl ((name) (items t) (printer)) (())
	 ((lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons))
	 ((setq lastcons (setf (cdr lastcons) (cons items nil))))
         ((dolist (item (cdr lst))
            (cl:funcall printer item name)))
	 ()
	 :context
	 )
 :optimizer
  (apply-literal-frag
   `((((items t) (printer)) (()) () ()
      () ((cl:funcall printer items ,name)) ()
      ((#'(lambda (c)
           c) :loop))
      :context
      )
     ,items ,printer))
 :trigger t)



;; API
(defS collect-alist (keys values)
  "(collect-alist keys values)

Combines a series of keys and a series of values together into an alist."
  (fragl ((keys t) (values t)) ((alist))
	 ((alist list nil))
	 ()
         ()
         ((setq alist (cons (cons keys values) alist)))
         () () nil)
 :trigger t)

;; API
(defS collect-plist (indicators values)
  "(collect-plist indicators values)

Combines a series of indicators and a series of values together into a plist."
  (fragl ((indicators t) (values t)) ((plist))
	 ((plist list nil))
	 ()
         ()
         ((setq plist (list* indicators values plist)))
         () () nil)
 :trigger t)

;; API
(defS collect-last (items &optional (default nil))
  "(collect-last items &optional (default nil))

Returns the last element of the series ITEMS.  If ITEMS has no
elements, DEFAULT is returned."
  (fragl ((items t) (default)) ((item))
         ((item (null-or (series-element-type items)) default))
	 ()
         ()
         ((setq item items))
	 () () nil)
 :trigger t)

;; API
(defS collect-first (items &optional (default nil))
  "(collect-first items &optional (default nil))

Returns the first element of the series ITEMS.  If ITEMS has no
elements, DEFAULT is returned."
  (fragl ((items t) (default)) ((item))
	 ((item (null-or (series-element-type items)) default))
	 ()
         ()
         ((setq item items) (go end))
	 () () nil)
 :trigger t)

;; API
(defS collect-nth (n items &optional (default nil))
  "(collect-nth n items &optional (default nil))

Returns the Nth element of the series ITEMS.  If ITEMS has no Nth
element, DEFAULT is returned."
  (fragl ((n) (items t) (default)) ((item))
         ((counter fixnum n)
          (item (null-or (series-element-type items)) default))
	 ()
         ()
         ((when (zerop counter) (setq item items) (go end))
          (decf counter)) () () nil)
 :trigger t)

;; API
(defS collect-and (bools)
  "(collect-and bools)

Computes the AND of the elements of BOOLS."
  (fragl ((bools t)) ((bool))
	 ((bool t t))
	 ()
         ()
	 ((if (null (setq bool bools)) (go end))) () () nil)
 :trigger t)

;; API
(defS collect-or (bools)
  "(collect-or bools)

Computes the OR of the elements of BOOLS."
  (fragl ((bools t)) ((bool))
	 ((bool t nil))
	 ()
         ()
	 ((if (setq bool bools) (go end))) () () nil)
 :trigger t)

;; API
(defS collect-length (items)
  "(collect-length items)
Returns the number of elements in ITEMS."
  (fragl ((items t)) ((number)) 
         ((number fixnum 0))
	 ()
         ()
	 ((incf number)) () () nil)
 :trigger t)

;; API
(defS collect-sum (numbers &optional (type 'number))
  "(collect-sum numbers &optional (type 'number))

Computes the sum of the elements in NUMBERS.  TYPE specifies the
type of sum to be created."
  (fragl ((numbers t) (type)) ((sum))
	 ((sum t (coerce-maybe-fold 0 type)))
	 ()
	 ()
         ((setq sum (+ sum numbers)))
	 () () nil)
 :optimizer
  (apply-literal-frag
    `((((numbers t)) ((sum))
       ((sum ,(must-be-quoted type) ,(coerce-maybe-fold 0 (must-be-quoted type)))) ()
       ()
       ((setq sum (+ sum numbers))) () () nil)
      ,numbers))
 :trigger t)

;; API
(defS collect-product (numbers &optional (type 'number))
  "(collect-product numbers &optional (type 'number))

Computes the product of the elements in NUMBERS.  TYPE specifies the
type of product to be created."
  (fragl ((numbers t)
          (type)) ((mul))
	  ((mul t (coerce-maybe-fold 1 type)))
	  ()
          ()
          ((setq mul (* mul numbers)))
	  () () nil)
  :optimizer
  (apply-literal-frag
   `((((numbers t)) ((mul))
      ((mul ,(must-be-quoted type) ,(coerce-maybe-fold 1 (must-be-quoted type)))) ()
      ()
      ((setq mul (* mul numbers))) () () nil)
     ,numbers))
  :trigger t)


;; API
(defS collect-max (numbers &optional (items numbers items-p) (default nil))
    "(collect-max numbers &optional (items numbers items-p) (default nil))

Returns the element of ITEMS that corresponds to the maximum element
of NUMBERS.  If ITEMS is omitted, then the maximum of NUMBERS itself
is returned.  The value DEFAULT is returned if either NUMBERS or ITEMS
has zero length."
  (if items-p
      (fragl ((numbers t) (items t) (default)) ((result))
             ((number t nil)
              (result (null-or (series-element-type items))))
	     ()
             ()
             ((if (or (null number) (< number numbers))
                  (setq number numbers result items)))
             ((if (null number) (setq result default)))
	     ()
	     nil)
    (fragl ((numbers t) (default)) ((number))
	   ((number t nil))
	   ()
	   ()
	   ((if (or (null number) (< number numbers)) (setq number numbers)))
	   ((if (null number) (setq number default)))
	   ()
	   nil))
 :trigger t)

;; API
(defS collect-min (numbers &optional (items numbers items-p) (default nil))
    "(collect-min numbers &optional (items numbers items-p) (default nil))

Returns the element of ITEMS that corresponds to the minimum element
of NUMBERS.  If ITEMS is omitted, then the minimum of NUMBERS itself
is returned.  The value DEFAULT is returned if either NUMBERS or ITEMS
has zero length."
  (if items-p
      (fragl ((numbers t) (items t) (default)) ((result))
             ((number t nil) 
              (result (null-or (series-element-type items))))
	     ()
             ()
             ((if (or (null number) (> number numbers))
                  (setq number numbers result items)))
             ((if (null number) (setq result default)))
	     ()
	     nil)
    (fragl ((numbers t) (default)) ((number))
	   ((number t nil))
	   ()
	   ()
	   ((if (or (null number) (> number numbers)) (setq number numbers)))
	   ((if (null number) (setq number default)))
	   ()
	   nil))
 :trigger t)

(pushnew :series *features*)

#+:cltl2-series
(pushnew :cltl2-series *features*)

;;#+:excl
;;(setf excl:*cltl1-in-package-compatibility-p* nil)
;;#+:excl
;;(setf comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

;;; A note on types.  Things are set up so that every aux variable
;;; (except the variable that is necessary when a non-series input is
;;; not a constant) is given a type declaration whereas inputs are
;;; never given types.  This ensures that everything is given a type
;;; definition and only once.  The only exception is that a user can
;;; use a type decl in a series::let which will then override the
;;; default type.  You can also wrap a (THE TYPE ...) around any
;;; series function call to override the types of the output.

;;; Some types are given in the form (series-element-type var) where
;;; var is a series input.  A final pass substitutes this type if it
;;; can be found.  Note that this is a purely one-way propagation of
;;; information starting on the inputs. The final pass also discards
;;; any type declarations which are t.

(provide "SERIES")

;;; ------------------------------------------------------------

;;; some things added since the last documentation
;;; #nM for returning multiple values
;;; Note #M does odd stuff with the keywords for keyword arguments, but
;;;   there is nothing we can do about this, because the documentation is very 
;;;   clear on what #M does.  If your are using implicit mapping,
;;;   #M is unnecessary anyway.
;;; *series-implicit-map*, note the detailed rules for when mapping
;;;   happens, which are much like OSS was, but more conservative.
;;;   We never map a function unless we MUST---i.e., only when one of
;;;   its actual arguments is a series.  We never map a special form except IF.
;;;   The virtue of this is that it is applicable on a single function
;;;   by single function basis, and gets the same results no matter what
;;;   the input looks like syntactically.  (Note you might not get portable
;;;   resuls. if some standard macro expands into code with an IF in one
;;;   implementation and without in another.)  (Note the forced evaluation
;;;   of non-series functions that are not last in a let etc. is already done.)

;;; ------------------------------------------------------------------------

;;; Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.

;;;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;    SOFTWARE.

;;; -------------------------------------------------------------------------

