                           Series RELEASE NOTES

                           Fernando D. Mato Mira
           CSEM (Centre Suisse d'Electronique et de Microtechnique)
                Jaquet-Droz 1, CH-2007 Neuchatel, Switzerland
                             matomira@acm.org


                              25th March, 2000


                    Series created by Richard C. Waters
                    

   Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

   Permission to use, copy, modify, and distribute this software and its
   documentation for any purpose and without fee is hereby granted,
   provided that this copyright and permission notice appear in all
   copies and supporting documentation, and that the name of M.I.T. not
   be used in advertising or publicity pertaining to distribution of the
   software without specific, written prior permission. M.I.T. makes no
   representations about the suitability of this software for any
   purpose.  It is provided "as is" without express or implied warranty.

   M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
   ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
   M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
   ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
   SOFTWARE.


                                CREDITS

                        Fostering by Raymond Toy
                   

                           Featuring Fixes by:

                              Tim Bradshaw    (Symbolics)
                             Alexey Dejneka
                              Bruno Haible    (CLISP)
                             Hannu Koivisto
                             Arthur Lemmens
                             Dave Linenberg
                             Lieven Marchand  (Allegro)
                               Joe Marshall
                            Reginald S. Perry
                              Pekka Pirinen   (Liquid)
                             Paul Werkowski   (CMUCL)

 
                          Additional Tests by:

                            Arthur Lemmens


                          Special Effects by

                          Many unknown others


Installation
------------

Both MK-DEFSYSTEM and ASDF system definitions are provided.  Assuming
you have properly set up either (or both) of those and taken care that
they can find the respective system definition files from the Series
installation directory (or in the case of ASDF alternatively symlinked
series.asd to a directory in the search path), you can use Series as a
dependency in your own systems and compile and load Series as follows:

ASDF:

(asdf:operate 'asdf:compile-op :series) ; compile only
(asdf:operate 'asdf:load-op :series)    ; compile if needed, load

MK-DEFSYSTEM:

(mk:oos :series :compile) ; compile if needed, load
(mk:oos :series :load)    ; just load

If you want to run the tests, [compile and] load the `series-test'
system and call CL-USER::DO-TESTS.  Don't get scared about
"compilation aborted" messages.  If it's reported that all tests
passed, it means it was just the error testing.

You can use SERIES::INSTALL for "use-package"ing Series in a way that
extended special forms are shadow-import'ed and reader macros are
installed.


Release notes
-------------

* Series 2.2.11 Release Notes
  o Update to work with CCL.
  o Implement feature request 2295778 - don't ignore fill-pointer.
  o Export COLLECT-IGNORE functionality.

* Series 2.2.10 Release Notes
  o Add optimizer for SERIES so (series t nil) returns #z(t nil t nil
  ...) instead of #(list t nil list t nil ...)
  o The #Z reader signals an error if we are trying to create an
  infinite literal series.
  o Many docstrings updated with more descriptive documentation.
  o Documentation updated 
  o Support ECL and SCL.

* Series 2.2.9 Release Notes

o Fix issue with SERIES being a declaration and a type, which is not
  allowed by ANSI CL.

* Series 2.2.8 Release Notes

o Support for SBCL and asdf.
o Some fixes for CMUCL 18e and later.
o ALTER was not working in some cases.
o Bug 434120 fixed.

* Series 2.2.7 Release Notes

o Some changes for CLISP 2.29.  The declaration SYSTEM::READ-ONLY was
  blocking optimization.
o Fix bugs:
  498418:  cmucl doesn't like dumping functions
  516952:  only optimized split-if works
o A bug in scan-fn-opt causing unbound variables in some situations
  has been fixed.  (From Joe Marshall.)
o The original (ASCII) documentation for series is now included in
  s-doc.txt.  
o Added a code walker for walking macrolet forms for
  Clisp.  Useful since Clisp macroexpands extended loop forms into
  forms containing macrolet.


* Series 2.2.6 Release Notes

o Use list pretty-printer for printing out series (for CMUCL).
o Some more doc strings.
o Some optimizations from Joe Marshall for Allegro.  Allegro
  apparently doesn't fold constants in coerce.
o Make series work with Allegro in modern mode (case-sensitive
  reader).


* Series 2.2.6 was never released


* Series 2.2.5 Release Notes

o Fixed a bug in scan-stream returning the wrong stuff.


* Series 2.2.4 Release Notes

o Fixed a bug wherein Clisp fails on test 530.  (Some variables were
  sharing structure?)  Clisp passes all tests now.
o Made changes to support MCL.  MCL now passes all tests.  (Thanks to
  Rainer Joswig for testing and debugging this version!)


* Series 2.2.3 Release Notes

Two small bugs are fixed in this release:

1. collect 'vector sometimes collected the result in reverse order.
   (See Bug #108331.)
2. scan fails sometimes when scanning a constant. (See Bug #113625.)


* Series 2.2.2 Release Notes

NEW SPECIFICATION RESTRICTIONS

1. The consequences of using the gatherers introduced by GATHERING
   outside the dynamic scope of the form are now explicitly stated to
   be undefined, except for those declared INDEFINITE-EXTENT.
   
   If you need them to be indefinite extent by default, evaluate
   (pushnew :cltl2-series) before compilation (or interpreted load)

   After loading series :cltl2-series will indicate if
   backward-compatibility mode is enabled.

   Rationale for this change: 
    "Weird code must be the one escaping gatherers so created"
    "WHO EVER DID THAT???"


NEW FEATURES:
------------

- GATHER-NEXT
  A macro with the same effect as the NEXT-OUT function, but with
  result undefined.
- GATHER-RESULT
  A macro with the same semantics as the RESULT-OF function,
- FGATHER-NEXT
  Like GATHER-NEXT, but the gatherer argument must be a symbol
  denoting a gatherer bound by FLET.
- FGATHER-RESULT
  Like GATHER-RESULT, but the gatherer argument must be a symbol
  denoting a gatherer bound by FLET.
- FGATHERING
  Like GATHERING, but where the gatherers are bound by FLET instead of
  LET.  Same extent rules as for GATHERING apply. Remember that where
  you would use (declare (indefinite-extent g)) inside GATHERING, you
  need to do (declare (indefinite-extent #'g)) with FGATHERING.
- GATHERLET
  The basic construct upon which GATHERING rests. No automatic return
  of gatherer results. And no extent restrictions, of course. If you
  want something dynamic-extent, you'll have to declare it.
- FGATHERLET
  The `F' version of GATHERLET. Duh.
- COLLECT-PRODUCT
  Like COLLECT-SUM, but for #'*. How more obvious does it get?
- INDEFINITE-EXTENT
  Declaration exported.
- (collect 'set
  Collects a series into a list removing any duplicates in the most
  efficient way possible.
- (collect 'ordered-set
  Collects a series into a list removing any duplicates but keeping the 
  original series order.
- SCAN now allows to drop the type specifier for any argument
  [:cltl2-series reactivates the old 'list assumption]
- SCAN now can scan multidimensional arrays in row-major order.

IMPROVEMENTS:
------------
- MACROLETs or CLOS::VARIABLE-REBINDING declarations do not block
  optimization under LispWorks.
- Better code generation
  . Stricter typing.
  . OUTPUT IS NOW `LETIFIED'. SETQs GO HOME!!
    (Replaced as many SETQs as possible by initializations at LET
    binding time)
  . Removed global function namespace pollution.
  . Encapsulated series subfunctions.
  . Prevented generation of some dead termination code.
  . Whole expression (t) and :loop wrapper placement
    (:prolog and :epilog support easy to add, if necessary).
  . Optimized collections of the form (collect [type] (scan [type]
    ({list | ..}
  . Some fixnum declarations were further constrained.
  . Optimized scanning of constant sequences.
  . Somewhat optimized scanning of "empty" vectors, ie,
    declared to be of constant 0 length, like in
    (collect (scan '(vector t 0) <gimme-a-huge-array-to-throw-away>) 
    now gives you NIL generating/executing less instructions.
    [<gimme-a-huge-array-to-throw-away> is still executed if not constantp, 
     though]
  . Variables of type NULL are replaced by constant NILs.
  . Avoid indirect funcalling through a variable in CHOOSE-IF and
    basic-collect-fn whenever possible.
  . Avoid consing in MAP-FN.
  . Removes fake series result variables from generated code.
  . polycall et al. are now tail recursive [If your compiler cannot
    optimize this, bark to your provider]

- Source clean up.
  Local series functions are almost there (FLET extension)
  Bottom-up definition for improved compilation.
  Make source `FLET and LABELS ready'.
  Renamed old fragL *fragL. Used new `*type*-free' fragL whenever possible.
  `readability/documentation' release.
  Abstracted use of aux component of frags.
  Some work done towards multiple assignment support in PRODUCING.
  Added private ANSI-compliant eval-when macro when special form is old-style.
  Abstracted use of wrapper component of frags.
  GENERATOR deftyped to CONS, not LIST, when necessary.
  Abstracted use of prolog component of frags.
  MERGE-FRAGS no longer depends on the exact order of frag components.
  Removed almost all uses of *fragL already.

BUG FIXES:
---------
- Does not unintern EXT:COLLECT and EXT:ITERATE anymore under CMUCL!!!
- GATHERING et al. are not bitten by CMUCL DEFSTRUCT bugs.
- Allegro 5.x needs the GENERATOR deftype, too.
- Workaround for ACL 5.0.1 TAGBODY bug.
- ALL-TIME SERIES BUG FIX: wrappers now inserted more precisely.
- Some initial (unread) bindings of variables could be NIL,
  contradicting declarations. Uses LOCALLY now for those (and also for
  all others for which NIL is OK, but which can be left unbound, if an
  implementation provides another version of LET that does not do NIL
  defaulting (and ideally would raise a compilation error if a
  variable is read before its written to)).
- Full letification works (modulo off-lining, and disabled because
  still useless).
- Fixed letified merge-frags bug.
- Adapted handle-dflow and non-series-merge for letification.
- Some incorrect fixnum declarations were relaxed.
- Improved some declarations to avoid spurious range warnings regarding 
  dead code by not-so-smart compilers.
- Removed redundant &optional in binding lists.
- lister-p was too aggressive regarding *CD*R

COMPILING
--------
(load "s-package")
(compile-file "s-package")
(compile-file "s-code")

LOADING
-------
(load "s-package")
(load "s-code")

TESTING
-------
Load SERIES, then:
(compile-file "s-test")
(load "s-test")
(user::do-tests)
[Don't get scared about "compilation aborted" messages. 
 If it's reported that all tests passed, 
 it means it was just the error testing]

"use-package'ing"
-----------------
Load SERIES, then:
(series::install)
[This will shadow-import extended special forms and install readmacros]


QUALIFIED ON:
-----------
 
CMUCL 18b (cmucl-2.4.19-2)           [SuSE Linux 6.3/Intel] 
CMUCL 18c                            [Solaris]          
LispWorks 4.1                        [Windows NT 4.0 SP6]
clisp-99.01.08-21 with -a option     [SuSE Linux 6.3/Intel]
ACL 5.0.1                            [SuSE Linux 6.3/Intel]
