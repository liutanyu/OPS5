Article 1003 of comp.sources.unix:
Path: s.cc.purdue.edu!h.cc.purdue.edu!j.cc.purdue.edu!pur-ee!iuvax!inuxc!ihnp4!ptsfa!pyramid!decwrl!sun!pitstop!sundc!seismo!uunet!rsalz
From: rsalz@uunet.UU.NET (Rich Salz)
Newsgroups: comp.sources.unix
Subject: v12i016:  OPS5 in Common Lisp, Part01/05
Message-ID: <2259@uunet.UU.NET>
Date: 14 Oct 87 02:18:57 GMT
Organization: UUNET Communications Services, Arlington, VA
Lines: 297
Approved: rs@uunet.UU.NET

Submitted-by: eric@dlcdev.UUCP (eric van tassell)
Posting-number: Volume 12, Issue 16
Archive-name: ops5/part01

[  I have not done anything with this code.  --r$  ]

[  Alert readers will note the conflict copyright/p-d claims in the
   first paragraph.  This source has been on CompuServe, apparently
   with the author's permission.  That, coupled with the note asking
   users to join a mailing list leads me to believe that this is PD
   code.  I can't imagine what the foriegn restrictions might be,
   since nothing in here is covered by export regulations.  --r$  ]

OPS5 is has been made public domain by C. Lanny Forgy. There are, I
believe, some restrictions on transporting OPS5 to some foreign
countries (which countries is pretty obvious).  The code is copyrighted
by Forgy, and anyone considering using it for commercial purposes
should probably contact him at CMU first.

The Vax Common Lisp and TI Explorer versions were ported by Dan Neiman
of the University of Massachusetts, COINS Dept.  They are *not*
guaranteed to be 100% bug free, particularly in the I/O functions, but
any bugs found should be mostly syntactic in nature.  The TI Explorer
takes advantage of some non-standard features not normally in Common
Lisp (such as the &quote keyword) and is somewhat cleaner; the Vax Lisp
version is more generic and will run on more systems.  The Common Lisp
versions are far from optimized, the major emphasis was on getting them
working and there are many idioms which could be expressed more
compactly and efficiently.

Modifications to OPS itself.  The ported versions of OPS are faithful
to the manual with the following exceptions, Common Lisp already
possesses functions remove, write, and call; the OPS5 functions have
been renamed oremove, owrite, and ocall respectively.  The OPS5
compilation functions have been modified to perform this renaming
automagically for RHS functions.  The user will have to remember to use
oremove when removing working memory from the top level.

Test programs:  There are not a lot of OPS5 benchmark programs
available.  The monkey and bananas program was included in the original
distribution.  The sort and Towers of Hanoi problems demonstrate OPS5,
but are not particularly good exemplars of the tasks that you want to
solve using a production system.


Questions about these versions of OPS5 can be directed to Dan Neiman,
at electronic mail addresses CSNET: dann@cs.umass.edu,
dann@umass-cs.csnet
          CompuServe: 72277,2604
          Real Mail: COINS Dept.
                     Lederle Graduate Research Center
                     UMass
                     Amherst, MA 01003  

--------------------
Appearing below is output from the diff utility (UNIX 4.3 bsd, if it
matters) indicating the changes needed to make the "Common Lisp" OPS5
source posted to this forum compatible with Gold Hill "GC 286 Developer."
Explanatory material is interspersed, denoted by ***. This file also
includes comments on GC 286 versus ExperLisp and ExperCommon Lisp on
the Macintosh.

With these changes, OPS5 compiles without errors, though I haven't 
verified all of its behaviors yet: the math operators seem to be
a little squirrely. The monkeys-and-bananas benchmark LOADS without
error and RUNs in a little over 5 seconds (COMPAQ DeskPro 286, 3 MB
extended memory, 8 MHz clock/6 MHz bus); this is on the order of
three times the time reported by Production Systems for a Symbolics
machine in their OPS83 advertising. Highly cost-effective!

This is the first thing we've tackled with GC 286 -- the COMPAQ was just
delivered a week ago, and we discovered and downloaded the OPS5 source
over that weekend. We're surely looking forward to refining it with some
of the Rete tools discussed in the January AIE. GC 286 is a wonder:
my co-workers, who think a Symbolics is a personal computer, were visibly
impressed by the excellent integration of the GMACS editor into the
GC 286 environment, and by the ability to jump from Lisp into DOS -- for
example, to run a terminal emulator and bring Lisp files down from the
VAX (which is Ethernetted to the Symbolics machines). They're already
starting to ship me Symbolics code with requests to investigate its
portability to GC 286 for delivery.

A final holiday note: I called my contact at Gold Hill to pass on some of
these impressions, the day before Christmas at 4:45 Boston time. "He's on
vacation," I was told, "but maybe I can help you." "Who's this?," I asked
innocently. Turned out to be Eugene Wang, president of the company... we
had a most enjoyable discussion of micro Lisps and the ANSI Lisp effort.
Some people are just a pleasure to do business with.

As a long-time user and generally a fan of ExperLisp (and now ExperCommon
Lisp), I can't help but regret some of the differences between Exper and
GC. Neither is uniformly better. Exper has the wonderful Mac editing 
interface, but GMACS is a more workmanlike environment with a lot more
power in searches, moves by expression, etc. ExperCommon can only edit
files up to 32K, requiring aggravating segmentation of the 100+K OPS5
source, while GMACS works quite happily with the whole thing at once.
ExperCommon chokes on the OPS5 definitions of { and }, since it uses
those as multi-line comment delimiters, and on OPS5 functions whose names
begin with $ since it defines that as a macro character for "hex value to
follow."

GC 286 runs elementary benchmarks like FIBONACCI about 30% slower in
its interpreter than ExperLisp 1.5 requires in its incremental compiler;
GC is faster than Exper 1.5 by a factor of 10 when compiled (the times
for Exper are on a 1 MB Mac with ExperLisp configured for 24,000 cons
cells and 4800 symbols; GC times are on the configuration given above).

On the other hand, the complete Mac toolbox integration of ExperCommon
is going to make for some really sensational applications. Imagine an
implementation of OPEN that makes the filename &optional, and if null
goes into the standard file dialogue for mouse-menu selection of any
file (with appropriate restrictions on type) in the entire hierarchical
file system. Nice...

If this sounds as if I'm not sure which I prefer, that's pretty accurate.
Muscle memory for GMACS keychords is developing quickly, and it's easy to
get spoiled by on-line documentation and all the other GC goodies. The
Mac user interface is hard to beat, though, and Apple's future CPU
upgrade path looks fairly smooth (compared to the chaos of 286 vs. 386
and IBM vs. Microsoft operating system intentions). I was at the meeting
of the Personal Computer Professionals Association that is reported in
PC Week's December 23rd issue, and the two front-page articles on the
meeting accurately capture the uproar. 1987 will be a tough year for
standards.

Herewith the diff results:
 
*** The == function is intended for small-integer comparisons. GC 286
*** appears to generate "eq" integers +/- 32767.

78c78,80
< (defun == (x y) (= x y))
---
> ;(defun == (x y) (= x y))
>
> (defun == (x y) (eq x y))

*** CHARACTER is valid CL but not implemented as a GC primitive.

194c196,198
<
---
>
> (defun character (x) (coerce x 'character))
>

*** The REMOVE-DUPLICATES function is commented out in the posted version,
*** but is not implemented as a GC primitive so needs to be enabled.

*** CxxxxR functions such as CADDDR are valid CL but not GC primitives.
*** Several changes are necessary to accommodate this, for example...
 
944c949
< (defun memory-part (mem-node) (car (cadddr mem-node)))
---
> (defun memory-part (mem-node) (car (car (cdddr mem-node))))
 
1044c1049
<        (equal (cddddr a) (cddddr b))
---
>        (equal (cdr (cdddr a)) (cdr (cdddr b)))

*** Here is an old-form DO that totally confuses CL. The GC compiler told
*** me that it was assuming "K" and "-" to be special variables...!
 
2225,2226c2228,2230
<         (do k (- width size) (1- k) (not (> k 0)) (princ '| | port))
<         (princ value port)))
---
>         (do ((k (- width size) (1- k)))
>           ((not (> k 0)) (princ '| | port)))
>     (princ value port)))

*** I'm not sure what the "correct" behavior is, but the following change
*** makes GC 286 print ^, as desired, instead of its ASCII value:
 
2596c2601
<          (princ '^ port)
---
>          (princ '|^| port)

*** More CxxxxR revisions...
 
2904c2903
< (defun find-right-mem (node) (memory-part (cadddr node)))
---
> (defun find-right-mem (node) (memory-part (car (cdddr node))))
 
3099c3093
<   (check-last-substr-index (cadddr x)))
---
>   (check-last-substr-index (car (cdddr x))))

*** The remaining changes are implementations and re-bindings that
*** should be self-explanatory, except for the "MATH" file load: this
*** is needed because GC does not AUTOLOAD library functions called
*** from compiled code, generating an error message that is highly
*** cryptic but *is* explained in a READ.ME file distributed with the
*** compiler.
 
> ;;; Calls and function definitions added for GC286
>
> (load (string-append SYS::*LISP-LIBRARY-PATHNAME* "MATH"))
>
> ;;; INTERSECTION as defined here works when interpreted, but not when
> ;;; compiled. For now, put it in USERINIT and comment it out of this
> ;;; source.
>
> ; (defun intersection (s1 s2 &rest keys&values)
> ;   (let ((:test (or (cadr (member ':test keys&values)) #'eql))
> ;     (:test-not (cadr (member ':test-not keys&values)))
> ;     (:key  (or (cadr (member ':key  keys&values)) #'identity))
> ;     (result nil))
> ;    (do ((patterns s1 (cdr patterns)))
> ;     ((null patterns) (nreverse result))
> ;     (let ((pattern (funcall :key (car patterns))))
> ;     (do ((data s2 (cdr data)))
> ;         ((null data))
> ;       (let ((datum (funcall :key (car data))))
> ;         (if (or (funcall :test pattern datum)
> ;                 (and :test-not (not (funcall :test-not pattern datum))))
> ;             (pushnew datum result ':test :test))))))))
>

*** The use of (SETF (SYMBOL-FUNCTION...) ...) instead of DEFUN generates a
*** compiler warning that (for example) GREATERP is referenced but not defined.
*** Of course, it works anyway.

> (setf (symbol-function 'greaterp) #'>)
>

*** PRINC-TO-STRING is valid CL but not a GC primitive.

> (defun princ-to-string (x)
>   (let ((outstream (make-string-output-stream)))
>     (princ x outstream)
>     (get-output-stream-string outstream)))
>

*** TYIPEEK is *not* CL, but I think the following does what's intended:

> (defun tyipeek (&optional input-stream &aux inchar)
>   (prog1
>     (setq inchar (read-char input-stream))
>     (unread-char inchar input-stream)))
>
> (setf (symbol-function 'difference) #'-
>       (symbol-function 'times     ) #'*)
>

*** EQUALP is valid CL, though not GC, and I think this is a correct
*** implementation:

> (defun equalp (x y)
>   (or (equal x y)
>       (and (numberp x) (numberp y) (= x y))
>       (and (characterp x) (characterp y) (char-equal x y))
>       (and (consp x) (consp y) (equalp (car x) (car y))
>                              (equalp (cdr x) (cdr y)))))
>

--------------------
;Common Lisp Support Functions: 
;These functions are not defined in vanilla Common Lisp, but are used
;in the OPSMODS.l code and in OPS5.


(defun putprop(name val att)
   (setf (get name att) val))

(defun memq(obj lis)
    (member obj lis :test #'eq))

(defun fix(num)
    (round num))
    

(defun assq(item alist)
     (assoc item alist :test #'eq))

(defun ncons(x) (cons x nil))

(defun neq(x y) (not (eq x y)))

(defun delq(obj list)
   (delete obj list :test #'eq))

(defmacro comment(&optional &rest x) nil) ;comment is a noop

(defun plus(x y)
   (+ x y))

(defun quotient(x y)
   (/ x y))

(defun flatc(x)
   (length (princ-to-string x)))


