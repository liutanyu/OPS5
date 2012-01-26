Article 1000 of comp.sources.unix:
Path: s.cc.purdue.edu!h.cc.purdue.edu!j.cc.purdue.edu!pur-ee!iuvax!inuxc!ihnp4!ptsfa!pyramid!decwrl!sun!pitstop!sundc!seismo!uunet!rsalz
From: rsalz@uunet.UU.NET (Rich Salz)
Newsgroups: comp.sources.unix
Subject: v12i017:  OPS5 in Common Lisp, Part02/05
Message-ID: <2260@uunet.UU.NET>
Date: 14 Oct 87 02:20:09 GMT
Organization: UUNET Communications Services, Arlington, VA
Lines: 911
Approved: rs@uunet.UU.NET

Submitted-by: eric@dlcdev.UUCP (eric van tassell)
Posting-number: Volume 12, Issue 17
Archive-name: ops5/part02


; File OPS5.common.1.lsp: part 1 of OPS5 in Common Lisp
; ----------


;	VPS2 -- Interpreter for OPS5
;
;	Copyright (C) 1979, 1980, 1981
;	Charles L. Forgy,  Pittsburgh, Pennsylvania



; Users of this interpreter are requested to contact

;
;	Charles Forgy
;	Computer Science Department
;	Carnegie-Mellon University
;	Pittsburgh, PA  15213
; or
;	Forgy@CMUA
; 
; so that they can be added to the mailing list for OPS5.  The mailing list
; is needed when new versions of the interpreter or manual are released.



;;; Definitions

;#+ vax (defun putprop(name val att)
;   (setf (get name att) val))



(proclaim '(special *matrix* *feature-count* *pcount* *vars* *cur-vars*
          *curcond* *subnum* *last-node* *last-branch* *first-node*
          *sendtocall* *flag-part* *alpha-flag-part* *data-part*
          *alpha-data-part* *ce-vars* *virtual-cnt* *real-cnt*
          *current-token* *c1* *c2* *c3* *c4* *c5* *c6* *c7* *c8* *c9*
          *c10* *c11* *c12* *c13* *c14* *c15* *c16* *c17* *c18* *c19*
          *c20* *c21* *c22* *c23* *c24* *c25* *c26* *c27* *c28* *c29*
          *c30* *c31* *c32* *c33* *c34* *c35* *c36* *c37* *c38* *c39*
          *c40* *c41* *c42* *c43* *c44* *c45* *c46* *c47* *c48* *c49*
          *c50* *c51* *c52* *c53* *c54* *c55* *c56* *c57* *c58* *c59*
          *c60* *c61* *c62* *c63* *c64* *record-array* *result-array* 
          *max-cs* *total-cs* *limit-cs* *cr-temp* *side*
          *conflict-set* *halt-flag* *phase* *critical*
          *cycle-count* *total-token* *max-token* *refracts* 
          *limit-token* *total-wm* *current-wm* *max-wm*
          *action-count* *wmpart-list* *wm* *data-matched* *p-name*
          *variable-memory* *ce-variable-memory* 
          *max-index* ; number of right-most field in wm element 
          *next-index* *size-result-array* *rest* *build-trace* *last*
          *ptrace* *wtrace* *in-rhs* *recording* *accept-file* *trace-file* 
          *mtrace* *madeby* ; used to trace and record makers of elements
          *write-file* *record-index* *max-record-index* *old-wm*
          *record* *filters* *break-flag* *strategy* *remaining-cycles*
	  *wm-filter* *rhs-bound-vars* *rhs-bound-ce-vars* *ppline* 
	  *ce-count* *brkpts* *class-list* *buckets* *action-type*
          *literals*   ;stores literal definitions
          *pnames*     ;stores production names
	  *externals*  ;tracks external declarations 
          *vector-attributes*  ;list of vector-attributes
	  ))

;(declare (localf ce-gelm gelm peek-sublex sublex
;          eval-nodelist sendto and-left and-right not-left not-right
;          top-levels-eq add-token real-add-token remove-old
;          remove-old-num remove-old-no-num removecs insertcs dsort
;          best-of best-of* conflict-set-compare =alg ))


;;; Functions that were revised so that they would compile efficiently


;* The function == is machine dependent\!
;* This function compares small integers for equality.  It uses EQ
;* so that it will be fast, and it will consequently not work on all
;* Lisps.  It works in Franz Lisp for integers in [-128, 127]


;(defun == (&rest z) (= (cadr z) (caddr z)))
(defun == (x y) (= x y))

; =ALG returns T if A and B are algebraicly equal.

(defun =alg (a b) (= a b))

(defmacro fast-symeval (&rest z)
	 `(cond ((eq ,(car z) '*c1*) *c1*)
		((eq ,(car z) '*c2*) *c2*)
		((eq ,(car z) '*c3*) *c3*)
		((eq ,(car z) '*c4*) *c4*)
		((eq ,(car z) '*c5*) *c5*)
		((eq ,(car z) '*c6*) *c6*)
		((eq ,(car z) '*c7*) *c7*)
		(t (eval ,(car z)))  ))

; getvector and putvector are fast routines for using one-dimensional
; arrays.  these routines do no checking; they assume
;	1. the array is a vector with 0 being the index of the first
;	   element
;	2. the vector holds arbitrary list values
;defun versions are useful for tracing

; Example call: (putvector array index value)

(defmacro putvector (array_ref ind var)
      `(setf (aref ,array_ref ,ind) ,var))

;(defun putvector (array_ref ind var)
;      (setf (aref array_ref ind) var))

; Example call: (getvector name index)

;(defmacro getvector(&rest z)
;     (list 'cxr (caddr z) (cadr z)))

(defmacro getvector(array_ref ind)
      `(aref ,array_ref ,ind))

;(defun getvector (array_ref ind)
 ;       (aref array_ref ind))

(defun ce-gelm (x k)
  (prog nil
   loop (and (== k 1.) (return (car x)))
        (setq k (1- k))
        (setq x (cdr x))
        (go loop))) 

; The loops in gelm were unwound so that fewer calls on DIFFERENCE
; would be needed

(defun gelm (x k)
  (prog (ce sub)
        (setq ce  (floor (/ k 10000)))
        (setq sub (- k (* ce 10000)))
 celoop (and (== ce 0) (go ph2))
        (setq x (cdr x))
        (and (== ce 1) (go ph2))
        (setq x (cdr x))
        (and (== ce 2) (go ph2))
        (setq x (cdr x))
        (and (== ce 3) (go ph2))
        (setq x (cdr x))
        (and (== ce 4) (go ph2))
        (setq ce (- ce 4))
        (go celoop)
   ph2  (setq x (car x))
   subloop (and (== sub 0) (go finis))
        (setq x (cdr x))
        (and (== sub 1) (go finis))
        (setq x (cdr x))
        (and (== sub 2) (go finis))
        (setq x (cdr x))
        (and (== sub 3) (go finis))
        (setq x (cdr x))
        (and (== sub 4) (go finis))
        (setq x (cdr x))
        (and (== sub 5) (go finis))
        (setq x (cdr x))
        (and (== sub 6) (go finis))
        (setq x (cdr x))
        (and (== sub 7) (go finis))
        (setq x (cdr x))
        (and (== sub 8) (go finis))
        (setq sub (- sub 8))
        (go subloop)
   finis (return (car x)))) 


;;; Utility functions



(defun printline (x) (mapc (function printline*) x)) 

(defun printline* (y) (princ '| |) (print y)) 

(defun printlinec (x) (mapc (function printlinec*) x)) 

(defun printlinec* (y) (princ '| |) (princ y)) 

; intersect two lists using eq for the equality test

(defun interq (x y)
  (intersection x y :test #'eq))

(defun enter (x ll)
   (and (not (member x ll :test #'equal))
       (push x ll)))


;Hack read-macro tables to accept single characters -- right out of CL book.
(defun single-macro-character (stream char)
   (declare (ignore stream))
   (character char))

(defun i-g-v nil
 (prog (x)
        (set-macro-character #\{ #'single-macro-character )
        (set-macro-character #\} #'single-macro-character )
        (set-macro-character #\^ #'single-macro-character )
;	(setsyntax '\{ 66.) ;These are already normal characters in CL
;	(setsyntax '\} 66.)
;	(setsyntax '^ 66.)
	(setq *buckets* 64.)		; OPS5 allows 64 named slots
	(setq *accept-file* nil)
	(setq *write-file* nil)
	(setq *trace-file* nil)
        (and (boundp '*class-list*)
          (mapc #'(lambda(class) (putprop class nil 'att-list)) *class-list*))
	(setq *class-list* nil)
	(setq *brkpts* nil)
	(setq *strategy* 'lex)
  	(setq *in-rhs* nil)
  	(setq *ptrace* t)
  	(setq *wtrace* nil)
	(setq *mtrace* t)            ; turn on made-by tracing
	(setq *madeby* nil)          ; record makers of wm elements
  	(setq *recording* nil)
        (setq *refracts* nil)
	(setq *real-cnt* (setq *virtual-cnt* 0.))
	(setq *max-cs* (setq *total-cs* 0.))
  	(setq *limit-token* 1000000.)
	(setq *limit-cs* 1000000.)
	(setq *critical* nil)
	(setq *build-trace* nil)
	(setq *wmpart-list* nil)
        (setq *pnames* nil)
        (setq *literals* nil) ; records literal definitions
	(setq *externals* nil) ; records external definitions
	(setq *vector-attributes* nil) ;records vector attributes
	(setq *size-result-array* 127.)
	(setq *result-array* (make-array 128))
	(setq *record-array* (make-array 128))
	(setq x 0)
        (setq *pnames* nil)     ; list of production names
  loop	(putvector *result-array* x nil)
	(setq x (1+ x))
	(and (not (> x *size-result-array*)) (go loop))
	(make-bottom-node)
	(setq *pcount* 0.)
	(initialize-record)
	(setq *cycle-count* (setq *action-count* 0.))
	(setq *total-token*
	       (setq *max-token* (setq *current-token* 0.)))
	(setq *total-cs* (setq *max-cs* 0.))
	(setq *total-wm* (setq *max-wm* (setq *current-wm* 0.)))
	(setq *conflict-set* nil)
	(setq *wmpart-list* nil)
	(setq *p-name* nil)
	(setq *remaining-cycles* 1000000)
))

; if the size of result-array changes, change the line in i-g-v which
; sets the value of *size-result-array*

(defun %warn (what where)
  (prog nil
    (terpri)
    (princ '?)
    (and *p-name* (princ *p-name*))
    (princ '|..|)
    (princ where)
    (princ '|..|)
    (princ what)
    (return where))) 

(defun %error (what where)
    (%warn what where)
    (throw '!error! nil)) 


(defun top-levels-eq (la lb)
  (prog nil
   lx   (cond ((eq la lb) (return t))
              ((null la) (return nil))
              ((null lb) (return nil))
              ((not (eq (car la) (car lb))) (return nil)))
        (setq la (cdr la))
        (setq lb (cdr lb))
        (go lx))) 


;;; LITERAL and LITERALIZE

(defmacro literal (&rest z)
  `(prog (atm val old args)
        (setq args ',z)
   top  (and (atom args) (return 'bound))
        (or (eq (cadr args) '=) (return (%warn '|wrong format| args)))
        (setq atm (car args))
        (setq val (caddr args))
        (setq args (cdddr args))
        (cond ((not (numberp val))
               (%warn '|can bind only to numbers| val))
              ((or (not (symbolp atm)) (variablep atm))
                (%warn '|can bind only constant atoms| atm))
              ((and (setq old (literal-binding-of atm)) (not (equal old val)))
               (%warn '|attempt to rebind attribute| atm))
              (t (putprop atm val 'ops-bind )))
        (go top))) 

(defmacro literalize (&rest l)
  `(prog (class-name atts)
    (setq class-name (car ',l))
    (cond ((have-compiled-production)
           (%warn '|literalize called after p| class-name)
           (return nil))
          ((get class-name 'att-list)
           (%warn '|attempt to redefine class| class-name)
	   (return nil)))
    (setq *class-list* (cons class-name *class-list*))
    (setq atts (remove-duplicates (cdr ',l)))
    (test-attribute-names atts)
    (mark-conflicts atts atts)
    (putprop class-name  atts 'att-list))) 

(defmacro vector-attribute  (&rest l)
  `(cond ((have-compiled-production)
         (%warn '|vector-attribute called after p| ',l))
        (t 
         (test-attribute-names ',l)
	 (mapc (function vector-attribute2) ',l)))) 

(defun vector-attribute2 (att) (putprop att t 'vector-attribute)
			       (setq  *vector-attributes* 
				   (enter att *vector-attributes*)))

(defun is-vector-attribute (att) (get att 'vector-attribute))

(defun test-attribute-names (l)
  (mapc (function test-attribute-names2) l)) 

(defun test-attribute-names2 (atm)
  (cond ((or (not (symbolp atm)) (variablep atm))
         (%warn '|can bind only constant atoms| atm)))) 

(defun finish-literalize nil
  (cond ((not (null *class-list*))
         (mapc (function note-user-assigns) *class-list*)
         (mapc (function assign-scalars) *class-list*)
         (mapc (function assign-vectors) *class-list*)
         (mapc (function put-ppdat) *class-list*)
         (mapc (function erase-literal-info) *class-list*)
         (setq *class-list* nil)
         (setq *buckets* nil)))) 

(defun have-compiled-production nil (not (zerop *pcount*))) 

(defun put-ppdat (class)
  (prog (al att ppdat)
        (setq ppdat nil)
        (setq al (get class 'att-list))
   top  (cond ((not (atom al))
               (setq att (car al))
               (setq al (cdr al))
               (setq ppdat
                     (cons (cons (literal-binding-of att) att)
                           ppdat))
               (go top)))
        (putprop class ppdat 'ppdat))) 

; note-user-assigns and note-user-vector-assigns are needed only when
; literal and literalize are both used in a program.  They make sure that
; the assignments that are made explicitly with literal do not cause problems
; for the literalized classes.

(defun note-user-assigns (class)
  (mapc (function note-user-assigns2) (get class 'att-list)))

(defun note-user-assigns2 (att)
  (prog (num conf buck clash)
        (setq num (literal-binding-of att))
	(and (null num) (return nil))
	(setq conf (get att 'conflicts))
	(setq buck (store-binding att num))
	(setq clash (find-common-atom buck conf))
	(and clash
	     (%warn '|attributes in a class assigned the same number|
	            (cons att clash)))
        (return nil)))

(defun note-user-vector-assigns (att given needed)
  (and (> needed given)
       (%warn '|vector attribute assigned too small a value in literal| att)))

(defun assign-scalars (class)
  (mapc (function assign-scalars2) (get class 'att-list))) 

(defun assign-scalars2 (att)
  (prog (tlist num bucket conf)
        (and (literal-binding-of att) (return nil))
        (and (is-vector-attribute att) (return nil))
        (setq tlist (buckets))
        (setq conf (get att 'conflicts))
   top  (cond ((atom tlist)
               (%warn '|could not generate a binding| att)
               (store-binding att -1.)
               (return nil)))
        (setq num (caar tlist))
        (setq bucket (cdar tlist))
        (setq tlist (cdr tlist))
        (cond ((disjoint bucket conf) (store-binding att num))
        (t (go top))))) 

(defun assign-vectors (class)
  (mapc (function assign-vectors2) (get class 'att-list))) 

(defun assign-vectors2 (att)
  (prog (big conf new old need)
        (and (not (is-vector-attribute att)) (return nil))
        (setq big 1.)
        (setq conf (get att 'conflicts))
   top  (cond ((not (atom conf))
               (setq new (car conf))
               (setq conf (cdr conf))
               (cond ((is-vector-attribute new)
                      (%warn '|class has two vector attributes|
		              (list att new)))
                     (t (setq big (max (literal-binding-of new) big))))
               (go top)))
        (setq need (1+ big))
	(setq old (literal-binding-of att))
	(cond (old (note-user-vector-assigns att old need))
	      (t (store-binding att need)))
        (return nil)))

(defun disjoint (la lb) (not (find-common-atom la lb))) 

(defun find-common-atom (la lb)
  (prog nil
   top  (cond ((null la) (return nil))
              ((member (car la) lb :test #'eq) (return (car la)))
              (t (setq la (cdr la)) (go top))))) 

(defun mark-conflicts (rem all)
  (cond ((not (null rem))
         (mark-conflicts2 (car rem) all)
         (mark-conflicts (cdr rem) all)))) 

(defun mark-conflicts2 (atm lst)
  (prog (l)
        (setq l lst)
   top  (and (atom l) (return nil))
        (conflict atm (car l))
        (setq l (cdr l))
        (go top))) 

(defun conflict (a b)
  (prog (old)
    (setq old (get a 'conflicts))
    (and (not (eq a b))
         (not (member b old :test #'eq))
         (putprop a (cons b old) 'conflicts )))) 

;(defun remove-duplicates (lst)
;  (cond ((atom lst) nil)
;        ((member (car lst) (cdr lst) :test #'eq) (remove-duplicates (cdr lst)))
;        (t (cons (car lst) (remove-duplicates (cdr lst)))))) 

(defun literal-binding-of (name) (get name 'ops-bind)) 

(defun store-binding (name lit)
  (putprop name lit 'ops-bind)
  (add-bucket name lit)) 

(defun add-bucket (name num)
  (prog (buc)
    (setq buc (assoc num (buckets)))
    (and (not (member name buc :test #'eq))
         (rplacd buc (cons name (cdr buc))))
    (return buc))) 

(defun buckets nil
  (and (atom *buckets*) (setq *buckets* (make-nums *buckets*)))
  *buckets*) 

(defun make-nums (k)
  (prog (nums)
        (setq nums nil)
   l    (and (< k 2.) (return nums))
        (setq nums (cons (cons k nil) nums))
        (setq k (1- k))
        (go l))) 

;(defun erase-literal-info (class)
;  (mapc (function erase-literal-info2) (get class 'att-list))
;  (remprop class 'att-list)) 

; modified to record literal info in the variable *literals*
(defun erase-literal-info (class)
      (setq *literals*
            (cons (cons class (get class 'att-list)) *literals*))
      (mapc (function erase-literal-info2) (get class 'att-list))
      (remprop class 'att-list))


(defun erase-literal-info2 (att) (remprop att 'conflicts)) 


;;; LHS Compiler

(defmacro p (&rest z) 
 `(progn 
   (finish-literalize)
   (princ '*) 
  ;(drain);drain probably drains a line feed
   (compile-production (car ',z) (cdr ',z)))) 

(defun compile-production (name matrix)
  (prog (erm)
        (setq *p-name* name)
        (setq erm (catch '!error! (cmp-p name matrix) ))
	; following line is modified to save production name on *pnames*
        (and (null erm) (setq *pnames* (enter name *pnames*)))
	(setq *p-name* nil)
	(return erm)))

(defun peek-lex nil (car *matrix*)) 

(defun lex nil
  (prog2 nil (car *matrix*) (setq *matrix* (cdr *matrix*)))) 

(defun end-of-p nil (atom *matrix*)) 

(defun rest-of-p nil *matrix*) 

(defun prepare-lex (prod) (setq *matrix* prod)) 


(defun peek-sublex nil (car *curcond*)) 

(defun sublex nil
  (prog2 nil (car *curcond*) (setq *curcond* (cdr *curcond*)))) 

(defun end-of-ce nil (atom *curcond*)) 

(defun rest-of-ce nil *curcond*) 

(defun prepare-sublex (ce) (setq *curcond* ce)) 

(defun make-bottom-node nil (setq *first-node* (list '&bus nil))) 

(defun cmp-p (name matrix)
  (prog (m bakptrs)
        (cond ((or (null name) (listp name))
               (%error '|illegal production name| name))
              ((equal (get name 'production) matrix)
	       (return nil)))
        (prepare-lex matrix)
        (excise-p name)
        (setq bakptrs nil)
        (setq *pcount* (1+ *pcount*))
        (setq *feature-count* 0.)
	(setq *ce-count* 0)
        (setq *vars* nil)
        (setq *ce-vars* nil)
	(setq *rhs-bound-vars* nil)
	(setq *rhs-bound-ce-vars* nil)
        (setq *last-branch* nil)
        (setq m (rest-of-p))
   l1   (and (end-of-p) (%error '|no '-->' in production| m))
        (cmp-prin)
        (setq bakptrs (cons *last-branch* bakptrs))
        (or (eq '--> (peek-lex)) (go l1))
        (lex)
	(check-rhs (rest-of-p))
        (link-new-node (list '&p
                             *feature-count*
                             name
                             (encode-dope)
                             (encode-ce-dope)
                             (cons 'progn (rest-of-p))))
        (putprop name (cdr (nreverse bakptrs)) 'backpointers )
	(putprop name matrix 'production)
        (putprop name *last-node* 'topnode))) 

(defun rating-part (pnode) (cadr pnode)) 

(defun var-part (pnode) (car (cdddr pnode))) 

(defun ce-var-part (pnode) (cadr (cdddr pnode))) 

(defun rhs-part (pnode) (caddr (cdddr pnode))) 

(defun excise-p (name)
  (cond ((and (symbolp name) (get name 'topnode))
	 (printline (list name 'is 'excised))
         (setq *pcount* (1- *pcount*))
         (remove-from-conflict-set name)
         (kill-node (get name 'topnode))
         (setq *pnames* (delete name *pnames* :test #'eq))
	 (remprop name 'production)
	 (remprop name 'backpointers)
         (remprop name 'topnode)))) 

(defun kill-node (node)
  (prog nil
   top  (and (atom node) (return nil))
        (rplaca node '&old)
        (setq node (cdr node))
        (go top))) 

(defun cmp-prin nil
  (prog nil
        (setq *last-node* *first-node*)
        (cond ((null *last-branch*) (cmp-posce) (cmp-nobeta))
              ((eq (peek-lex) '-) (cmp-negce) (cmp-not))
              (t (cmp-posce) (cmp-and))))) 

(defun cmp-negce nil (lex) (cmp-ce)) 

(defun cmp-posce nil
  (setq *ce-count* (1+ *ce-count*))
  (cond ((eq (peek-lex) #\{) (cmp-ce+cevar))
        (t (cmp-ce)))) 

(defun cmp-ce+cevar nil
  (prog (z)
        (lex)
        (cond ((atom (peek-lex)) (cmp-cevar) (cmp-ce))
              (t (cmp-ce) (cmp-cevar)))
        (setq z (lex))
        (or (eq z #\}) (%error '|missing '}'| z)))) 

(defun new-subnum (k)
  (or (numberp k) (%error '|tab must be a number| k))
  (setq *subnum* (round k))) 

(defun incr-subnum nil (setq *subnum* (1+ *subnum*))) 

(defun cmp-ce nil
  (prog (z)
        (new-subnum 0.)
        (setq *cur-vars* nil)
        (setq z (lex))
        (and (atom z)
             (%error '|atomic conditions are not allowed| z))
        (prepare-sublex z)
   la   (and (end-of-ce) (return nil))
        (incr-subnum)
        (cmp-element)
        (go la))) 

(defun cmp-element nil
        (and (eq (peek-sublex) #\^) (cmp-tab))
        (cond ((eq (peek-sublex) '#\{) (cmp-product))
              (t (cmp-atomic-or-any))))

(defun cmp-atomic-or-any nil
        (cond ((eq (peek-sublex) '<<) (cmp-any))
              (t (cmp-atomic))))

(defun cmp-any nil
  (prog (a z)
        (sublex)
        (setq z nil)
   la   (cond ((end-of-ce) (%error '|missing '>>'| a)))
        (setq a (sublex))
        (cond ((not (eq '>> a)) (setq z (cons a z)) (go la)))
        (link-new-node (list '&any nil (current-field) z)))) 


(defun cmp-tab nil
  (prog (r)
        (sublex)
        (setq r (sublex))
        (setq r ($litbind r))
        (new-subnum r))) 

(defun $litbind (x)
  (prog (r)
        (cond ((and (symbolp x) (setq r (literal-binding-of x)))
               (return r))
              (t (return x))))) 

(defun get-bind (x)
  (prog (r)
        (cond ((and (symbolp x) (setq r (literal-binding-of x)))
               (return r))
              (t (return nil))))) 

(defun cmp-atomic nil
  (prog (test x)
        (setq x (peek-sublex))
        (cond ((eq x '=) (setq test 'eq) (sublex))
              ((eq x '<>) (setq test 'ne) (sublex))
              ((eq x '<) (setq test 'lt) (sublex))
              ((eq x '<=) (setq test 'le) (sublex))
              ((eq x '>) (setq test 'gt) (sublex))
              ((eq x '>=) (setq test 'ge) (sublex))
              ((eq x '<=>) (setq test 'xx) (sublex))
              (t (setq test 'eq)))
        (cmp-symbol test))) 

(defun cmp-product nil
  (prog (save)
        (setq save (rest-of-ce))
        (sublex)
   la   (cond ((end-of-ce)
               (cond ((member #\} save) 
		      (%error '|wrong contex for '}'| save))
		     (t (%error '|missing '}'| save))))
              ((eq (peek-sublex) #\}) (sublex) (return nil)))
        (cmp-atomic-or-any)
        (go la))) 

(defun variablep (x) (and (symbolp x) (char-equal (char (symbol-name x) 0) #\<))) 

(defun cmp-symbol (test)
  (prog (flag)
        (setq flag t)
        (cond ((eq (peek-sublex) '//) (sublex) (setq flag nil)))
        (cond ((and flag (variablep (peek-sublex)))
               (cmp-var test))
              ((numberp (peek-sublex)) (cmp-number test))
              ((symbolp (peek-sublex)) (cmp-constant test))
              (t (%error '|unrecognized symbol| (sublex)))))) 

(defun concat3(x y z)
   (intern (format nil "~s~s~s" x y z)))

(defun cmp-constant (test)
  (or (member test '(eq ne xx) )
      (%error '|non-numeric constant after numeric predicate| (sublex)))
  (link-new-node (list (concat3 't test 'a)
                       nil
                       (current-field)
                       (sublex)))) 


(defun cmp-number (test)
  (link-new-node (list (concat3 't test 'n)
                       nil
                       (current-field)
                       (sublex)))) 

(defun current-field nil (field-name *subnum*)) 

(defun field-name (num)
  (cond ((= num 1.) '*c1*)
        ((= num 2.) '*c2*)
        ((= num 3.) '*c3*)
        ((= num 4.) '*c4*)
        ((= num 5.) '*c5*)
        ((= num 6.) '*c6*)
        ((= num 7.) '*c7*)
        ((= num 8.) '*c8*)
        ((= num 9.) '*c9*)
        ((= num 10.) '*c10*)
        ((= num 11.) '*c11*)
        ((= num 12.) '*c12*)
        ((= num 13.) '*c13*)
        ((= num 14.) '*c14*)
        ((= num 15.) '*c15*)
        ((= num 16.) '*c16*)
        ((= num 17.) '*c17*)
        ((= num 18.) '*c18*)
        ((= num 19.) '*c19*)
        ((= num 20.) '*c20*)
        ((= num 21.) '*c21*)
        ((= num 22.) '*c22*)
        ((= num 23.) '*c23*)
        ((= num 24.) '*c24*)
        ((= num 25.) '*c25*)
        ((= num 26.) '*c26*)
        ((= num 27.) '*c27*)
        ((= num 28.) '*c28*)
        ((= num 29.) '*c29*)
        ((= num 30.) '*c30*)
        ((= num 31.) '*c31*)
        ((= num 32.) '*c32*)
        ((= num 33.) '*c33*)
        ((= num 34.) '*c34*)
        ((= num 35.) '*c35*)
        ((= num 36.) '*c36*)
        ((= num 37.) '*c37*)
        ((= num 38.) '*c38*)
        ((= num 39.) '*c39*)
        ((= num 40.) '*c40*)
        ((= num 41.) '*c41*)
        ((= num 42.) '*c42*)
        ((= num 43.) '*c43*)
        ((= num 44.) '*c44*)
        ((= num 45.) '*c45*)
        ((= num 46.) '*c46*)
        ((= num 47.) '*c47*)
        ((= num 48.) '*c48*)
        ((= num 49.) '*c49*)
        ((= num 50.) '*c50*)
        ((= num 51.) '*c51*)
        ((= num 52.) '*c52*)
        ((= num 53.) '*c53*)
        ((= num 54.) '*c54*)
        ((= num 55.) '*c55*)
        ((= num 56.) '*c56*)
        ((= num 57.) '*c57*)
        ((= num 58.) '*c58*)
        ((= num 59.) '*c59*)
        ((= num 60.) '*c60*)
        ((= num 61.) '*c61*)
        ((= num 62.) '*c62*)
        ((= num 63.) '*c63*)
        ((= num 64.) '*c64*)
        (t (%error '|condition is too long| (rest-of-ce))))) 


;;; Compiling variables
;
;
;
; *cur-vars* are the variables in the condition element currently 
; being compiled.  *vars* are the variables in the earlier condition
; elements.  *ce-vars* are the condition element variables.  note
; that the interpreter will not confuse condition element and regular
; variables even if they have the same name.
;
; *cur-vars* is a list of triples: (name predicate subelement-number)
; eg:		( (<x> eq 3)
;		  (<y> ne 1)
;		  . . . )
;
; *vars* is a list of triples: (name ce-number subelement-number)
; eg:		( (<x> 3 3)
;		  (<y> 1 1)
;		  . . . )
;
; *ce-vars* is a list of pairs: (name ce-number)
; eg:		( (ce1 1)
;		  (<c3> 3)
;		  . . . )

(defun var-dope (var) (assoc var *vars* :test #'eq))

(defun ce-var-dope (var) (assoc var *ce-vars* :test #'eq))

(defun cmp-var (test)
  (prog (old name)
        (setq name (sublex))
        (setq old (assoc name *cur-vars* :test #'eq))
        (cond ((and old (eq (cadr old) 'eq))
               (cmp-old-eq-var test old))
              ((and old (eq test 'eq)) (cmp-new-eq-var name old))
              (t (cmp-new-var name test))))) 

(defun cmp-new-var (name test)
  (setq *cur-vars* (cons (list name test *subnum*) *cur-vars*))) 

(defun cmp-old-eq-var (test old)
  (link-new-node (list (concat3 't test 's)
                       nil
                       (current-field)
                       (field-name (caddr old))))) 

(defun cmp-new-eq-var (name old)
  (prog (pred next)
        (setq *cur-vars* (delete old *cur-vars* :test #'eq))
        (setq next (assoc name *cur-vars* :test #'eq))
        (cond (next (cmp-new-eq-var name next))
              (t (cmp-new-var name 'eq)))
        (setq pred (cadr old))
        (link-new-node (list (concat3 't pred 's)
                             nil
                             (field-name (caddr old))
                             (current-field))))) 

(defun cmp-cevar nil
  (prog (name old)
        (setq name (lex))
        (setq old (assoc name *ce-vars* :test #'eq))
        (and old
             (%error '|condition element variable used twice| name))
        (setq *ce-vars* (cons (list name 0.) *ce-vars*)))) 

(defun cmp-not nil (cmp-beta '&not)) 

(defun cmp-nobeta nil (cmp-beta nil)) 

(defun cmp-and nil (cmp-beta '&and)) 

(defun cmp-beta (kind)
  (prog (tlist vdope vname vpred vpos old)
        (setq tlist nil)
   la   (and (atom *cur-vars*) (go lb))
        (setq vdope (car *cur-vars*))
        (setq *cur-vars* (cdr *cur-vars*))
        (setq vname (car vdope))
        (setq vpred (cadr vdope))
        (setq vpos (caddr vdope))
        (setq old (assoc vname *vars* :test #'eq))
        (cond (old (setq tlist (add-test tlist vdope old)))
              ((not (eq kind '&not)) (promote-var vdope)))
        (go la)
   lb   (and kind (build-beta kind tlist))
        (or (eq kind '&not) (fudge))
        (setq *last-branch* *last-node*))) 

(defun add-test (list new old)
  (prog (ttype lloc rloc)
	(setq *feature-count* (1+ *feature-count*))
        (setq ttype (concat3 't (cadr new) 'b))
        (setq rloc (encode-singleton (caddr new)))
        (setq lloc (encode-pair (cadr old) (caddr old)))
        (return (cons ttype (cons lloc (cons rloc list)))))) 


