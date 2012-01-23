Article 1004 of comp.sources.unix:
Path: s.cc.purdue.edu!h.cc.purdue.edu!j.cc.purdue.edu!pur-ee!iuvax!inuxc!ihnp4!ptsfa!pyramid!decwrl!sun!pitstop!sundc!seismo!uunet!rsalz
From: rsalz@uunet.UU.NET (Rich Salz)
Newsgroups: comp.sources.unix
Subject: v12i019:  OPS5 in Common Lisp, Part04/05
Message-ID: <2262@uunet.UU.NET>
Date: 14 Oct 87 02:22:04 GMT
Organization: UUNET Communications Services, Arlington, VA
Lines: 1715
Approved: rs@uunet.UU.NET

Submitted-by: eric@dlcdev.UUCP (eric van tassell)
Posting-number: Volume 12, Issue 19
Archive-name: ops5/part04

;;; WM maintaining functions
;
; The order of operations in the following two functions is critical.
; add-to-wm order: (1) change wm (2) record change (3) match 
; remove-from-wm order: (1) record change (2) match (3) change wm
; (back will not restore state properly unless wm changes are recorded
; before the cs changes that they cause)  (match will give errors if 
; the thing matched is not in wm at the time)


(defun add-to-wm (wme override)
  (prog (fa z part timetag port)
    (setq *critical* t)
    (setq *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (setq *max-wm* *current-wm*))
    (setq *action-count* (1+ *action-count*))
    (setq fa (wm-hash wme))
    (or (member fa *wmpart-list* :test #'eq)
        (setq *wmpart-list* (cons fa *wmpart-list*)))
    (setq part (get fa 'wmpart*))
    (cond (override (setq timetag override))
          (t (setq timetag *action-count*)))
    (setq z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (setq *critical* nil)
    (cond ((and *in-rhs* *wtrace*)
           (setq port (trace-file))
           (terpri port)
           (princ '|=>wm: | port)
           (ppelm wme port)))
    (and *in-rhs* *mtrace* (setq *madeby* 
                                 (cons (cons wme *p-name*) *madeby*))))) 

; remove-from-wm uses eq, not equal to determine if wme is present

(defun remove-from-wm (wme)
  (prog (fa z part timetag port)
    (setq fa (wm-hash wme))
    (setq part (get fa 'wmpart*))
    (setq z (assoc wme part :test #'eq))
    (or z (return nil))
    (setq timetag (cdr z))
    (cond ((and *wtrace* *in-rhs*)
           (setq port (trace-file))
           (terpri port)
           (princ '|<=wm: | port)
           (ppelm wme port)))
    (setq *action-count* (1+ *action-count*))
    (setq *critical* t)
    (setq *current-wm* (1- *current-wm*))
    (record-change '<=wm timetag wme)
    (match nil wme)
    (putprop fa (delete z part :test #'eq) 'wmpart* )
    (setq *critical* nil))) 

; mapwm maps down the elements of wm, applying fn to each element
; each element is of form (datum . creation-time)

(defun mapwm (fn)
  (prog (wmpl part)
        (setq wmpl *wmpart-list*)
   lab1 (cond ((atom wmpl) (return nil)))
        (setq part (get (car wmpl) 'wmpart*))
        (setq wmpl (cdr wmpl))
        (mapc fn part)
        (go lab1))) 

(defmacro wm (&rest a) 
  `(progn
   (mapc (function (lambda (z) (terpri) (ppelm z t))) 
	(get-wm ',a))
  nil) )

(defun get-wm (z)
  (setq *wm-filter* z)
  (setq *wm* nil)
  (mapwm (function get-wm2))
  (prog2 nil *wm* (setq *wm* nil))) 

(defun get-wm2 (elem) 
 (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*))
	(setq *wm* (cons (car elem) *wm*)))))

(defun wm-hash (x)
  (cond ((not x) '<default>)
        ((not (car x)) (wm-hash (cdr x)))
        ((symbolp (car x)) (car x))
        (t (wm-hash (cdr x))))) 

(defun creation-time (wme)
  (cdr (assoc wme (get (wm-hash wme) 'wmpart*) :test #'eq))) 

(defun rehearse nil
  (prog nil
    (setq *old-wm* nil)
    (mapwm (function refresh-collect))
    (mapc (function refresh-del) *old-wm*)
    (mapc (function refresh-add) *old-wm*)
    (setq *old-wm* nil))) 

(defun refresh-collect (x) (setq *old-wm* (cons x *old-wm*))) 

(defun refresh-del (x) (remove-from-wm (car x))) 

(defun refresh-add (x) (add-to-wm (car x) (cdr x))) 

(defun trace-file ()
  (prog (port)
        (setq port t)
	(cond (*trace-file*
	       (setq port ($ofile *trace-file*))
	       (cond ((null port)
	              (%warn '|trace: file has been closed| *trace-file*)
		      (setq port t)))))
        (return port)))


;;; Basic functions for RHS evaluation

(defun eval-rhs (pname data)
  (prog (node port)
    (cond (*ptrace*
           (setq port (trace-file))
           (terpri port)
           (princ *cycle-count* port)
           (princ '|. | port)
           (princ pname port)
           (time-tag-print data port)))
    (setq *data-matched* data)
    (setq *p-name* pname)
    (setq *last* nil)
    (setq node (get pname 'topnode))
    (init-var-mem (var-part node))
    (init-ce-var-mem (ce-var-part node))
    (begin-record pname data)
    (setq *in-rhs* t)
    (eval (rhs-part node))
    (setq *in-rhs* nil)
    (end-record))) 

(defun time-tag-print (data port)
  (cond ((not (null data))
         (time-tag-print (cdr data) port)
         (princ '| | port)
         (princ (creation-time (car data)) port))))

(defun init-var-mem (vlist)
  (prog (v ind r)
        (setq *variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (gelm *data-matched* ind))
        (setq *variable-memory* (cons (cons v r) *variable-memory*))
        (go top))) 

(defun init-ce-var-mem (vlist)
  (prog (v ind r)
        (setq *ce-variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (ce-gelm *data-matched* ind))
        (setq *ce-variable-memory*
              (cons (cons v r) *ce-variable-memory*))
        (go top))) 

(defun make-ce-var-bind (var elem)
  (setq *ce-variable-memory*
        (cons (cons var elem) *ce-variable-memory*))) 

(defun make-var-bind (var elem)
  (setq *variable-memory* (cons (cons var elem) *variable-memory*))) 

(defun $varbind (x)
  (prog (r)
	(and (not *in-rhs*) (return x))
        (setq r (assoc x *variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return x))))) 

(defun get-ce-var-bind (x)
  (prog (r)
        (cond ((numberp x) (return (get-num-ce x))))
        (setq r (assoc x *ce-variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return nil))))) 

(defun get-num-ce (x)
  (prog (r l d)
        (setq r *data-matched*)
        (setq l (length r))
        (setq d (- l x))
        (and (> 0. d) (return nil))
   la   (cond ((null r) (return nil))
              ((> 1. d) (return (car r))))
        (setq d (1- d))
        (setq r (cdr r))
        (go la))) 


(defun build-collect (z)
  (prog (r)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((and r (listp r))
               ($value '\()
               (build-collect r)
               ($value '\)))
              ((eq r '\\) ($change (car z)) (setq z (cdr z)))
              (t ($value r)))
        (go la))) 

(defun unflat (x) (setq *rest* x) (unflat*)) 

(defun unflat* nil
  (prog (c)
        (cond ((atom *rest*) (return nil)))
        (setq c (car *rest*))
        (setq *rest* (cdr *rest*))
        (cond ((eq c '\() (return (cons (unflat*) (unflat*))))
              ((eq c '\)) (return nil))
              (t (return (cons c (unflat*))))))) 


(defun $change (x)
  (prog nil
        (cond ((and x (listp x)) (eval-function x)) ;modified to check for nil
              (t ($value ($varbind x)))))) 

(defun eval-args (z)
  (prog (r)
        (rhs-tab 1.)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((eq r #\^)
               (rhs-tab (car z))
               (setq r (cadr z))
               (setq z (cddr z))))
        (cond ((eq r '//) ($value (car z)) (setq z (cdr z)))
              (t ($change r)))
        (go la))) 


(defun eval-function (form)
  (cond ((not *in-rhs*)
	 (%warn '|functions cannot be used at top level| (car form)))
	(t (eval form))))


;;; Functions to manipulate the result array


(defun $reset nil
  (setq *max-index* 0)
  (setq *next-index* 1)) 

; rhs-tab implements the tab ('^') function in the rhs.  it has
; four responsibilities:
;	- to move the array pointers
;	- to watch for tabbing off the left end of the array
;	  (ie, to watch for pointers less than 1)
;	- to watch for tabbing off the right end of the array
;	- to write nil in all the slots that are skipped
; the last is necessary if the result array is not to be cleared
; after each use; if rhs-tab did not do this, $reset
; would be much slower.

(defun rhs-tab (z) ($tab ($varbind z)))

(defun $tab (z)
  (prog (edge next)
        (setq next ($litbind z))
        (and (floatp next) (setq next (round next)))
        (cond ((or (not (numberp next)) 
		   (> next *size-result-array*)
		   (> 1. next))
               (%warn '|illegal index after ^| next)
               (return *next-index*)))
        (setq edge (- next 1.))
        (cond ((> *max-index* edge) (go ok)))
   clear (cond ((== *max-index* edge) (go ok)))
        (putvector *result-array* edge nil)
        (setq edge (1- edge))
        (go clear)
   ok   (setq *next-index* next)
        (return next))) 

(defun $value (v)
  (cond ((> *next-index* *size-result-array*)
         (%warn '|index too large| *next-index*))
        (t
         (and (> *next-index* *max-index*)
              (setq *max-index* *next-index*))
         (putvector *result-array* *next-index* v)
         (setq *next-index* (1+ *next-index*))))) 

(defun use-result-array nil
  (prog (k r)
        (setq k *max-index*)
        (setq r nil)
   top  (and (== k 0.) (return r))
        (setq r (cons (getvector *result-array* k) r))
        (setq k (1- k))
        (go top))) 

(defun $assert nil
  (setq *last* (use-result-array))
  (add-to-wm *last* nil))

(defun $parametercount nil *max-index*)

(defun $parameter (k)
  (cond ((or (not (numberp k)) (> k *size-result-array*) (< k 1.))
	 (%warn '|illegal parameter number | k)
         nil)
        ((> k *max-index*) nil)
	(t (getvector *result-array* k))))


;;; RHS actions


(defmacro make(&rest z)
  `(prog nil
        ($reset)
        (eval-args ',z)
        ($assert))) 

(defmacro modify (&rest z)
  `(prog (old args)
        (setq args ',z)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'modify)
	       (return nil)))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|modify: first argument must be an element variable|
                        (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        ($reset)
   copy (and (atom old) (go fin))
        ($change (car old))
        (setq old (cdr old))
        (go copy)
   fin  (eval-args args)
        ($assert))) 

(defmacro bind (&rest z)
  `(prog (val)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'bind)
	       (return nil)))
    (cond ((< (length z) 1.)
           (%warn '|bind: wrong number of arguments to| ',z)
           (return nil))
          ((not (symbolp (car ',z)))
           (%warn '|bind: illegal argument| (car ',z))
           (return nil))
          ((= (length ',z) 1.) (setq val (gensym)))
          (t ($reset)
             (eval-args (cdr ',z))
             (setq val ($parameter 1.))))
    (make-var-bind (car ',z) val))) 

(defmacro cbind (&rest z)
  `(cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'cbind))
	((not (= (length ',z) 1.))
	 (%warn '|cbind: wrong number of arguments| ',z))
	((not (symbolp (car ',z)))
	 (%warn '|cbind: illegal argument| (car ',z)))
	((null *last*)
	 (%warn '|cbind: nothing added yet| (car ',z)))
	(t (make-ce-var-bind (car ',z) *last*)))) 

(defmacro oremove (&rest z)
  `(prog (old args)
        (setq args ',z)
	(and (not *in-rhs*)(return (top-level-remove args)))
   top  (and (atom args) (return nil))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|remove: argument not an element variable| (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        (go top))) 

(defmacro ocall (&rest z)
  `(prog (f)
	(setq f (car ',z))
        ($reset)
        (eval-args (cdr ',z))
        (funcall f))) 

(defmacro owrite (&rest z)
 `(prog (port max k x needspace)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'write)
	       (return nil)))
	($reset)
	(eval-args ',z)
	(setq k 1.)
	(setq max ($parametercount))
	(cond ((< max 1.)
	       (%warn '|write: nothing to print| ',z)
	       (return nil)))
	(setq port (default-write-file))
	(setq x ($parameter 1.))
	(cond ((and (symbolp x) ($ofile x)) 
	       (setq port ($ofile x))
	       (setq k 2.)))
        (setq needspace t)
   la   (and (> k max) (return nil))
	(setq x ($parameter k))
	(cond ((eq x '|=== C R L F ===|)
	       (setq needspace nil)
               (terpri port))
              ((eq x '|=== R J U S T ===|)
	       (setq k (+ 2 k))
	       (do-rjust ($parameter (1- k)) ($parameter k) port))
	      ((eq x '|=== T A B T O ===|)
	       (setq needspace nil)
	       (setq k (1+ k))
	       (do-tabto ($parameter k) port))
	      (t 
	       (and needspace (princ '| | port))
	       (setq needspace t)
	       (princ x port)))
	(setq k (1+ k))
	(go la))) 
	
(defun default-write-file ()
  (prog (port)
	(setq port t)
	(cond (*write-file*
	       (setq port ($ofile *write-file*))
	       (cond ((null port) 
		      (%warn '|write: file has been closed| *write-file*)
		      (setq port t)))))
        (return port)))

                                                                                                                                                                                                         
(defun do-rjust (width value port)
  (prog (size)
	(cond ((eq value '|=== T A B T O ===|)
	       (%warn '|rjust cannot precede this function| 'tabto)
               (return nil))
	      ((eq value '|=== C R L F ===|)
	       (%warn '|rjust cannot precede this function| 'crlf)
               (return nil))
	      ((eq value '|=== R J U S T ===|)
	       (%warn '|rjust cannot precede this function| 'rjust)
               (return nil)))
        (setq size (length (princ-to-string value )))
	(cond ((> size width)
	       (princ '| | port)
	       (princ value port)
	       (return nil)))
        (do k (- width size) (1- k) (not (> k 0)) (princ '| | port))
	(princ value port)))

(defun do-tabto (col port)
  (eval `(format ,port (concatenate 'string "~" (princ-to-string ,col) "T"))))

;  (prog (pos)
;	(setq pos (1+ (nwritn port)))
;	(cond ((> pos col)
;	       (terpri port)
;	       (setq pos 1)))
;	(do k (- col pos) (1- k) (not (> k 0)) (princ '| | port))
;	(return nil)))


(defun halt nil 
  (cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'halt))
	(t (setq *halt-flag* t)))) 

(defmacro build (&rest z)
  `(prog (r)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'build)
	       (return nil)))
        ($reset)
        (build-collect ',z)
        (setq r (unflat (use-result-array)))
        (and *build-trace* (funcall *build-trace* r))
        (compile-production (car r) (cdr r)))) 

(defun infile(file)
   (open file :direction :input))

(defun outfile(file)
   (open file :direction :output))

(defmacro openfile (&rest z)
  `(prog (file mode id)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 3.))
	       (%warn '|openfile: wrong number of arguments| ',z)
	       (return nil)))
	(setq id ($parameter 1))
	(setq file ($parameter 2))
	(setq mode ($parameter 3))
	(cond ((not (symbolp id))
	       (%warn '|openfile: file id must be a symbolic atom| id)
	       (return nil))
              ((null id)
               (%warn '|openfile: 'nil' is reserved for the terminal| nil)
               (return nil))
	      ((or ($ifile id)($ofile id))
	       (%warn '|openfile: name already in use| id)
	       (return nil)))
	(cond ((eq mode 'in) (putprop id  (infile file) 'inputfile))
	      ((eq mode 'out) (putprop id  (outfile file) 'outputfile))
	      (t (%warn '|openfile: illegal mode| mode)
		 (return nil)))
	(return nil)))

(defun $ifile (x) 
  (cond ((and x (symbolp x)) (get x 'inputfile))
        (t *standard-input*)))
 
(defun $ofile (x) 
  (cond ((and x (symbolp x)) (get x 'outputfile))
        (t *standard-output*)))


(defmacro closefile (&rest z)
  `(progn 
    ($reset)
    (eval-args ',z)
    (mapc (function closefile2) (use-result-array))))

(defun closefile2 (file)
  (prog (port)
	(cond ((not (symbolp file))
	       (%warn '|closefile: illegal file identifier| file))
	      ((setq port ($ifile file))
	       (close port)
	       (remprop file 'inputfile))
	      ((setq port ($ofile file))
	       (close port)
	       (remprop file 'outputfile)))
	(return nil)))

(defmacro default (&rest z)
  `(prog (file use)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 2.))
	       (%warn '|default: wrong number of arguments| ',z)
	       (return nil)))
	(setq file ($parameter 1))
	(setq use ($parameter 2))
	(cond ((not (symbolp file))
	       (%warn '|default: illegal file identifier| file)
	       (return nil))
	      ((not (member use '(write accept trace)))
	       (%warn '|default: illegal use for a file| use)
	       (return nil))
	      ((and (member use '(write trace)) 
		    (not (null file))
		    (not ($ofile file)))
	       (%warn '|default: file has not been opened for output| file)
	       (return nil))
	      ((and (eq use 'accept) 
		    (not (null file))
		    (not ($ifile file)))
	       (%warn '|default: file has not been opened for input| file)
	       (return nil))
	      ((eq use 'write) (setq *write-file* file))
	      ((eq use 'accept) (setq *accept-file* file))
	      ((eq use 'trace) (setq *trace-file* file)))
	(return nil)))



;;; RHS Functions

(defmacro accept (&rest z)
  `(prog (port arg)
	(cond ((> (length ',z) 1.)
	       (%warn '|accept: wrong number of arguments| ',z)
	       (return nil)))
	(setq port t)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|accept: file has been closed| *accept-file*)
		      (return nil)))))
	(cond ((= (length ',z) 1)
	       (setq arg ($varbind (car ',z)))
	       (cond ((not (symbolp arg))
	              (%warn '|accept: illegal file name| arg)
		      (return nil)))
	       (setq port ($ifile arg))
	       (cond ((null port) 
		      (%warn '|accept: file not open for input| arg)
		      (return nil)))))
        (cond ((= (tyipeek port) -1.)
	       ($value 'end-of-file)
	       (return nil)))
	(flat-value (read port)))) 

(defun flat-value (x)
  (cond ((atom x) ($value x))
        (t (mapc (function flat-value) x)))) 

(defun span-chars (x prt)
  (do ((ch (tyipeek prt) (tyipeek prt))) ((not (member ch x #'char-equal))) (read-char prt)))

(defmacro acceptline (&rest z)
  `(prog ( def arg port)
	(setq port t)
	(setq def ',z)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|acceptline: file has been closed| 
		             *accept-file*)
		      (return nil)))))
	(cond ((> (length def) 0)
	       (setq arg ($varbind (car def)))
	       (cond ((and (symbolp arg) ($ifile arg))
	              (setq port ($ifile arg))
		      (setq def (cdr def))))))
        (span-chars '(9. 41.) port)
	(cond ((member (tyipeek port) '(-1. 10.))
	       (mapc (function $change) def)
	       (return nil)))
   lp1	(flat-value (read port))
        (span-chars '(9. 41.) port)
	(cond ((not (member (tyipeek port) '(-1. 10.))) (go lp1)))))

(defmacro substr (&rest l)
  `(prog (k elm start end)
        (cond ((not (= (length ',l) 3.))
               (%warn '|substr: wrong number of arguments| ',l)
               (return nil)))
        (setq elm (get-ce-var-bind (car ',l)))
        (cond ((null elm)
               (%warn '|first argument to substr must be a ce var|
                        ',l)
               (return nil)))
        (setq start ($varbind (cadr ',l)))
	(setq start ($litbind start))
        (cond ((not (numberp start))
               (%warn '|second argument to substr must be a number|
                        ',l)
               (return nil)))
	;if a variable is bound to INF, the following
	;will get the binding and treat it as INF is
	;always treated.  that may not be good
        (setq end ($varbind (caddr ',l)))
        (cond ((eq end 'inf) (setq end (length elm))))
	(setq end ($litbind end))
        (cond ((not (numberp end))
               (%warn '|third argument to substr must be a number|
                        ',l)
               (return nil)))
        ;this loop does not check for the end of elm
        ;instead it relies on cdr of nil being nil
        ;this may not work in all versions of lisp
        (setq k 1.)
   la   (cond ((> k end) (return nil))
              ((not (< k start)) ($value (car elm))))
        (setq elm (cdr elm))
        (setq k (1+ k))
        (go la))) 


(defmacro compute (&rest z) `($value (ari ',z))) 

; arith is the obsolete form of compute
(defmacro arith (&rest z) `($value (ari ',z))) 

(defun ari (x)
  (cond ((atom x)
         (%warn '|bad syntax in arithmetic expression | x)
	 0.)
        ((atom (cdr x)) (ari-unit (car x)))
        ((eq (cadr x) '+)
         (+ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '-)
         (difference (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '*)
         (times (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '//)
         (/ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '\\)
         (mod (round (ari-unit (car x))) (round (ari (cddr x)))))
        (t (%warn '|bad syntax in arithmetic expression | x) 0.))) 

(defun ari-unit (a)
  (prog (r)
        (cond ((listp a) (setq r (ari a)))
              (t (setq r ($varbind a))))
        (cond ((not (numberp r))
               (%warn '|bad value in arithmetic expression| a)
               (return 0.))
              (t (return r))))) 

(defun genatom nil ($value (gensym))) 

(defmacro litval (&rest z)
  `(prog (r)
	(cond ((not (= (length ',z) 1.))
	       (%warn '|litval: wrong number of arguments| ',z)
	       ($value 0) 
	       (return nil))
	      ((numberp (car ',z)) ($value (car ',z)) (return nil)))
	(setq r ($litbind ($varbind (car ',z))))
	(cond ((numberp r) ($value r) (return nil)))
	(%warn '|litval: argument has no literal binding| (car ',z))
	($value 0)))


(defmacro rjust (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|rjust: wrong number of arguments| ',z)
               (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|rjust: illegal value for field width| val)
	       (return nil)))
        ($value '|=== R J U S T ===|)
	($value val)))


(defmacro crlf()
     ($value '|=== C R L F ===|))

(defmacro tabto (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|tabto: wrong number of arguments| ',z)
	       (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|tabto: illegal column number| ',z)
	       (return nil)))
        ($value '|=== T A B T O ===|)
	($value val)))



;;; Printing WM

(defmacro ppwm (&rest z)
  `(prog (next a avlist)
        (setq avlist ',z)
        (setq *filters* nil)
        (setq next 1.)
   l   (and (atom avlist) (go print))
        (setq a (car avlist))
        (setq avlist (cdr avlist))
        (cond ((eq a #\^)
               (setq next (car avlist))
               (setq avlist (cdr avlist))
               (setq next ($litbind next))
               (and (floatp next) (setq next (round next)))
               (cond ((or (not (numberp next))
                          (> next *size-result-array*)
                          (> 1. next))
                      (%warn '|illegal index after ^| next)
                      (return nil))))
              ((variablep a)
               (%warn '|ppwm does not take variables| a)
               (return nil))
              (t (setq *filters* (cons next (cons a *filters*)))
                 (setq next (1+ next))))
        (go l)
   print (mapwm (function ppwm2))
        (terpri)
        (return nil))) 

(defun ppwm2 (elm-tag)
  (cond ((filter (car elm-tag)) (terpri) (ppelm (car elm-tag) t)))) 

(defun filter (elm)
  (prog (fl indx val)
        (setq fl *filters*)
   top  (and (atom fl) (return t))
        (setq indx (car fl))
        (setq val (cadr fl))
        (setq fl (cddr fl))
        (and (ident (nth (1- indx) elm) val) (go top))
        (return nil))) 

(defun ident (x y)
  (cond ((eq x y) t)
        ((not (numberp x)) nil)
        ((not (numberp y)) nil)
        ((=alg x y) t)
        (t nil))) 

; the new ppelm is designed especially to handle literalize format
; however, it will do as well as the old ppelm on other formats

(defun ppelm (elm port)
  (prog (ppdat sep val att mode lastpos)
	(princ (creation-time elm) port)
	(princ '|:  | port)
        (setq mode 'vector)
	(setq ppdat (get (car elm) 'ppdat))
	(and ppdat (setq mode 'a-v))
	(setq sep '|(|)
        (setq lastpos 0)
	(do
	 ((curpos 1 (1+ curpos)) (vlist elm (cdr vlist)))
	 ((atom vlist) nil)
	 (setq val (car vlist))
	 (setq att (assoc curpos ppdat))
	 (cond (att (setq att (cdr att)))
	       (t (setq att curpos)))
         (and (symbolp att) (is-vector-attribute att) (setq mode 'vector))
	 (cond ((or (not (null val)) (eq mode 'vector))
		(princ sep port)
		(ppval val att lastpos port)
		(setq sep '|    |)
		(setq lastpos curpos))))
	(princ '|)| port)))

(defun ppval (val att lastpos port)
  (cond ((not (equal att (1+ lastpos)))
         (princ '^ port)
         (princ att port)
         (princ '| | port)))
  (princ val port))





1,filed,,
>From RELAY.CS.NET!cdaf%indiana.csnet  Tue Mar 17 23:37:15 1987 remote from mit-eddie
Received: by EDDIE.MIT.EDU (5.31/4.7) id AA21258; Tue, 17 Mar 87 23:36:18 EST
Message-Id: <8703180436.AA21258@EDDIE.MIT.EDU>
Received: from relay2.cs.net by RELAY.CS.NET id aa16041; 17 Mar 87 23:35 EST
Received: from indiana by RELAY.CS.NET id aa06643; 17 Mar 87 23:30 EST
Date: Tue, 17 Mar 87 19:20:14 est
From: "Charles A. Daffinger" <cdaf%indiana.csnet@RELAY.CS.NET>
To: dlcdev!eric@EDDIE.MIT.EDU
Subject: common lisp ops5 part 3

*** EOOH ***
>From RELAY.CS.NET!cdaf%indiana.csnet  Tue Mar 17 23:37:15 1987 remote from mit-eddie
Date: Tue, 17 Mar 87 19:20:14 est
From: "Charles A. Daffinger" <cdaf%indiana.csnet@RELAY.CS.NET>
To: dlcdev!eric@EDDIE.MIT.EDU
Subject: common lisp ops5 part 3

; File OPS5.common.3.lsp: part 3 of OPS5 in Common Lisp
; ----------


;;; WM maintaining functions
;
; The order of operations in the following two functions is critical.
; add-to-wm order: (1) change wm (2) record change (3) match 
; remove-from-wm order: (1) record change (2) match (3) change wm
; (back will not restore state properly unless wm changes are recorded
; before the cs changes that they cause)  (match will give errors if 
; the thing matched is not in wm at the time)


(defun add-to-wm (wme override)
  (prog (fa z part timetag port)
    (setq *critical* t)
    (setq *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (setq *max-wm* *current-wm*))
    (setq *action-count* (1+ *action-count*))
    (setq fa (wm-hash wme))
    (or (member fa *wmpart-list* :test #'eq)
        (setq *wmpart-list* (cons fa *wmpart-list*)))
    (setq part (get fa 'wmpart*))
    (cond (override (setq timetag override))
          (t (setq timetag *action-count*)))
    (setq z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (setq *critical* nil)
    (cond ((and *in-rhs* *wtrace*)
           (setq port (trace-file))
           (terpri port)
           (princ '|=>wm: | port)
           (ppelm wme port)))
    (and *in-rhs* *mtrace* (setq *madeby* 
                                 (cons (cons wme *p-name*) *madeby*))))) 

; remove-from-wm uses eq, not equal to determine if wme is present

(defun remove-from-wm (wme)
  (prog (fa z part timetag port)
    (setq fa (wm-hash wme))
    (setq part (get fa 'wmpart*))
    (setq z (assoc wme part :test #'eq))
    (or z (return nil))
    (setq timetag (cdr z))
    (cond ((and *wtrace* *in-rhs*)
           (setq port (trace-file))
           (terpri port)
           (princ '|<=wm: | port)
           (ppelm wme port)))
    (setq *action-count* (1+ *action-count*))
    (setq *critical* t)
    (setq *current-wm* (1- *current-wm*))
    (record-change '<=wm timetag wme)
    (match nil wme)
    (putprop fa (delete z part :test #'eq) 'wmpart* )
    (setq *critical* nil))) 

; mapwm maps down the elements of wm, applying fn to each element
; each element is of form (datum . creation-time)

(defun mapwm (fn)
  (prog (wmpl part)
        (setq wmpl *wmpart-list*)
   lab1 (cond ((atom wmpl) (return nil)))
        (setq part (get (car wmpl) 'wmpart*))
        (setq wmpl (cdr wmpl))
        (mapc fn part)
        (go lab1))) 

(defmacro wm (&rest a) 
  `(progn
   (mapc (function (lambda (z) (terpri) (ppelm z t))) 
	(get-wm ',a))
  nil) )

(defun get-wm (z)
  (setq *wm-filter* z)
  (setq *wm* nil)
  (mapwm (function get-wm2))
  (prog2 nil *wm* (setq *wm* nil))) 

(defun get-wm2 (elem) 
 (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*))
	(setq *wm* (cons (car elem) *wm*)))))

(defun wm-hash (x)
  (cond ((not x) '<default>)
        ((not (car x)) (wm-hash (cdr x)))
        ((symbolp (car x)) (car x))
        (t (wm-hash (cdr x))))) 

(defun creation-time (wme)
  (cdr (assoc wme (get (wm-hash wme) 'wmpart*) :test #'eq))) 

(defun rehearse nil
  (prog nil
    (setq *old-wm* nil)
    (mapwm (function refresh-collect))
    (mapc (function refresh-del) *old-wm*)
    (mapc (function refresh-add) *old-wm*)
    (setq *old-wm* nil))) 

(defun refresh-collect (x) (setq *old-wm* (cons x *old-wm*))) 

(defun refresh-del (x) (remove-from-wm (car x))) 

(defun refresh-add (x) (add-to-wm (car x) (cdr x))) 

(defun trace-file ()
  (prog (port)
        (setq port t)
	(cond (*trace-file*
	       (setq port ($ofile *trace-file*))
	       (cond ((null port)
	              (%warn '|trace: file has been closed| *trace-file*)
		      (setq port t)))))
        (return port)))


;;; Basic functions for RHS evaluation

(defun eval-rhs (pname data)
  (prog (node port)
    (cond (*ptrace*
           (setq port (trace-file))
           (terpri port)
           (princ *cycle-count* port)
           (princ '|. | port)
           (princ pname port)
           (time-tag-print data port)))
    (setq *data-matched* data)
    (setq *p-name* pname)
    (setq *last* nil)
    (setq node (get pname 'topnode))
    (init-var-mem (var-part node))
    (init-ce-var-mem (ce-var-part node))
    (begin-record pname data)
    (setq *in-rhs* t)
    (eval (rhs-part node))
    (setq *in-rhs* nil)
    (end-record))) 

(defun time-tag-print (data port)
  (cond ((not (null data))
         (time-tag-print (cdr data) port)
         (princ '| | port)
         (princ (creation-time (car data)) port))))

(defun init-var-mem (vlist)
  (prog (v ind r)
        (setq *variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (gelm *data-matched* ind))
        (setq *variable-memory* (cons (cons v r) *variable-memory*))
        (go top))) 

(defun init-ce-var-mem (vlist)
  (prog (v ind r)
        (setq *ce-variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (ce-gelm *data-matched* ind))
        (setq *ce-variable-memory*
              (cons (cons v r) *ce-variable-memory*))
        (go top))) 

(defun make-ce-var-bind (var elem)
  (setq *ce-variable-memory*
        (cons (cons var elem) *ce-variable-memory*))) 

(defun make-var-bind (var elem)
  (setq *variable-memory* (cons (cons var elem) *variable-memory*))) 

(defun $varbind (x)
  (prog (r)
	(and (not *in-rhs*) (return x))
        (setq r (assoc x *variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return x))))) 

(defun get-ce-var-bind (x)
  (prog (r)
        (cond ((numberp x) (return (get-num-ce x))))
        (setq r (assoc x *ce-variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return nil))))) 

(defun get-num-ce (x)
  (prog (r l d)
        (setq r *data-matched*)
        (setq l (length r))
        (setq d (- l x))
        (and (> 0. d) (return nil))
   la   (cond ((null r) (return nil))
              ((> 1. d) (return (car r))))
        (setq d (1- d))
        (setq r (cdr r))
        (go la))) 


(defun build-collect (z)
  (prog (r)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((and r (listp r))
               ($value '\()
               (build-collect r)
               ($value '\)))
              ((eq r '\\) ($change (car z)) (setq z (cdr z)))
              (t ($value r)))
        (go la))) 

(defun unflat (x) (setq *rest* x) (unflat*)) 

(defun unflat* nil
  (prog (c)
        (cond ((atom *rest*) (return nil)))
        (setq c (car *rest*))
        (setq *rest* (cdr *rest*))
        (cond ((eq c '\() (return (cons (unflat*) (unflat*))))
              ((eq c '\)) (return nil))
              (t (return (cons c (unflat*))))))) 


(defun $change (x)
  (prog nil
        (cond ((and x (listp x)) (eval-function x)) ;modified to check for nil
              (t ($value ($varbind x)))))) 

(defun eval-args (z)
  (prog (r)
        (rhs-tab 1.)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((eq r #\^)
               (rhs-tab (car z))
               (setq r (cadr z))
               (setq z (cddr z))))
        (cond ((eq r '//) ($value (car z)) (setq z (cdr z)))
              (t ($change r)))
        (go la))) 


(defun eval-function (form)
  (cond ((not *in-rhs*)
	 (%warn '|functions cannot be used at top level| (car form)))
	(t (eval form))))


;;; Functions to manipulate the result array


(defun $reset nil
  (setq *max-index* 0)
  (setq *next-index* 1)) 

; rhs-tab implements the tab ('^') function in the rhs.  it has
; four responsibilities:
;	- to move the array pointers
;	- to watch for tabbing off the left end of the array
;	  (ie, to watch for pointers less than 1)
;	- to watch for tabbing off the right end of the array
;	- to write nil in all the slots that are skipped
; the last is necessary if the result array is not to be cleared
; after each use; if rhs-tab did not do this, $reset
; would be much slower.

(defun rhs-tab (z) ($tab ($varbind z)))

(defun $tab (z)
  (prog (edge next)
        (setq next ($litbind z))
        (and (floatp next) (setq next (round next)))
        (cond ((or (not (numberp next)) 
		   (> next *size-result-array*)
		   (> 1. next))
               (%warn '|illegal index after ^| next)
               (return *next-index*)))
        (setq edge (- next 1.))
        (cond ((> *max-index* edge) (go ok)))
   clear (cond ((== *max-index* edge) (go ok)))
        (putvector *result-array* edge nil)
        (setq edge (1- edge))
        (go clear)
   ok   (setq *next-index* next)
        (return next))) 

(defun $value (v)
  (cond ((> *next-index* *size-result-array*)
         (%warn '|index too large| *next-index*))
        (t
         (and (> *next-index* *max-index*)
              (setq *max-index* *next-index*))
         (putvector *result-array* *next-index* v)
         (setq *next-index* (1+ *next-index*))))) 

(defun use-result-array nil
  (prog (k r)
        (setq k *max-index*)
        (setq r nil)
   top  (and (== k 0.) (return r))
        (setq r (cons (getvector *result-array* k) r))
        (setq k (1- k))
        (go top))) 

(defun $assert nil
  (setq *last* (use-result-array))
  (add-to-wm *last* nil))

(defun $parametercount nil *max-index*)

(defun $parameter (k)
  (cond ((or (not (numberp k)) (> k *size-result-array*) (< k 1.))
	 (%warn '|illegal parameter number | k)
         nil)
        ((> k *max-index*) nil)
	(t (getvector *result-array* k))))


;;; RHS actions


(defmacro make(&rest z)
  `(prog nil
        ($reset)
        (eval-args ',z)
        ($assert))) 

(defmacro modify (&rest z)
  `(prog (old args)
        (setq args ',z)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'modify)
	       (return nil)))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|modify: first argument must be an element variable|
                        (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        ($reset)
   copy (and (atom old) (go fin))
        ($change (car old))
        (setq old (cdr old))
        (go copy)
   fin  (eval-args args)
        ($assert))) 

(defmacro bind (&rest z)
  `(prog (val)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'bind)
	       (return nil)))
    (cond ((< (length z) 1.)
           (%warn '|bind: wrong number of arguments to| ',z)
           (return nil))
          ((not (symbolp (car ',z)))
           (%warn '|bind: illegal argument| (car ',z))
           (return nil))
          ((= (length ',z) 1.) (setq val (gensym)))
          (t ($reset)
             (eval-args (cdr ',z))
             (setq val ($parameter 1.))))
    (make-var-bind (car ',z) val))) 

(defmacro cbind (&rest z)
  `(cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'cbind))
	((not (= (length ',z) 1.))
	 (%warn '|cbind: wrong number of arguments| ',z))
	((not (symbolp (car ',z)))
	 (%warn '|cbind: illegal argument| (car ',z)))
	((null *last*)
	 (%warn '|cbind: nothing added yet| (car ',z)))
	(t (make-ce-var-bind (car ',z) *last*)))) 

(defmacro oremove (&rest z)
  `(prog (old args)
        (setq args ',z)
	(and (not *in-rhs*)(return (top-level-remove args)))
   top  (and (atom args) (return nil))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|remove: argument not an element variable| (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        (go top))) 

(defmacro ocall (&rest z)
  `(prog (f)
	(setq f (car ',z))
        ($reset)
        (eval-args (cdr ',z))
        (funcall f))) 

(defmacro owrite (&rest z)
 `(prog (port max k x needspace)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'write)
	       (return nil)))
	($reset)
	(eval-args ',z)
	(setq k 1.)
	(setq max ($parametercount))
	(cond ((< max 1.)
	       (%warn '|write: nothing to print| ',z)
	       (return nil)))
	(setq port (default-write-file))
	(setq x ($parameter 1.))
	(cond ((and (symbolp x) ($ofile x)) 
	       (setq port ($ofile x))
	       (setq k 2.)))
        (setq needspace t)
   la   (and (> k max) (return nil))
	(setq x ($parameter k))
	(cond ((eq x '|=== C R L F ===|)
	       (setq needspace nil)
               (terpri port))
              ((eq x '|=== R J U S T ===|)
	       (setq k (+ 2 k))
	       (do-rjust ($parameter (1- k)) ($parameter k) port))
	      ((eq x '|=== T A B T O ===|)
	       (setq needspace nil)
	       (setq k (1+ k))
	       (do-tabto ($parameter k) port))
	      (t 
	       (and needspace (princ '| | port))
	       (setq needspace t)
	       (princ x port)))
	(setq k (1+ k))
	(go la))) 
	
(defun default-write-file ()
  (prog (port)
	(setq port t)
	(cond (*write-file*
	       (setq port ($ofile *write-file*))
	       (cond ((null port) 
		      (%warn '|write: file has been closed| *write-file*)
		      (setq port t)))))
        (return port)))

                                                                                                                                                                                                         
(defun do-rjust (width value port)
  (prog (size)
	(cond ((eq value '|=== T A B T O ===|)
	       (%warn '|rjust cannot precede this function| 'tabto)
               (return nil))
	      ((eq value '|=== C R L F ===|)
	       (%warn '|rjust cannot precede this function| 'crlf)
               (return nil))
	      ((eq value '|=== R J U S T ===|)
	       (%warn '|rjust cannot precede this function| 'rjust)
               (return nil)))
        (setq size (length (princ-to-string value )))
	(cond ((> size width)
	       (princ '| | port)
	       (princ value port)
	       (return nil)))
        (do k (- width size) (1- k) (not (> k 0)) (princ '| | port))
	(princ value port)))

(defun do-tabto (col port)
  (eval `(format ,port (concatenate 'string "~" (princ-to-string ,col) "T"))))

;  (prog (pos)
;	(setq pos (1+ (nwritn port)))
;	(cond ((> pos col)
;	       (terpri port)
;	       (setq pos 1)))
;	(do k (- col pos) (1- k) (not (> k 0)) (princ '| | port))
;	(return nil)))


(defun halt nil 
  (cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'halt))
	(t (setq *halt-flag* t)))) 

(defmacro build (&rest z)
  `(prog (r)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'build)
	       (return nil)))
        ($reset)
        (build-collect ',z)
        (setq r (unflat (use-result-array)))
        (and *build-trace* (funcall *build-trace* r))
        (compile-production (car r) (cdr r)))) 

(defun infile(file)
   (open file :direction :input))

(defun outfile(file)
   (open file :direction :output))

(defmacro openfile (&rest z)
  `(prog (file mode id)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 3.))
	       (%warn '|openfile: wrong number of arguments| ',z)
	       (return nil)))
	(setq id ($parameter 1))
	(setq file ($parameter 2))
	(setq mode ($parameter 3))
	(cond ((not (symbolp id))
	       (%warn '|openfile: file id must be a symbolic atom| id)
	       (return nil))
              ((null id)
               (%warn '|openfile: 'nil' is reserved for the terminal| nil)
               (return nil))
	      ((or ($ifile id)($ofile id))
	       (%warn '|openfile: name already in use| id)
	       (return nil)))
	(cond ((eq mode 'in) (putprop id  (infile file) 'inputfile))
	      ((eq mode 'out) (putprop id  (outfile file) 'outputfile))
	      (t (%warn '|openfile: illegal mode| mode)
		 (return nil)))
	(return nil)))

(defun $ifile (x) 
  (cond ((and x (symbolp x)) (get x 'inputfile))
        (t *standard-input*)))
 
(defun $ofile (x) 
  (cond ((and x (symbolp x)) (get x 'outputfile))
        (t *standard-output*)))


(defmacro closefile (&rest z)
  `(progn 
    ($reset)
    (eval-args ',z)
    (mapc (function closefile2) (use-result-array))))

(defun closefile2 (file)
  (prog (port)
	(cond ((not (symbolp file))
	       (%warn '|closefile: illegal file identifier| file))
	      ((setq port ($ifile file))
	       (close port)
	       (remprop file 'inputfile))
	      ((setq port ($ofile file))
	       (close port)
	       (remprop file 'outputfile)))
	(return nil)))

(defmacro default (&rest z)
  `(prog (file use)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 2.))
	       (%warn '|default: wrong number of arguments| ',z)
	       (return nil)))
	(setq file ($parameter 1))
	(setq use ($parameter 2))
	(cond ((not (symbolp file))
	       (%warn '|default: illegal file identifier| file)
	       (return nil))
	      ((not (member use '(write accept trace)))
	       (%warn '|default: illegal use for a file| use)
	       (return nil))
	      ((and (member use '(write trace)) 
		    (not (null file))
		    (not ($ofile file)))
	       (%warn '|default: file has not been opened for output| file)
	       (return nil))
	      ((and (eq use 'accept) 
		    (not (null file))
		    (not ($ifile file)))
	       (%warn '|default: file has not been opened for input| file)
	       (return nil))
	      ((eq use 'write) (setq *write-file* file))
	      ((eq use 'accept) (setq *accept-file* file))
	      ((eq use 'trace) (setq *trace-file* file)))
	(return nil)))



;;; RHS Functions

(defmacro accept (&rest z)
  `(prog (port arg)
	(cond ((> (length ',z) 1.)
	       (%warn '|accept: wrong number of arguments| ',z)
	       (return nil)))
	(setq port t)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|accept: file has been closed| *accept-file*)
		      (return nil)))))
	(cond ((= (length ',z) 1)
	       (setq arg ($varbind (car ',z)))
	       (cond ((not (symbolp arg))
	              (%warn '|accept: illegal file name| arg)
		      (return nil)))
	       (setq port ($ifile arg))
	       (cond ((null port) 
		      (%warn '|accept: file not open for input| arg)
		      (return nil)))))
        (cond ((= (tyipeek port) -1.)
	       ($value 'end-of-file)
	       (return nil)))
	(flat-value (read port)))) 

(defun flat-value (x)
  (cond ((atom x) ($value x))
        (t (mapc (function flat-value) x)))) 

(defun span-chars (x prt)
  (do ((ch (tyipeek prt) (tyipeek prt))) ((not (member ch x #'char-equal))) (read-char prt)))

(defmacro acceptline (&rest z)
  `(prog ( def arg port)
	(setq port t)
	(setq def ',z)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|acceptline: file has been closed| 
		             *accept-file*)
		      (return nil)))))
	(cond ((> (length def) 0)
	       (setq arg ($varbind (car def)))
	       (cond ((and (symbolp arg) ($ifile arg))
	              (setq port ($ifile arg))
		      (setq def (cdr def))))))
        (span-chars '(9. 41.) port)
	(cond ((member (tyipeek port) '(-1. 10.))
	       (mapc (function $change) def)
	       (return nil)))
   lp1	(flat-value (read port))
        (span-chars '(9. 41.) port)
	(cond ((not (member (tyipeek port) '(-1. 10.))) (go lp1)))))

(defmacro substr (&rest l)
  `(prog (k elm start end)
        (cond ((not (= (length ',l) 3.))
               (%warn '|substr: wrong number of arguments| ',l)
               (return nil)))
        (setq elm (get-ce-var-bind (car ',l)))
        (cond ((null elm)
               (%warn '|first argument to substr must be a ce var|
                        ',l)
               (return nil)))
        (setq start ($varbind (cadr ',l)))
	(setq start ($litbind start))
        (cond ((not (numberp start))
               (%warn '|second argument to substr must be a number|
                        ',l)
               (return nil)))
	;if a variable is bound to INF, the following
	;will get the binding and treat it as INF is
	;always treated.  that may not be good
        (setq end ($varbind (caddr ',l)))
        (cond ((eq end 'inf) (setq end (length elm))))
	(setq end ($litbind end))
        (cond ((not (numberp end))
               (%warn '|third argument to substr must be a number|
                        ',l)
               (return nil)))
        ;this loop does not check for the end of elm
        ;instead it relies on cdr of nil being nil
        ;this may not work in all versions of lisp
        (setq k 1.)
   la   (cond ((> k end) (return nil))
              ((not (< k start)) ($value (car elm))))
        (setq elm (cdr elm))
        (setq k (1+ k))
        (go la))) 


(defmacro compute (&rest z) `($value (ari ',z))) 

; arith is the obsolete form of compute
(defmacro arith (&rest z) `($value (ari ',z))) 

(defun ari (x)
  (cond ((atom x)
         (%warn '|bad syntax in arithmetic expression | x)
	 0.)
        ((atom (cdr x)) (ari-unit (car x)))
        ((eq (cadr x) '+)
         (+ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '-)
         (difference (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '*)
         (times (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '//)
         (/ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '\\)
         (mod (round (ari-unit (car x))) (round (ari (cddr x)))))
        (t (%warn '|bad syntax in arithmetic expression | x) 0.))) 

(defun ari-unit (a)
  (prog (r)
        (cond ((listp a) (setq r (ari a)))
              (t (setq r ($varbind a))))
        (cond ((not (numberp r))
               (%warn '|bad value in arithmetic expression| a)
               (return 0.))
              (t (return r))))) 

(defun genatom nil ($value (gensym))) 

(defmacro litval (&rest z)
  `(prog (r)
	(cond ((not (= (length ',z) 1.))
	       (%warn '|litval: wrong number of arguments| ',z)
	       ($value 0) 
	       (return nil))
	      ((numberp (car ',z)) ($value (car ',z)) (return nil)))
	(setq r ($litbind ($varbind (car ',z))))
	(cond ((numberp r) ($value r) (return nil)))
	(%warn '|litval: argument has no literal binding| (car ',z))
	($value 0)))


(defmacro rjust (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|rjust: wrong number of arguments| ',z)
               (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|rjust: illegal value for field width| val)
	       (return nil)))
        ($value '|=== R J U S T ===|)
	($value val)))


(defmacro crlf()
     ($value '|=== C R L F ===|))

(defmacro tabto (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|tabto: wrong number of arguments| ',z)
	       (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|tabto: illegal column number| ',z)
	       (return nil)))
        ($value '|=== T A B T O ===|)
	($value val)))



;;; Printing WM

(defmacro ppwm (&rest z)
  `(prog (next a avlist)
        (setq avlist ',z)
        (setq *filters* nil)
        (setq next 1.)
   l   (and (atom avlist) (go print))
        (setq a (car avlist))
        (setq avlist (cdr avlist))
        (cond ((eq a #\^)
               (setq next (car avlist))
               (setq avlist (cdr avlist))
               (setq next ($litbind next))
               (and (floatp next) (setq next (round next)))
               (cond ((or (not (numberp next))
                          (> next *size-result-array*)
                          (> 1. next))
                      (%warn '|illegal index after ^| next)
                      (return nil))))
              ((variablep a)
               (%warn '|ppwm does not take variables| a)
               (return nil))
              (t (setq *filters* (cons next (cons a *filters*)))
                 (setq next (1+ next))))
        (go l)
   print (mapwm (function ppwm2))
        (terpri)
        (return nil))) 

(defun ppwm2 (elm-tag)
  (cond ((filter (car elm-tag)) (terpri) (ppelm (car elm-tag) t)))) 

(defun filter (elm)
  (prog (fl indx val)
        (setq fl *filters*)
   top  (and (atom fl) (return t))
        (setq indx (car fl))
        (setq val (cadr fl))
        (setq fl (cddr fl))
        (and (ident (nth (1- indx) elm) val) (go top))
        (return nil))) 

(defun ident (x y)
  (cond ((eq x y) t)
        ((not (numberp x)) nil)
        ((not (numberp y)) nil)
        ((=alg x y) t)
        (t nil))) 

; the new ppelm is designed especially to handle literalize format
; however, it will do as well as the old ppelm on other formats

(defun ppelm (elm port)
  (prog (ppdat sep val att mode lastpos)
	(princ (creation-time elm) port)
	(princ '|:  | port)
        (setq mode 'vector)
	(setq ppdat (get (car elm) 'ppdat))
	(and ppdat (setq mode 'a-v))
	(setq sep '|(|)
        (setq lastpos 0)
	(do
	 ((curpos 1 (1+ curpos)) (vlist elm (cdr vlist)))
	 ((atom vlist) nil)
	 (setq val (car vlist))
	 (setq att (assoc curpos ppdat))
	 (cond (att (setq att (cdr att)))
	       (t (setq att curpos)))
         (and (symbolp att) (is-vector-attribute att) (setq mode 'vector))
	 (cond ((or (not (null val)) (eq mode 'vector))
		(princ sep port)
		(ppval val att lstpos port)
		(setq sep '|    |)
		(setq lastpos curpos))))
	(princ '|)| port)))

(defun ppval (val att lastpos port)
  (cond ((not (equal att (1+ lastpos)))
         (princ '^ port)
         (princ att port)
         (princ '| | port)))
  (princ val port))


