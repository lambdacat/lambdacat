;;;; lambdacat.lisp

(in-package #:lambdacat)

;;; "lambdacat" goes here. Hacks and glory await!

(defun g!-symbol? (value)
  (and (symbolp value)
       (> (length (symbol-name value)) 2)
       (string= (symbol-name value)
		"G!"
		:end1 2)))

(defmacro defmacro/g! (name args &body body)
  (let ((g!* (remove-duplicates
	      (remove-if-not #'g!-symbol?
			     (flatten body)))))
    ~(defmacro $name $args
       (let $(mapcar (lambda (g)
		       ~($g (gensym $(subseq (symbol-name g) 2))))
		     g!*)
	 @body))))

(defun o!-symbol? (value)
  (and (symbolp value)
       (> (length (symbol-name value)) 2)
       (string= (symbol-name value)
		"O!"
		:end1 2)))

(defun o!-symbol->g!-symbol (o!-sym)
  (atom*->symbol 'g!
		 (subseq (symbol-name o!-sym) 2)))

(defmacro defmacro! (name args &body body)
  (let* ((o!* (remove-if-not #'o!-symbol?
			    (flatten args)))
	 (g!* (mapcar #'o!-symbol->g!-symbol o!*)))
    ~(defmacro/g! $name $args
       ~(let $(mapcar #'list (list @g!*) (list @o!*))
	  $(progn @body)))))

(defmacro! and1 (o!first &rest rest)
  ~(when (and $g!first @rest)
     $g!first))

(defmacro once-only (var* &body body)
  (let ((var->macro-gensym* (mapcar (lambda (var)
				      (cons var
					    (gensym (symbol-name var))))
				    var*)))
    ~(let $(mapcar (lambda (var->macro-gensym)
		     ~($(cdr var->macro-gensym)
			(gensym
			 $(symbol-name (car var->macro-gensym)))))
		   var->macro-gensym*)
       ~(let $(list @(mapcar (lambda (var->macro-gensym)
				~~($$(cdr var->macro-gensym)
				     $$(car var->macro-gensym)))
			      var->macro-gensym*))
	  $(let $(mapcar (lambda (var->macro-gensym)
			   ~($(car var->macro-gensym)
			      $(cdr var->macro-gensym)))
			 var->macro-gensym*)
	     @body)))))

;;; ----------------------------------------------------------------------

;;; RLAMBDA is like LAMBDA, except that it provides a name with which
;;; to make recursive calls.
;;;
;;; For example$ a recursive FACTORIAL can be defined as
;;;
;;;     (rlambda factorial (n)
;;;       (if (> 0 n)
;;;           (* n (factorial (- n 1))) 1))
;;;
;;; where the identifier FACTORIAL is only bound within the body of
;;; the RLAMBDA.
(defmacro rlambda (fn-name arg* &body body)
  ~(labels (($fn-name $arg* @body))
     #'$fn-name))

;;; ----------------------------------------------------------------------

;;; RLET is like LET, except that it provides a function which allows
;;; the body of the RLET to be entered recursively with the arguments
;;; bound to new values.
;;;
;;; For example, here is a definition of the REVERSE function which
;;; utilizes RLET
;;;
;;;     (defun reverse (ls)
;;;       (rlet rec ((result nil)
;;;                  (ls ls))
;;;         (if ls
;;;             (rec (cons (car ls) result)
;;;                  (cdr ls))
;;;             result)))
(defmacro rlet (fn-name bind* &body body)
  (let ((arg* (mapcar (lambda (bind)
			(if (consp bind)
			    (car bind)
			    bind))
		      bind*))
	(val* (mapcar (lambda (bind)
			(if (consp bind)
			    (cadr bind)
			    nil))
		      bind*)))
    ~(funcall (rlambda $fn-name $arg* @body)
	      @val*)))

;;; ----------------------------------------------------------------------

;;; WITH-GENSYMS is a shorthand for binding gensyms to variable names.
;;;
;;; For example$ the expression
;;;
;;;     (with-gensyms (a b c)
;;;       (list a b c))
;;;
;;; is equivalent to
;;;
;;;     (let ((a (gensym "a"))
;;;           (b (gensym "b"))
;;;           (c (gensym "c")))
;;;       (list a b c))
(defmacro with-gensyms (g* &body body)
  ~(let $(mapcar (lambda (g)
		   ~($g (gensym $(symbol-name g))))
		 g*)
     @body))

;;; ----------------------------------------------------------------------

;;; RANGE binds an identifier given by V to the integers in the range
;;; [LO, HI), and evaluates BODY for each.
(defmacro! range (v o!lo o!hi &body body)
  ~(rlet $g!rec (($v $g!lo))
     (if (< $v $g!hi)
	 (progn @body
		($g!rec (+ $v 1))))))

;;; ----------------------------------------------------------------------

;;; COLLECT-RANGE is like RANGE$ except that the value of BODY for
;;; each integer is collected in a list.
(defmacro! collect-range (v lo hi &body body)
  ~(let ($g!result)
     (range $v $lo $hi
       (push (progn @body) $g!result))
     (nreverse $g!result)))

;;; ----------------------------------------------------------------------

;;; INDEXED-MAP is like mapcar$ except that it passes the zero-based
;;; index of the list element as a second argument to FN.
;;;
;;; For example$ the expression
;;;
;;;     (indexed-map #'cons '(a b c d e f))
;;;
;;; yields the value
;;;
;;;     ((A . 0) (B . 1) (C . 2) (D . 3) (E . 4) (F . 5))
(defun indexed-map (fn ls)
  (let ((n 0))
    (mapcar (lambda (x)
	      (prog1 (funcall fn x n)
		(incf n)))
	    ls)))

;;; ----------------------------------------------------------------------

;;; Concatenate the printed representation of each atom in ATOM* into a
;;; a string$ and intern it as a symbol.
(defun atom*->symbol (&rest atom*)
  (values
   (intern
    (apply #'concatenate 'string (mapcar (lambda (atom)
					   (format nil "~A" atom))
					 atom*)))))

;;; ----------------------------------------------------------------------

;;; Return the first N elements of LS as a list.
;;;
;;;   (take 3 '(1 2 3 4 5 6 7 8)) => (1 2 3)
(defun take (n ls)
  (if (and ls (> n 0))
      (cons (car ls)
	    (take (- n 1) (cdr ls)))
      nil))

;;; ----------------------------------------------------------------------

;;; Return LS with the first N elements removed.
;;;
;;;   (drop 3 '(1 2 3 4 5 6 7 8)) => (4 5 6 7 8)
;;;
(defun drop (n ls)
  (if (and ls (> n 0))
      (drop (- n 1) (cdr ls))
      ls))

;;; ----------------------------------------------------------------------

;;; Group LS into sub-lists of size N.
;;;
;;;    (group 3 '(1 2 3 4 5 6 7 8)) => ((1 2 3) (4 5 6) (7 8))
;;;
(defun group (n ls)
  (if ls
      (cons (take n ls)
	    (group n (drop n ls)))
      nil))

(defmacro with-floats (f* &body body)
  ~(let $(mapcar (lambda (f)
		   ~($f (float $f)))
		 f*)
     @body))

(defmacro! let-if (var o!test then &body else)
  (assert (< (length else) 2) (else)
	  "Else clause must have 0 or 1 expression")
  ~(if $g!test
       (let (($var $g!test))
	 $then)
       @else))

(defmacro let-when (var test &body body)
  ~(let-if $var $test
	   (progn @body)))

(defmacro! aif (o!test then &optional else)
  ~(if $g!test
       (let ((it $g!test))
	 $then)
       $else))

(defmacro awhen (test &body body)
  ~(aif $test
	(progn @body)
	nil))

(defmacro! with-escape (esc-fn &body body)
  ~(block $g!block-name
     (labels (($esc-fn (val)
		(return-from $g!block-name val)))
       @body)))

(defun fold (fn init ls)
  (if ls
      (fold fn
	    (funcall fn init (car ls))
	    (cdr ls))
      init))

(defun flatten (tree)
  (nreverse
   (rlet rec (rev-atoms 
	      (tree tree))
     (cond ((consp tree) (rec (rec rev-atoms (car tree))
			      (cdr tree)))
	   ((null tree) rev-atoms)
	   (t (cons tree rev-atoms))))))

(defun cons-new (old car cdr)
  (if (and (eq (car old) car)
	   (eq (cdr old) cdr))
      old
      (cons car cdr)))

(defun all (pred ls)
  (rlet rec ((ls ls))
    (or (null ls)
	(and (funcall pred (car ls))
	     (rec (cdr ls))))))

(defun any (pred ls)
  (rlet rec ((ls ls))
    (and ls
	 (or (funcall pred (car ls))
	     (rec (cdr ls))))))
