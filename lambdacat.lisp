;;;; lambdacat.lisp

(in-package #:lambdacat)

;;; "lambdacat" goes here. Hacks and glory await!

;;; This is a macro-yielding macro. It ensures that each variable in VAR* is only
;;; evaluated once in the expansion of the final macro.
(defmacro once-only (var* &body body)
  ;; VAR/GENSYM* is a list of variable/gensym pairs, where each gensym is the name
  ;; given to the corresponding variable in the final expanded macro.
  (let ((var/gensym* (mapcar (lambda (var)
			       (cons var
				     (gensym (symbol-name var))))
			     var*)))
    ;; The nested backticks are evaluated in the expansion of the final macro.
    ;; This assigns the evaluated contents of each var to the given gensym name,
    ;; effectively creating a temporary variable (to avoid redundant evaluation).
    ``(let ,(list ,@(mapcar (lambda (var/gensym)
			      (let ((v (car var/gensym))
				    (g (cdr var/gensym)))
				``(,',g ,,v)))
			    var/gensym*))
	;; This let statement is evaluated in the intermediate macro (where once-only
	;; is directly called). It shadows each variable (which previously evaluated
	;; to some form in the macro arguments) with the corresponding gensym.
	;;
	;; So now, if X previously evaluated to (FOO BAR), it will evaluate to a gensym
	;; which will have the value of (FOO BAR) assigned to it in the final macro (by
	;; the above let statement).
	,(let ,(mapcar (lambda (var/gensym)
			 (let ((v (car var/gensym))
			       (g (cdr var/gensym)))
			   `(,v ',g)))
		       var/gensym*)
	   ,@body))))

;;; ----------------------------------------------------------------------

;;; RLAMBDA is like LAMBDA, except that it provides a name with which to make
;;; recursive calls.
;;;
;;; For example, a recursive FACTORIAL can be defined as
;;;
;;;     (rlambda factorial (n)
;;;       (if (> 0 n)
;;;           (* n (factorial (- n 1))) 1))
;;;
;;; where the identifier FACTORIAL is only bound within the body of the RLAMBDA.
(defmacro rlambda (fn-name arg* &body body)
  `(labels ((,fn-name ,arg* ,@body))
     #',fn-name))

;;; ----------------------------------------------------------------------

;;; RLET is like LET, except that it provides a function which allows the body
;;; of the RLET to be entered recursively with the arguments bound to new values.
;;;
;;; For example, here is a definition of the REVERSE function which utilizes RLET
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
    `(funcall (rlambda ,fn-name ,arg* ,@body)
	      ,@val*)))

;;; ----------------------------------------------------------------------

;;; WITH-GENSYMS is a shorthand for binding gensyms to variable names.
;;;
;;; For example, the expression
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
  `(let ,(mapcar (lambda (g)
		   `(,g (gensym ,(symbol-name g))))
		 g*)
     ,@body))

;;; ----------------------------------------------------------------------

;;; RANGE binds an identifier given by V to the integers in the range [LO, HI),
;;; and evaluates BODY for each.
(defmacro range (v lo hi &body body)
  (once-only (lo hi)
    (with-gensyms (rec)
      `(rlet ,rec ((,v ,lo))
	 (if (< ,v ,hi)
	     (progn ,@body
		    (,rec (+ ,v 1))))))))

;;; ----------------------------------------------------------------------

;;; COLLECT-RANGE is like RANGE, except that the value of BODY for each integer
;;; is collected in a list.
(defmacro collect-range (v lo hi &body body)
  (with-gensyms (result)
    `(let (,result)
       (range ,v ,lo ,hi
	      (push (progn ,@body) ,result))
       (nreverse ,result))))

;;; ----------------------------------------------------------------------

;;; INDEXED-MAP is like mapcar, except that it passes the zero-based index of
;;; the list element as a second argument to FN.
;;;
;;; For example, the expression
;;;
;;;     (indexed-map #'cons '(a b c d e f))
;;;
;;; yields the value
;;;
;;;     ((a . 0) (b . 1) (c . 2) (d . 3) (e . 4) (f . 5))
(defun indexed-map (fn ls)
  (let ((n 0))
    (mapcar (lambda (x)
	      (prog1 (funcall fn x n)
		(incf n)))
	    ls)))
