;;;; semiliteral.lisp
;;;;
;;;; An alternative to the standard quasi-quote, which can be walked using
;;;; normal tree-walking techniques.

(in-package #:lambdacat)

(defun |~-reader| (stream ch)
  (declare (ignore ch))
  (list 'semiliteral (read stream)))

(set-macro-character #\~ #'|~-reader| t)

(defun |$-reader| (stream ch)
  (declare (ignore ch))
  (list 'escape-semiliteral (read stream)))

(set-macro-character #\$ #'|$-reader| t)

(defun |@-reader| (stream ch)
  (declare (ignore ch))
  (list 'escape-append-semiliteral (read stream)))

(set-macro-character #\@ #'|@-reader| t)

(defun escape? (expr)
  (and (consp expr)
       (eq (car expr) 'escape-semiliteral)))

(defun semiliteral? (expr)
  (and (consp expr)
       (eq (car expr) 'semiliteral)))

(defun escape-append? (expr)
  (and (consp expr)
       (eq (car expr) 'escape-append-semiliteral)))

(defun expand-semiliteral (expr depth)
  (assert (not (< depth 1))
	  (depth)
	  "Depth should never be < 1 in EXPAND-SEMILITERAL, got ~A"
	  depth)
  (cond ((escape? expr)
	 (if (= 1 depth)
	     (cadr expr)
	     (list 'escape-semiliteral
		   (expand-semiliteral (cadr expr) (- depth 1)))))
	((semiliteral? expr)
	 (list 'semiliteral
	       (expand-semiliteral (cadr expr) (+ depth 1))))
	((and (consp expr)
	      (escape-append? (car expr)))
	 (if (= 1 depth)
	     (list 'append
		   (cadar expr)
		   (expand-semiliteral (cdr expr) depth))
	     (cons (list 'escape-append-semiliteral
			 (expand-semiliteral (cadar expr) (- depth 1)))
		   (expand-semiliteral (cdr expr) depth))))
	((consp expr)
	 (list 'cons
	       (expand-semiliteral (car expr) depth)
	       (expand-semiliteral (cdr expr) depth)))
	((null expr) nil)
	((atom expr)
	 (list 'quote expr))))

(defmacro semiliteral (expr)
  (expand-semiliteral expr 1))
