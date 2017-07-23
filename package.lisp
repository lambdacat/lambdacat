;;;; package.lisp

(defpackage #:lambdacat
  (:use #:cl)
  (:export #:rlambda
	   #:rlet
	   #:aif
	   #:awhen
	   #:let-if
	   #:let-when
	   #:it
	   #:once-only
	   #:with-gensyms
	   #:range
	   #:collect-range
	   #:indexed-map
	   #:atom*->symbol
	   #:take
	   #:drop
	   #:group
	   #:fold
	   #:flatten
	   #:with-floats
	   #:with-escape
	   #:cons-new
	   #:and1
	   #:all
	   #:any))
