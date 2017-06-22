;;;; package.lisp

(defpackage #:lambdacat
  (:use #:cl)
  (:export #:rlambda
	   #:rlet
	   #:once-only
	   #:with-gensyms
	   #:range
	   #:collect-range
	   #:indexed-map
	   #:atom*->symbol
	   #:take
	   #:drop
	   #:group))

