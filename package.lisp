;;;; package.lisp

(defpackage #:lambdacat
  (:use #:cl)
  (:export #:rlambda
	   #:rlet
	   #:once-only
	   #:with-gensyms))

