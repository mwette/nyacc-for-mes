;; nyacc-cp/common-utils.scm
;;

(define-module (nyacc-cp common-utils)
  #:export (sferr pperr)
  #:re-export (remove fold fold-right define-record-type)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module ((srfi srfi-1) #:select (remove fold fold-right))
  #:use-module ((srfi srfi-9) #:select (define-record-type)))

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))

(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; --- last line ---
