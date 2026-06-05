;; nyacc-cp/common-utils.scm

;; Copyright (C) 2026 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file LICENSE included with the this distribution.

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
