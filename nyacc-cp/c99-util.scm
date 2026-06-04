;;; nyacc/c99-util.scm - C parser utilities

;; Copyright (C) 2015-2019,2022,2026 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (nyacc-cp c99-util)
  #:export (move-attributes attrl->attrs attrs->attrl extract-attr)
  #:use-module (ice-9 match)
  #:use-module (nyacc-cp tl-util)
  #:use-module (nyacc-cp sx-util)
  )

;; --- attributes ----------------------

(define (join-string-literal str-lit)
  (sx-list 'string (sx-attr str-lit) (string-join (sx-tail str-lit) "")))

;; used in c99-spec actions for attribute-specifiers
(define (attr-expr-list->string attr-expr-list)
  (string-append "(" (string-join (cdr attr-expr-list) ",") ")"))

;; ((attribute-list ...) (type-spec ...) (attribute-list ...)) =>
;;   (values (attribute-list ...)  ((type-spec ...) ...))

;; @deffn extract-attr tail => (values attr-tree tail)
;; Extract attributes from a sexp tail.
;; @end deffn
(define (extract-attr tail) ;; => (values attr-tree tail)
  (let loop ((atl '()) (tail1 '()) (tail0 tail))
    (cond
     ((null? tail0)
      (if (null? atl)
          (values '() tail)
          (values `(attribute-list . ,atl) (reverse tail1))))
     ((eq? 'attribute-list (sx-tag (car tail0)))
      (loop (append (sx-tail (car tail0)) atl) tail1 (cdr tail0)))
     (else
      (loop atl (cons (car tail0) tail1) (cdr tail0))))))

;; (attribute-list (attribute (ident "__packed__")) ...)
;;  =>
;; (attributes "__packed__;...")
;; OR
;; () => ()
(display "c99-util.scm: check attrl->attrs use of match vs sx-match\n")
(define (attrl->attrs attr-list)
  (define (spec->str spec)
    (match spec
      (`(ident ,name) name)
      (`(attribute ,name) (spec->str name))
      (`(attribute ,name ,args)
       (string-append (spec->str name) "(" (spec->str args) ")"))
      (`(attr-expr-list . ,expr-list)
       (string-join (map spec->str expr-list) ","))
      (`(fixed ,val) val)
      (`(float ,val) val)
      (`(char ,val) val)
      (`(string . ,val) (string-append "\"" (string-join val "") "\""))
      (`(type-name (decl-spec-list (type-spec ,spec))) (spec->str spec))
      (`(fixed-type ,name) name)
      (`(float-type ,name) name)
      ;;((mul ,lt ,rt) (c99
      (_ (let* ((sp (source-properties spec))
                 (fn (assq-ref sp 'filename))
                 (ln (assq-ref sp 'line))
                 (wm "warning: c99 attribute not processed"))
            (if sp (sferr "~a at ~a:~a\n" wm fn ln) (sferr "~a: ~s\n" wm spec))
            "MISSED"))))
  (if (null? attr-list) '()
      `(attributes ,(string-join (map spec->str (sx-tail attr-list)) ";"))))

;; @deffn {Procedure} move-attributes sexp
;; Given a sexpr, combine attribute-list kids and move to attribute ??
;; @example
;; (decl (decl-spec-list
;;         (attributes "__packed__" "__aligned__")
;;         (attributes "__alignof__(8)"))
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;;  =>
;; (decl (decl-spec-list
;;         (@ (attributes "__packed__;__aligned__;__alignof__(8)"))
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;; @end example
;; @end deffn
(define (move-attributes sexp)
  (let ((tag (sx-tag sexp)) (attr (sx-attr sexp)) (tail (sx-tail sexp)))
    (call-with-values (lambda () (extract-attr tail))
      (lambda (attrl stail)
        (sx-cons*
         tag 
         (cond
          ((null? attrl) attr)
          ((null? attr)`(@ ,(attrl->attrs attrl)))
          (else (append attr (list (attrl->attrs attrl)))))
         stail)))))

;; --- last line ---
