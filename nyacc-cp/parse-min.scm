;;; nyacc/parse-min.scm - cut down version of (nyacc parse)

;; Copyright (C) 2014-2021,2026 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; procedures to generate parsers, given a lexical analyzer
;; one for files; one for interactive use: newline is possible end of input

;;; Code:

(define-module (nyacc-cp parse-min)
  #:export (make-lalr-parser/num)
  #:use-module (nyacc-cp common-utils)
  #:use-module (nyacc-cp parser-utils))

(define $default 1)                     ; sync w/ lalr-min.scm
(define $error 2)                       ; sync w/ lalr-min.scm

(define (wrap-action actn)              ; see util.scm
  (define (mkarg i) (string->symbol (string-append "$" (number->string i))))
  (define (make-arg-list n) (let loop ((r '(. $rest)) (i 1))
                              (if (> i n) r (loop (cons (mkarg i) r) (1+ i)))))
  (cons* 'lambda (make-arg-list (car actn)) (cdr actn)))

(define (make-xct av env)
  (if (procedure? (vector-ref av 0))
      av
      (vector-map (lambda (ix f)
                    (eval f (or env (current-module))))
                  (vector-map (lambda (ix actn) (wrap-action actn)) av))))

(define (dmsg/n s t a ntab)
  (let* ((t (or (assq-ref ntab t) t))
         (t (if (and (string? t) (> (string-length t) 40))
                (substring t 0 40) t)))
    (cond
     ((not a) (sferr "state ~S, token ~S\t=> parse error\n" s t))
     ((positive? a) (sferr "state ~S, token ~S  => shift, goto ~S\n" s t a))
     ((negative? a) (sferr "state ~S, token ~S  => reduce ~S\n" s t (- a)))
     ((zero? a) (sferr "state ~S, token ~S  => accept\n" s t))
     (else (error "coding error in (nyacc parse)")))))

(define (parse-error state laval)
  (let* ((sp (source-properties laval))
         (fn (or (assq-ref sp 'filename)
                 (port-filename (current-input-port))
                 "(unknown)"))
         (ln (or (assq-ref sp 'line)
                 (and=> (port-line (current-input-port)) 1+))))
    (throw 'nyacc-error
           "~A:~A: parse failed at state ~A, on input ~S"
           fn ln (car state) (cdr laval))))

(define* (make-lalr-parser/num mach
                               #:key
                               env
                               (skip-if-unexp '())
                               (user-hook (lambda (tal tok stk) #f)))
  (let* ((mtab (assq-ref mach 'mtab))
         (len-v (assq-ref mach 'len-v))
         (rto-v (assq-ref mach 'rto-v))
         (pat-v (assq-ref mach 'pat-v))
         (xct-v (make-xct (assq-ref mach 'act-v) env))
         (ntab (acons 1 '$default (assq-ref mach 'ntab)))
         (start (assq-ref (assq-ref mach 'mtab) '$start))
         (siu (map (lambda (n) (assoc-ref mtab n)) skip-if-unexp)))
    (lambda* (lexr #:key debug)
      (let loop ((state (list 0))       ; state stack
                 (stack (list '$@))     ; semantic value stack
                 (nval #f)              ; non-terminal from prev reduction
                 (lval #f))             ; lexical value (from lex'r)
        (cond
         ((not (or nval lval))
          (if (eqv? $default (caar (vector-ref pat-v (car state))))
              (loop state stack (cons $default #f) lval)
              (loop state stack nval (lexr)))) ; reload
         (else
          (let* ((laval (or nval lval))
                 (tval (car laval))
                 (sval (cdr laval))
                 (stal (vector-ref pat-v (car state)))
                 (stx (or (user-hook stal tval state)
                          (assq-ref stal tval)
                          (and (not (memq tval skip-if-unexp))
                               (assq-ref stal $default))
                          #f)))         ; error
            (if debug (dmsg/n (car state) (if nval tval sval) stx ntab))
            (cond
             ((eq? #f stx)              ; error
              (if (memq tval skip-if-unexp)
                  (loop state stack #f #f)
                  (parse-error state laval)))
             ((negative? stx)           ; reduce
              (let* ((gx (abs stx))
                     (gl (vector-ref len-v gx))
                     ($$ (apply (vector-ref xct-v gx) stack))
                     (tval (cons (vector-ref rto-v gx) $$)))
                (loop (list-tail state gl) (list-tail stack gl) tval lval)))
             ((positive? stx)           ; shift
              (loop (cons stx state) (cons sval stack) 
                    #f (if nval lval #f)))
             (else                      ; accept
              (car stack))))))))))

;;; --- last line ---
