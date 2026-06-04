;;; /nyacc/tl-util.scm - runtime utilities for the parsers

;; Copyright (C) 2015-2018,2025 Matthew Wette
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

(define-module (nyacc-cp tl-util)
  #:export (
            report-error
            *input-stack* push-input pop-input
            reset-input-stack ;; input-stack-portinfo
            make-tl tl->list
            tl-append tl-insert ;; tl-extend tl+attr tl+attr*
            ;;move-if-changed
            sferr pperr)
  ;;#:use-module ((srfi srfi-1) #:select (fold fold-right))
  #:use-module (ice-9 pretty-print))

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp . kw-args)
  (apply pretty-print exp (current-error-port) kw-args))


(define *input-stack* (make-parameter '()))

(define (reset-input-stack)
  (*input-stack* '()))

(define (push-input port)
  (let ((curr (current-input-port))
        (ipstk (*input-stack*)))
    (*input-stack* (cons curr ipstk))
    (set-current-input-port port)))

;; Return #f if empty
(define (pop-input)
  (let ((ipstk (*input-stack*)))
    (if (null? ipstk) #f
        (begin
          (close-port (current-input-port))
          (set-current-input-port (car ipstk))
          (*input-stack* (cdr ipstk))))))

;; @deffn {Procedure} input-stack-portinfo
;; Return a list of pairs of input stack filename and line number.
;; @end deffn
(define (input-stack-portinfo)
  (define (port-info port)
    (cons (or (port-filename port) "(unknown)") (1+ (port-line port))))
  (fold-right (lambda (port info) (cons (port-info port) info)) '()
              (*input-stack*)))


;; @deffn {Procedure} report-error fmt args
;; Report an error, to stderr, providing file and line num info, and add
;; newline.  This also reports context of parent files.
;; @end deffn
(define (report-error fmt args)
  (apply simple-format (current-error-port) (string-append fmt "\n") args)
  (for-each
   (lambda (pair)
     (simple-format (current-error-port) "  at ~A:~A\n" (car pair) (cdr pair)))
   (input-stack-portinfo)))


;; @section Tagged Lists
;; Tagged lists are
;; They are implemented as a cons cell with the car and the cdr a list.
;; The cdr is used to accumulate appended items and the car is used to
;; keep the tag, attributes and inserted items.
;; @example
;; tl => '(H . T), H => (c a b 'tag); T =>
;; @end example

;; @deffn {Procedure} make-tl tag [item item ...]
;; Create a tagged-list structure.
;; @end deffn
(define (make-tl tag . rest)
  (let loop ((tail tag) (l rest))
    (if (null? l) (cons '() tail)
        (loop (cons (car l) tail) (cdr l)))))

;; @deffn {Procedure} tl->list tl
;; Convert a tagged list structure to a list.  This collects added attributes
;; and puts them right after the (leading) tag, resulting in something like
;; @example
;; (<tag> (@@ <attr>) <rest>)
;; @end example
;; @end deffn
(define (tl->list tl)
  (let ((head (let loop ((head '()) (attr '()) (tl-head (car tl)))
                (if (null? tl-head)
                    (if (pair? attr)
                        (cons (cons '@ attr) (reverse head))
                        (reverse head))
                    (if (and (pair? (car tl-head)) (eq? '@ (caar tl-head)))
                        (loop head (cons (cdar tl-head) attr) (cdr tl-head))
                        (loop (cons (car tl-head) head) attr (cdr tl-head)))))))
    (let loop ((tail '()) (tl-tail (cdr tl)))
      (if (pair? tl-tail)
          (loop (cons (car tl-tail) tail) (cdr tl-tail))
          (cons tl-tail (append head tail))))))

;; @deffn {Procedure} tl-insert tl item
;; Insert item at front of tagged list (but after tag).
;; @end deffn
(define (tl-insert tl item)
  (cons (cons item (car tl)) (cdr tl)))

;; @deffn {Procedure} tl-append tl item ...
;; Append items at end of tagged list.
;; @end deffn
(define (tl-append tl . rest)
  (cons (car tl)
        (let loop ((tail (cdr tl)) (items rest))
          (if (null? items) tail
              (loop (cons (car items) tail) (cdr items))))))

;;; --- last line ---
