;;; nyacc/lex-min.scm

;; Copyright (C) 2015-2022,2025,2026 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file LICENSE included with the this distribution.

;;; Description:

;; A module providing procedures for constructing lexical analyzers.
;; This version is stripped down for MES use.

;;; Code:

(define-module (nyacc-cp lex-min)
  #:export (make-comm-skipper
            make-chseq-reader
            read-c-ident
            read-c-string
            read-c-mclit
            read-c-num
            make-ident-like-p
            cnumstr->scm
            chseq->canon-map chseq->canon-unmap
            filter-mt remove-mt map-mt
            c:ws c:if c:ir)
  #:use-module (nyacc-cp common-utils)
  #:use-module (nyacc-cp parser-utils))

;; @section Constructing Lexical Analyzers
;; The @code{lex} module provides a set of procedures to build lexical
;; analyzers.  The approach is to first build a set of @defn{readers} for 
;; MORE TO COME
;;
;; Readers are procecures that take one character (presumably from the
;; current-input-port) and determine try to make a match.   If a match is
;; made something is returned, with any lookaheads pushed back into the
;; input port.  If no match is made @code{#f} is returned and the input
;; argument is still the character to work on.
;;
;; Here are the procedures used:
;; @table @code

(define digit "0123456789")
(define ucase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define lcase "abcdefghijklmnopqrstuvwxyz")

;; C lexemes are popular so include those.
;;(define c:ws (list->char-set '(#\space #\tab #\newline #\return )))
(define c:ws char-set:whitespace)
(define c:if (let ((cs (char-set #\_))) ; ident, first char
               (string->char-set! ucase cs)
               (string->char-set! lcase cs)))
(define c:ir (string->char-set digit c:if)) ; ident, rest chars
(define c:nx (string->char-set "eE"))       ; number exponent
(define c:hx (string->char-set "abcdefABCDEF"))
(define c:sx (string->char-set "lLuU")) ; fixed suffix
(define c:fx (string->char-set "fFlL")) ; float suffix
(define c:px (string->char-set "rRhHkKlLuU")) ; fixed-point suffix
(define c:bx (string->char-set "pP"))   ; binary float suffix
(define c:cx (string->char-set "LuU"))  ; char prefix

(define char-hex?
  (let ((cs (string->char-set "0123456789abcdefABCDEF")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define char-oct? 
  (let ((cs (string->char-set "01234567")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define rls reverse-list->string)       ; reverse-list->string

;; @deffn {Procedure} make-ident-reader cs-first cs-rest => ch -> #f|string
;; For identifiers, given the char-set for first character and the char-set
;; for following characters, return a return a reader for identifiers.
;; The reader takes a character as input and returns @code{#f} or @code{string}.
;; This will generate exception on @code{#<eof>}.
;; @end deffn
(define (make-ident-reader cs-first cs-rest)
  (lambda (ch)
    (cond
     ((eof-object? ch) #f)
     ((char-set-contains? cs-first ch)
      (let loop ((chl (list ch)) (ch (read-char)))
        (cond
         ((eof-object? ch)
          (if (null? chl) #f
              (rls chl)))
         ((char-set-contains? cs-rest ch)
          (loop (cons ch chl) (read-char)))
         (else (unread-char ch)
               (rls chl)))))
     (else #f))))

;; @deffn {Procedure} make-ident-like-p ident-reader
;; Generate a predicate, from a reader, that determines if a string qualifies
;; as an identifier. 
;; @end deffn
;; Implementation may not be very efficient.
(define (make-ident-like-p reader)
  (lambda (s) (and (string? s)
                   (positive? (string-length s))
                   (with-input-from-string s
                     (lambda ()
                       (and (reader (read-char))
                            (eof-object? (read-char)))))
                   #t)))

;; @deffn {Procedure} read-c-ident ch => #f|string
;; If ident pointer at following char, else (if #f) ch still last-read.
;; @end deffn
(define read-c-ident (make-ident-reader c:if c:ir))

;; @deffn {Procedure} read-oct => 123 | #f
;; Read octal number, assuming @code{\0} have already been read.
;; Return integer.
;; @end deffn
(define read-oct
  (let ((cs:oct (string->char-set "01234567")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
        (cond
         ((eof-object? ch) cv)
         ;;((> n 3) (unread-char ch) cv)
         ((char-set-contains? cs:oct ch)
          (loop (+ (* 8 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
         (else (unread-char ch) cv))))))

;; @deffn {Procedure} read-hex => 123 | #f
;; Read hex number.  Assumes prefix (e.g., "0x" has already been read).
;; Returns integer.
;; @end deffn
(define read-hex
  (let ((cs:dig (string->char-set "0123456789"))
        (cs:uhx (string->char-set "ABCDEF"))
        (cs:lhx (string->char-set "abcdef")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
        (cond
         ((eof-object? ch) cv)
         ;;((> n 2) (unread-char ch) cv)
         ((char-set-contains? cs:dig ch)
          (loop (+ (* 16 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
         ((char-set-contains? cs:uhx ch)
          (loop (+ (* 16 cv) (- (char->integer ch) 55)) (read-char) (1+ n)))
         ((char-set-contains? cs:lhx ch)
          (loop (+ (* 16 cv) (- (char->integer ch) 87)) (read-char) (1+ n)))
         (else (unread-char ch) cv))))))

;; @deffn {Procedure} c-escape seed
;; After @code{\\} in a C string, read the rest of the sequence and cons
;; the character, if exists, with the seed (a list).  Remember that @code{\n}
;; should, and will, just return the seed.
;; @end deffn
(define (c-escape seed)
  (let* ((ch (read-char)))
    (case ch
      ((#\newline) seed)
      ((#\\) (cons #\\ seed))
      ((#\") (cons #\" seed))
      ((#\') (cons #\' seed))
      ((#\n) (cons #\newline seed))
      ((#\r) (cons #\return seed))
      ((#\b) (cons #\bs seed))
      ((#\t) (cons #\tab seed))
      ((#\f) (cons #\page seed))
      ((#\a) (cons #\bel seed))       ; guile 1.8 doesn't know #\alarm
      ((#\v) (cons #\vt seed))        ; guile 1.8 doesn't know #\vtab
      ((#\0) (cons (integer->char (read-oct)) seed))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (unread-char ch) (cons (integer->char (read-oct)) seed))
      ((#\x) (cons (integer->char (read-hex)) seed))
      (else (cons ch seed)))))

;; @deffn {Procedure} read-c-string ch => ($string . "foo")
;; Read a C-code string.  Output to code is @code{write} not @code{display}.
;; Return #f if @var{ch} is not @code{"}. @*
;; TODO: parse trigraphs
;; ??=->#, ??/->\, ??'->^, ??(->[, ??)->], ??~->|, ??<->{, ??>->}, ??-->~
;; and digraphs <:->[, :>->], <%->{ %>->} %:->#
;; @end deffn
(define* (read-c-string ch #:optional (tval '$string))
  (if (not (eq? ch #\")) #f
      (let loop ((cl '()) (ch (read-char)))
        (cond ((eq? ch #\\) (loop (c-escape cl) (read-char)))
              ((eq? ch #\") (if tval (cons tval (rls cl)) (rls cl)))
              (else (loop (cons ch cl) (read-char)))))))

(define (read-esc-char)
  (let ((c2 (read-char)))
    (case c2
      ((#\t) #\tab)               ; horizontal tab U+0009
      ((#\n) #\newline)           ; newline U+000A
      ((#\v) #\vtab)              ; verticle tab U+000B
      ((#\f) #\page)              ; formfeed U+000C
      ((#\r) #\return)            ; return U+000D
      ((#\a) #\alarm)             ; alert U+0007
      ((#\b) #\backspace)         ; backspace U+0008 not in guile 1.8
      ((#\0) (integer->char (read-oct))) ; octal
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)     ; octal
       (unread-char c2) (integer->char (read-oct)))
      ((#\x) (integer->char (read-hex))) ; hex
      ((#\\ #\' #\" #\? #\|) c2)
      (else (error "bad escape sequence" c2)))))

(define (wchar t)
  (case t ((#\L) '$chlit/L) ((#\u) '$chlit/u) ((#\U) '$chlit/U)))

(define (read-c-chlit ch)
  (cond
   ((char=? ch #\')
    (let* ((c1 (read-char))
           (sc (string (if (eqv? c1 #\\) (read-esc-char) c1))))
      (if (not (char=? #\' (read-char)))
          (throw 'nyacc-error "read-c-chlit: bad char literal"))
      (cons '$chlit sc)))
   ((char-set-contains? c:cx ch)
    (let ((c1 (read-char)))
      (cond
       ((eof-object? c1) #f)
       ((char=? c1 #\') (cons (wchar ch) (cdr (read-c-chlit c1))))
       (else (unread-char c1) #f))))
   (else #f)))

(define (read-c-mclit ch)
  (define (readit ch)
    (let loop ((chl '()) (ch ch))
      (cond
       ((char=? ch #\') (cons '$chlit (rls chl)))
       ((char=? ch #\\) (loop (cons (read-esc-char) chl) (read-char)))
       (else (loop (cons ch chl) (read-char))))))
  (cond
   ((char=? ch #\') (readit (read-char)))
   ((char-set-contains? c:cx ch)
    (let ((c1 (read-char)))
      (cond
       ((eof-object? c1) #f)
       ((char=? c1 #\') (cons (wchar ch) (cdr (read-c-chlit c1))))
       (else (unread-char c1) #f))))
   (else #f)))

;; @deffn {Procedure} read-c-num ch => #f|pair
;; Reader for numbers in C. Here @evar{pair} is of the form
;; @code{($fixed . "1")}, @code{($float . "1.0")},
;; @code{($fixpt . "1.0r")} or @code{($decfl . "1.0DD")}.
;; The above coorespond to integers (including decimal, hexidecimal,
;; octal and binary), fixed-point, floating point (including the
;; C hex format) and decimal floating point.
;; Binary format integers, with prefix @code{0b}, are not part of
;; C, but commonly supported by compilers.  This routine will generate
;; the exception @code{nyacc-error} for some read errors, e.g., bad
;; prefix.
;; @end deffn
(define (read-c-num ch)
  (define (unread-chl chl)
    (let lp ((chl chl))
      (unless (null? (cdr chl)) (unread-char (car chl)) (lp (cdr chl)))))
  (define (bad-sfx chl)
    (let ((num (rls chl)))
      (unread-chl chl)
      (throw 'nyacc-error "bad suffix on number: ~S" num)))
  (define (bad-xfl chl)
    (let ((num (rls chl)))
      (unread-chl chl)
      (throw 'nyacc-error "hex float requires exponent: ~S" num)))

  ;; chl: char list; ty: type; st: state; ch: next char
  ;; st= 10=beg; 20=int; 30=frac; 40=sgn; 50=exp; 60=sfx 70=end
  (let loop ((chl '()) (ty #f) (st 10) (ch ch))
    (case st
      ((10) ;; entry
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char))) 
        ((char-numeric? ch) (loop chl '$fixed 20 ch))
        ((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char))) 
        (else #f)))
      ((11) ;; got 0/fixed, allow x, b
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char=? ch #\.) (loop (cons ch chl) '$float 30 (read-char)))
        ((memq ch '(#\X #\x)) (loop (cons ch chl) ty 21 (read-char)))
        ((memq ch '(#\B #\b)) (loop (cons ch chl) ty 23 (read-char)))
        ((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) ty 61 (read-char)))
        ((memq ch '(#\L #\l)) (loop (cons ch chl) ty 63 (read-char)))
        (else (loop chl ty 70 ch))))
      ((12) ;; got `.' only
       (cond
        ((eof-object? ch) #f)
        ((char-numeric? ch) (loop (cons ch chl) '$float 30 (read-char)))
        (else (unread-char ch) #f)))
      ((20) ;; parse decimal integer part
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-numeric? ch)
         (loop (cons ch chl) ty 20 (read-char)))
        ((char=? ch #\.)
         (loop (cons #\. chl) '$float 30 (read-char)))
        ((memq ch '(#\E #\e)) ;; float exponent
         (loop (cons ch chl) '$float 40 (read-char)))
        ((memq ch '(#\U #\u)) ;; fixed suffix start
         (loop (cons ch chl) '$fixed 61 (read-char)))
        ((memq ch '(#\L #\l)) ;; fixed suffix start
         (loop (cons ch chl) '$fixed 63 (read-char)))
        ((memq ch '(#\K #\R #\k #\r)) ;; fixpt suffix
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl '$fixed 70 ch))))
      ((21) ;; parse hex, fixed or float
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-hex? ch) (loop (cons ch chl) ty 21 (read-char)))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
        ((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
        ((char=? ch #\.) (loop (cons ch chl) '$float 35 (read-char)))
        ((memq ch '(#\P #\p)) (loop (cons ch chl) '$float 40 (read-char)))
        (else (loop chl ty 70 ch))))
      ((22) ;; parse fixed octal              
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
        ((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
        (else (loop chl ty 70 ch))))
      ((23) ;; parse fixed binary 
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\0 #\1)) (loop (cons ch chl) ty 23 (read-char)))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
        ((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
        (else (loop chl ty 70 ch))))
      ((30) ;; parse float fractional part
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-numeric? ch) (loop (cons ch chl) ty 30 (read-char)))
        ((memq ch '(#\E #\e))
         (loop (cons ch chl) ty 40 (read-char)))
        ((memq ch '(#\F #\f))
         (loop (cons ch chl) ty 69 (read-char)))
        ((memq ch '(#\L #\l))
         (loop (cons ch chl) ty 31 (read-char)))
        ((memq ch '(#\U #\u))
         (loop (cons ch chl) '$fixpt 66 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        ((memq ch '(#\D #\d)) ;; decfl
         (loop (cons ch chl) ty 68 (read-char)))
        (else (loop chl ty 70 ch))))
      ((31) ;; got l/float, check for fixpt
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ((35) ;; parse float hex fraction, requires exponent
       (cond
        ((eof-object? ch) (throw 'nyacc-error "hex float requires exponent"))
        ((char-hex? ch) (loop (cons ch chl) ty 35 (read-char)))
        ((memq ch '(#\P #\p)) (loop (cons ch chl) ty 40 (read-char)))
        (else (bad-xfl chl))))
      ((40) ;; have float e|p, look for sign
       (cond
        ((eof-object? ch) (bad-sfx chl))
        ((memq ch '(#\+ #\-)) (loop (cons ch chl) ty 50 (read-char)))
        (else (loop chl ty 50 ch))))
      ((50) ;; parse rest of exponent
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-numeric? ch) (loop (cons ch chl) ty 50 (read-char)))
        ((memq ch '(#\F #\f)) (loop (cons ch chl) ty 69 (read-char)))
        ((memq ch '(#\L #\l)) (loop (cons ch chl) ty 65 (read-char)))
        ((memq ch '(#\B #\b)) (loop (cons ch chl) ty 51 (read-char)))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixpt 66 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        ((memq ch '(#\D #\d)) (loop (cons ch chl) '$decfl 68 (read-char)))
        (else (loop chl ty 70 ch))))
      ((51) ;; got B looking for BF16 suffix
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\F #\f)) (loop (cons ch chl) ty 69 (read-char)))
        (else (bad-sfx chl))))
      ;; suffixes
      ((61) ;; fixed/u
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\L #\l))
         (loop (cons ch chl) ty 62 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ((62) ;; fixed/ul
       (cond
        ((eof-object? ch) (loop chl '$fixed 72 ch))
        ((memq ch '(#\L #\l))
         (loop (cons ch chl) '$fixed 70 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ((63) ;; fixed/l
       (cond
        ((eof-object? ch) (loop chl '$fixed 72 ch))
        ((memq ch '(#\L #\l))
         (loop (cons ch chl) ty 64 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl '$fixed 70 ch))))
      ((64) ;; fixed/ll
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\U #\u)) (loop (cons ch chl) ty 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ((65) ;; float/l
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ((66) ;; fixpt/u
       (cond
        ((eof-object? ch) (bad-sfx chl))
        ((memq ch '(#\L #\l))
         (loop (cons ch chl) '$fixpt 67 (read-char)))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (bad-sfx chl))))
      ((67) ;; fixpt/ul
       (cond
        ((eof-object? ch) (bad-sfx chl))
        ((memq ch '(#\K #\R #\k #\r))
         (loop (cons ch chl) '$fixpt 70 (read-char)))
        (else (bad-sfx chl))))
      ((68) ;; got (d|D), look for dDfFlL or 0-9
       (cond
        ((eof-object? ch) (bad-sfx chl))
        ((memq ch '(#\D #\d #\F #\f #\L #\l))
         (loop (cons ch chl) '$decfl 72 ch))
        ((char-numeric? ch) (loop (cons ch chl) ty 69 (read-char)))
        (else (bad-sfx chl))))
      ((69) ;; special float suffixes: F128 F32x F16 BF16
       (cond
        ((eof-object? ch) (loop chl ty 72 ch))
        ((char-numeric? ch) (loop (cons ch chl) ty 69 (read-char)))
        ((memq ch '(#\X #\x)) (loop (cons ch chl) ty 70 (read-char)))
        (else (loop chl ty 70 ch))))
      ;; exit
      ((70) ;; check char
       (cond
        ((eof-object? ch) (cons ty (rls chl)))
        (else (loop chl ty 71 ch))))
      ((71) ;; pushback 1 char
       (unread-char ch) (cons ty (rls chl)))
      ((72) ;; return
       (cons ty (rls chl)))
      (else
       (unread-chl chl) (error "coding error")))))

;; @deffn {Procedure} cnumstr->scm C99-str => scm-str
;; Convert C number-string (e.g, @code{0x123LL}) to Scheme numbers-string
;; (e.g., @code{#x123}).
;; This probably belongs in @code{(nyacc lang util)}.
;; @end deffn
(define (cnumstr->scm str)
  (define (2- n) (1- (1- n)))
  (define (3- n) (1- (2- n)))
  (define (trim-rt st nd) ;; trim U, UL, ULL (and lowercase) from right
    (if (char-set-contains? c:sx (string-ref str (1- nd)))
        (if (char-set-contains? c:sx (string-ref str (2- nd)))
            (if (char-set-contains? c:sx (string-ref str (3- nd)))
                (substring str st (3- nd))
                (substring str st (2- nd)))
            (substring str st (1- nd)))
        (substring str st nd)))
  (let* ((nd (string-length str)))
    (if (< nd 2) str
        (if (char=? #\0 (string-ref str 0))
            (cond
             ((char=? #\x (string-ref str 1))
              (string-append "#x" (trim-rt 2 nd)))
             ((char=? #\X (string-ref str 1))
              (string-append "#x" (trim-rt 2 nd)))
             ((char=? #\b (string-ref str 1))
              (string-append "#b" (trim-rt 2 nd)))
             ((char-numeric? (string-ref str 1))
              (string-append "#o" (trim-rt 1 nd)))
             (else (trim-rt 0 nd)))
            (trim-rt 0 nd)))))

;;.@deffn {Procedure} si-map string-list ix => a-list
;; Convert list of strings to alist of char at ix and strings.
;; This is a helper for make-tree.
;; @end deffn
(define (si-map string-list ix)
  (let loop ((sal '()) (sl string-list))
    (cond
     ((null? sl) sal)
     ((= ix (string-length (car sl)))
      (loop (reverse (acons 'else (car sl) sal)) (cdr sl)))
     ((assq (string-ref (car sl) ix) sal) =>
      (lambda (pair)
        (set-cdr! pair (cons (car sl) (cdr pair)))
        (loop sal (cdr sl))))
     (else ;; Add (#\? . string) to alist.
      (loop (cons (cons (string-ref (car sl) ix) (list (car sl))) sal)
            (cdr sl))))))

(define (chseq->symbol cs)
  (let* ((iseq (string-fold (lambda (c s) (cons* (char->integer c) s)) '() cs))
         (guts (string-join (map (lambda (i) (number->string i 16)) iseq) "%"))
         (name (string-append "%" guts)))
    (string->symbol name)))

;; @deffn chseq->canon-map cst
;; @deffnx chseq->canon-unmap cst
;; The first procedure maps a char-sequence alist to one mapping the string
;; form (e.g., @code{"+="}) to a canonical form (e.g. @code{_2B_3D_}).
;; The second generates an alist with the canonical form keys and values
;; from @var{cst}.  So, for example, pairs from @var{cst} are transformed as
;; @example
;; '("+=" . 123) -> '("+=" . %2B%3D)  '(%2B%3D . 123)
;; @example
;; @end deffn
(define (chseq->canon-map cst)
  (map (lambda (pair) (cons (car pair) (chseq->symbol (car pair)))) cst))
(define (chseq->canon-unmap cst)
  "See chseq"
  (map (lambda (pair) (cons (chseq->symbol (car pair)) (cdr pair))) cst))

;;.@deffn {Procedure} make-tree strtab -> tree
;; This routine takes an alist of strings and symbols and makes a tree
;; that parses one char at a time and provide @code{'else} entry for
;; signaling sequence found.  That is, if @code{("ab" . 1)} is an entry
;; then a chseq-reader (see below) would stop at @code{"ab"} and
;; return @code{1}.
;; @end deffn
(define (make-tree strtab)
  (define (si-cnvt string-list ix)
    (map (lambda (pair)
           (if (pair? (cdr pair))
               (cons (car pair) (si-cnvt (cdr pair) (1+ ix)))
               (cons (car pair) (assq-ref strtab (cdr pair)))))
         (si-map string-list ix)))
  (si-cnvt (map car strtab) 0))

;; @deffn {Procedure} make-chseq-reader strtab
;; Given alist of pairs (string, token) return a function that eats chars
;; until (token . string) is returned or @code{#f} if no match is found.
;; @end deffn
(define (make-chseq-reader strtab)
  ;; This code works on the assumption that the else-part is always last
  ;; in the list of transitions.
  (let ((tree (make-tree strtab)))
    (lambda (ch)
      (let loop ((cl (list ch)) (node tree))
        (cond
         ((assq-ref node (car cl)) => ;; accept or shift next character
          (lambda (n)
            (if (eq? (caar n) 'else) ; if only else, accept, else read on
                (cons (cdar n) (rls cl))
                (loop (cons (read-char) cl) n))))
         ((assq-ref node 'else) => ; else exists, accept
          (lambda (tok)
            (unless (eof-object? (car cl)) (unread-char (car cl)))
            (cons tok (rls (cdr cl)))))
         (else ;; reject
          (let pushback ((cl cl))
            (unless (null? (cdr cl))
              (unless (eof-object? (car cl)) (unread-char (car cl)))
              (pushback (cdr cl))))
          #f))))))

(define* (make-comm-skipper comm-table #:key eat-newline)
  ;; see make-comm-reader
  (let ((tree (make-tree comm-table)))
    (lambda* (ch)
      (letrec
          ((scol (1- (port-column (current-input-port))))
           (mc-read-char
            (lambda ()
              (let ((ch (read-char)))
                (if (eqv? ch #\\)
                    (let ((ch (read-char)))
                      (if (eqv? ch #\newline)
                          (read-char)
                          (begin (unread-char ch) #\\)))
                    ch))))
           (match-beg
            (lambda (cl node)
              (cond
               ((eof-object? (car cl)) #f)
               ((assq-ref node (car cl)) =>
                (lambda (n) (match-beg (cons (mc-read-char) cl) n)))
               ((assq-ref node 'else) =>
                (lambda (res) (unread-char (car cl)) res))
               (else
                (let pushback ((cl cl))
                  (unless (null? (cdr cl))
                    (unread-char (car cl))
                    (pushback (cdr cl))))
                #f))))
           (find-end 
            (lambda (sl il ps px)
              (cond
               ((eq? px (string-length ps)) ; can Guile optimize this?
                (if (and (not eat-newline) (eq? #\newline (car sl)))
                    (unread-char #\newline))
                #t)
               ((null? il) (find-end sl (cons (mc-read-char) il) ps px))
               ((eof-object? (car il))
                (if (char=? (string-ref ps px) #\newline) #t
                    (throw 'nyacc-error "open comment")))
               ((eqv? (car il) (string-ref ps px))
                (find-end (cons (car il) sl) (cdr il) ps (1+ px)))
               (else
                (let ((il1 (append-reverse sl il)))
                  (find-end '() (cdr il1) ps 0)))))))
        (let ((ep (match-beg (list ch) tree))) ;; ep=end pattern (e.g., "*/")
          (if ep (find-end '() (list (mc-read-char)) ep 0) #f))))))

;; @deffn {Procedure} filter-mt p? al => al
;; Filter match-table based on cars of al.
;; @end deffn
(define (filter-mt p? al) (filter (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} remove-mt p? al => al
;; Remove match-table based on cars of al.
;; @end deffn
(define (remove-mt p? al) (remove (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} map-mt f al => al
;; Map cars of al.
;; @end deffn
(define (map-mt f al) (map (lambda (x) (cons (f (car x)) (cdr x))) al))

;; --- last line ---
