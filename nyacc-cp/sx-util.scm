;;; /nyacc/sx-util.scm - runtime utilities for the parsers

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

;;; Notes:

;; The syntax of SXML trees is simple:
;; @example
;; expr => (tag item @dots{}) | (tag (@@ attr @dots{}) item @dots{})
;; item => string | expr
;; attr => (tag . string)
;; @end example


;;; Code:

(define-module (nyacc-cp sx-util)
  #:export (
            sx-ref sx-tail sx-cons* sx-find
            ;;make-sx
            ;;sx-tag sx-attr sx-tail sx-length sx-ref sx-ref*
            ;;sx-has-attr? sx-attr-ref sx-attr-add sx-attr-add* sx-attr-set!
            ;;sx-find
            ;;sx-split sx-split* sx-join sx-join* sx-cons sx-cons* sx-list
            ;;sx-cons*/src sx-list/src
            ;;sx-unitize
            ;;sx-match sx-match-tail
            )
  #:use-module ((srfi srfi-1) #:select (find fold fold-right append-reverse)))

;; @deffn {Procedure} sx-tag sx => tag
;; Return the tag for a tree
;; @end deffn
(define (sx-tag sx)
  (if (pair? sx) (car sx) #f))

;; @deffn {Procedure} sx-ref sx ix => item
;; Reference the @code{ix}-th element of the list, not counting the optional
;; attributes item.  If the list is shorter than the index, return @code{#f}.
;; [note to author: The behavior to return @code{#f} if no elements is not
;; consistent with @code{list-ref}.  Consider changing it.  Note also there
;; is never a danger of an element being @code{#f}.]
;; @example
;; (sx-ref '(abc 1) => #f
;; (sx-ref '(abc "def") 1) => "def"
;; (sx-ref '(abc (@ (foo "1")) "def") 1) => "def"
;; @end example
;; @end deffn
(define (sx-ref sx ix)
  (define (list-xref l x) (if (> (length l) x) (list-ref l x) #f))
  (cond
   ((zero? ix) (car sx))
   ((null? (cdr sx)) #f)
   ((and (pair? (cadr sx)) (eqv? '@ (caadr sx)))
    (list-xref sx (1+ ix)))
   (else
    (list-xref sx ix))))

;; @deffn {Procedure} sx-tail sx [ix] => (list)
;; Return the ix-th tail starting after the tag and attribut list, where
;; @var{ix} must be positive.  For example,
;; @example
;; (sx-tail '(tag (@ (abc . "123")) (foo) (bar)) 1) => ((foo) (bar))
;; @end example
;; Without second argument @var{ix} is 1.
;; @end deffn
(define sx-tail
  (case-lambda
   ((sx ix)
    (cond
     ((zero? ix) (error "sx-tail: expecting index greater than 0"))
     ((null? (cdr sx)) (list-tail sx ix))
     ((and (pair? (cadr sx)) (eqv? '@ (caadr sx))) (list-tail sx (1+ ix)))
     (else (list-tail sx ix))))
   ((sx)
    (sx-tail sx 1))))

(define (encode-attr attr)
  (cond
   ((not attr) #f)
   ((null? attr) #f)
   ((eq? '@ (car attr)) (if (null? (cdr attr)) #f attr))
   (else (cons '@ attr))))

;; @deffn {Procedure} sx-cons tag attr tail => sexp
;; @deffnx {Procedure} sx-cons* tag attr exp ... tail => sexp
;; @deffnx {Procedure} sx-list tag attr exp ... => sexp
;; Build an SXML element by its parts.  If @var{ATTR} is @code{#f},
;; @code{'()} or @code{(@)} it will not be included. 
;; @code{sx-cons*} and @code{sx-list} will remove any expressions @var{exp}
;; that are @code{#f} or @code{'()}.
;; @end deffn
(define (sx-cons tag attr tail)
  (let ((attr (encode-attr attr)))
    (if attr (cons* tag attr tail) (cons tag tail))))
(define (sx-cons* tag attr . rest)
  (let ((tail (let loop ((tail rest))
                (cond
                 ((null? (cdr tail)) (car tail))
                 ((not (car tail)) (loop (cdr tail)))
                 ((null? (car tail)) (loop (cdr tail)))
                 (else (cons (car tail) (loop (cdr tail))))))))
    (sx-cons tag attr tail)))
(define (sx-list tag attr . rest)
  (let ((tail (let loop ((tail rest))
                (cond
                 ((null? tail) '())
                 ((not (car tail)) (loop (cdr tail)))
                 ((null? (car tail)) (loop (cdr tail)))
                 (else (cons (car tail) (loop (cdr tail))))))))
    (sx-cons tag attr tail)))

;; @deffn {Procedure} sx-find tag sx => (tag ...)
;; @deffnx {Procedure} sx-find path sx => (tag ...)
;; In the first form @var{tag} is a symbolic tag in the first level.
;; Find the first matching element (in the first level).
;; In the second form, the argument @var{path} is a pair.  Apply sxpath
;; and take it's car,
;; if found, or return @code{#f}, like lxml's @code{tree.find()} method.
;; @* NOTE: the path version is currently disabled, to remove dependence
;; on the module @code{(sxml xpath)}.
;; @end deffn
(define (sx-find tag-or-path sx)
  (cond
   ((symbol? tag-or-path)
    (find (lambda (node)
            (and (pair? node) (eqv? tag-or-path (car node))))
          sx))
   ((pair? tag-or-path)
    (let ((sym (car tag-or-path)))
      (and=> (sx-find (car tag-or-path) sx)
             (lambda (sub-sx) (sx-find (cdr tag-or-path) sub-sx)))))
   ((null? tag-or-path)
    sx)
   (else
    (error "sx-find: expecting first arg to be tag or sxpath"))))

#|
;; === sx ==============================
;; @section SXML Utility Procedures
;; Some lot of these look like existing Guile list procedures (e.g.,
;; @code{sx-tail} versus @code{list-tail} but in sx lists the optional
;; attributea are `invisible'. For example, @code{'(elt (@abc) "d")}
;; is an sx of length two: the tag @code{elt} and the payload @code{"d"}.

;; @deffn {Procedure} sx-expr? expr
;; This predicate checks if @var{expr} looks like a valid SXML form.
;; It is not exhaustive: @var{expr} is checked to be a list with first
;; element a symbol.
;; @end deffn
(define (sxml-expr? sx)
  (and (pair? sx) (symbol? (car sx)) (list? sx)))

;; @deffn {Procedure} make-sx tag attr . elts
;; This will build an SXML expression from the symbolic tag, optional
;; attributes and elements.  The attributes @var{attr} can be of the from
;; @code{(@ (key "val") ...)} or @code{((key "val") ...)}.  If elements
;; in @var{elts} are not pairs or strings they are ignored, so elmeents of
;; @var{elts} of the form @code{#f} and @code{'()} will not end up in the
;; returned SXML form.
;; @end deffn
(define (make-sx tag attr . elts)
  (let ((tail (fold-right
               (lambda (elt sx)
                 (cond
                  ((pair? elt) (cons elt sx))
                  ((string? elt) (cons elt sx))
                  (else sx)))
               '() elts)))
    (if (pair? attr)
        (if (eq? '@ (car attr))
            (cons* tag attr tail)
            (cons* tag `(@ . ,attr) tail))
        (cons tag tail))))

;; @deffn {Procedure} sx-length sx => <int>
;; Return the length, don't include attributes, but do include tag
;; @end deffn
(define (sx-length sx)
  (let ((ln (length sx)))
    (cond
      ((zero? ln) 0)
      ((= 1 ln) 1)
      ((not (pair? (cadr sx))) ln)
      ((eq? '@ (caadr sx)) (1- ln))
      (else ln))))

;; @deffn {Procedure} sx-ref* sx ix1 ix2 ... => item
;; Equivalent to
;; @example
;; (((sx-ref (sx-ref sx ix1) ix2) ...) ...)
;; @end example
;; @end deffn
(define (sx-ref* sx . args)
  (fold (lambda (ix sx) (and (pair? sx) (sx-ref sx ix))) sx args))


;; @deffn {Procedure} sx-has-attr? sx
;; p to determine if @arg{sx} has attributes.
;; @end deffn
(define (sx-has-attr? sx)
  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx)) #t))

;; @deffn {Procedure} sx-attr sx => ((k v) ...)
;; @example
;; (sx-attr '(abc (@ (foo "1")) def) 1) => ((foo "1"))
;; @end example
;; @end deffn
(define (sx-attr sx)
  (if (and (pair? (cdr sx))
           (pair? (cadr sx))
           (eqv? '@ (caadr sx)))
      (cdadr sx)
      '()))

;; @deffn {Procedure} sx-attr-ref sx|node|tail| #f key => val
;; Return an attribute value given the key, or @code{#f}.
;; Also works if passed the attribute node @code{(@ ...)}, or its tail,
;; or @code{#f}.
;; @end deffn
(define (sx-attr-ref sx key)
  (let ((attr-tail (cond ((not sx) '())
                         ((null? sx) sx)
                         ((pair? (car sx)) sx)
                         ((eqv? '@ (car sx)) (car sx))
                         ((sx-attr sx))
                         (else '()))))
    (and=> (assq-ref attr-tail key) car)))

;; map to #f or '(@ ...)
(define (encode-attr attr)
  (cond
   ((not attr) #f)
   ((null? attr) #f)
   ((eq? '@ (car attr)) (if (null? (cdr attr)) #f attr))
   (else (cons '@ attr))))

;; @deffn {Procedure} sx-attr-add sx key-or-pair [val]
;; Add attribute to sx, passing either a key-val pair or key and val.
;; @end deffn
(define* (sx-attr-add sx key-or-pair #:optional val)
  (let* ((pair (if val (list key-or-pair val) key-or-pair))
         (key (car pair)) (val (cadr pair)))
    (cons
     (sx-tag sx)
     (if (sx-has-attr? sx)
         (cons `(@ pair
                   ,@(let loop ((atl (sx-attr sx)))
                       (cond ((null? atl) '())
                             ((eq? key (caar atl)) (loop (cdr atl)))
                             (else (cons (car atl) (loop (cdr atl)))))))
               (cddr sx))
         (cons `(@ ,pair) (cdr sx))))))

;; @deffn {Procedure} sx-attr-add* sx key val [key val [@dots{} ]] => sx
;; Add key-val pairs. @var{key} must be a symbol and @var{val} must be
;; a string.  Return a new @emph{sx}.
;; @end deffn
(define (sx-attr-add* sx . rest)
  (let* ((attrs (sx-attr sx))
         (attrs (let loop ((kvl rest))
                  (if (null? kvl) attrs
                      (cons (list (car kvl) (cadr kvl)) (loop (cddr kvl)))))))
    (cons* (sx-tag sx) (cons '@ attrs)
           (if (sx-has-attr? sx) (cddr sx) (cdr sx)))))

;; @deffn {Procedure} sx-attr-set! sx key val
;; Set attribute for sx.  If no attributes exist, if key does not exist,
;; add it, if it does exist, replace it.
;; @end deffn
(define (sx-attr-set! sx key val)
  (if (sx-has-attr? sx)
      (let ((attr (cadr sx)))
        (set-cdr! attr (assoc-set! (cdr attr) key (list val))))
      (set-cdr! sx (cons `(@ (,key ,val)) (cdr sx))))
  sx)

(define sx-join sx-cons)
(define sx-join* sx-list)

;; @deffn {Procedure} sx-cons*/src src tag attr exp ... tail => sexp
;; @deffnx {Procedure} sx-list/src src tag attr exp ... => sexp
;; These are like their respective procedures without the @code{/src}
;; but allow source properties to be transferred from the sx @var{src}.
;; @end deffn
(define (sx-cons*/src src tag attr . rest)
  (let ((sx (apply sx-cons* tag attr rest)))
    (cons-source src (car sx) (cdr sx))))
(define (sx-list/src src tag attr . rest)
  (let ((sx (apply sx-list tag attr rest)))
    (cons-source src (car sx) (cdr sx))))

;; @deffn {Procedure} sx-split sexp => tag attr tail
;; @deffnx {Procedure} sx-split* sexp => tag attr exp ...
;; Split an SXML element into its constituent parts, as a @code{values},
;; where @var{attr} is list of pairs.  If no attributes exist, @var{attr}
;; is @code{'()}.
;; @end deffn
(define (sx-split sexp)
  (let ((tag (sx-tag sexp))
        (attr (sx-attr sexp))
        (tail (sx-tail sexp)))
    (values tag attr tail)))
(define (sx-split* sexp)
  (let ((tag (sx-tag sexp))
        (attr (sx-attr sexp))
        (tail (sx-tail sexp)))
    (apply values tag attr tail)))

;; @deffn {Procedure} sx-unitize list-tag form seed
;; Given a declaration of form @code{(tag ... (elt-list ...) ...)}
;; fold into the seed broken list of
;; @code{(tag ... elt1 ...) (tag ... elt2 ...) ... seed}.
;; Any attributes for the list form are lost.
;; @end deffn
(define (sx-unitize list-tag form seed)
  (let loop ((head '()) (elts '()) (tail '()) (form form))
    (if (null? elts)
        (if (and (pair? (car form)) (eq? list-tag (sx-tag (car form))))
            (loop head (cdar form) (cdr form) '())
            (loop (cons (car form) head) elts tail (cdr form)))
        (let loop2 ((elts elts))
          (if (null? elts) seed
              (cons (append-reverse (cons (car elts) head) tail)
                    (loop2 (cdr elts))))))))

|#
;; --- last line ---
