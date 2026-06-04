;; nyacc-cp/parser-utils.scm
;;

(define-module (nyacc-cp parser-utils)
  #:export (report-error
            *input-stack* push-input pop-input
            reset-input-stack
            make-tl tl->list
            tl-append tl-insert tl-extend
            sx-ref sx-tag sx-attr sx-tail sx-cons* sx-find sx-attr-add sx-list
            move-attributes attrl->attrs attrs->attrl extract-attr
            vector-map)
  #:re-export (append-reverse find match)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((srfi srfi-1) #:select (find append-reverse))
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module (nyacc-cp common-utils))

;; --- input stacks

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
  (let ((port (current-error-port)))
    (simple-format port fmt args)
    (newline port)
    (for-each
     (lambda (pair)
       (simple-format port "  at ~A:~A\n" (car pair) (cdr pair)))
     (input-stack-portinfo))))


;; --- sxml utilities

;; @deffn {Procedure} sx-tag sx => tag
;; Return the tag for a tree
;; @end deffn
(define (sx-tag sx)
  (if (pair? sx) (car sx) #f))

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
;; Where @var{tag} is a symbolic tag in the first level, find the first
;; matching element (in the first level).
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

(define (sx-has-attr? sx)
  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx)) #t))

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


;; --- tagged lists

(define (make-tl tag . rest)
  (let loop ((tail tag) (l rest))
    (if (null? l) (cons '() tail)
        (loop (cons (car l) tail) (cdr l)))))

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

(define (tl-insert tl item)
  (cons (cons item (car tl)) (cdr tl)))

(define (tl-append tl . rest)
  (cons (car tl)
        (let loop ((tail (cdr tl)) (items rest))
          (if (null? items) tail
              (loop (cons (car items) tail) (cdr items))))))

(define (tl-extend tl item-l)
  (apply tl-append tl item-l))


;; --- attributes 

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
