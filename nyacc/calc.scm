(use-modules (nyacc lalr-min))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define calc-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start prog)
   (grammar
    (prog
     (stmt-list ($$ (tl->list $1))))
    (stmt-list
     (stmt ($$ (make-tl 'stmt-list $1)))
     (stmt-list stmt ($$ (tl-append $1 $2))))
    (stmt
     ("\n" ($$ `(empty-stmt)))
     (expr "\n" ($$ `(expr-stmt ,$1)))
     (assn "\n" ($$ `(assn-stmt ,$1))))
    (expr
     (expr "+" expr ($$ `(add ,$1 ,$3)))
     (expr "-" expr ($$ `(sub ,$1 ,$3)))
     (expr "*" expr ($$ `(mul ,$1 ,$3)))
     (expr "/" expr ($$ `(div ,$1 ,$3)))
     ($fixed ($$ `(num ,$1)))
     ($float ($$ `(num ,$1)))
     ($ident ($$ `(ident ,$1)))
     ("(" expr ")" ($$ $2)))
    (assn
     ($ident "=" expr ($$ `(assn (ident ,$1) ,$3)))))))

;; Build an automaton for the full language (i.e., list of statements).
;; This is hashed, so tokens are represented by integers.
(define calc-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine calc-spec))))

(define (gen-calc-files)
  (write-lalr-actions calc-mach "calc-act.scm" #:prefix "calc-")
  (write-lalr-tables calc-mach "calc-tab.scm" #:prefix "calc-"))

;; --- last line ---
