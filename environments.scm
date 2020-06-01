(module environments (lib "eopl.ss" "eopl")

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env location)

  ;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  (define init-env
    (lambda ()
      (empty-env)))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "no binding for ~s" search-sym))
        (extend-env (bvars bvals saved-env)
          (cond
            ((location search-sym bvars)
             => (lambda (n) (list-ref bvals n)))
            (else
              (apply-env saved-env search-sym)))))))

  ;; location : Sym * Listof(Sym) -> Maybe(Int)
  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms))
         => (lambda (n) 
              (+ n 1)))
        (else #f))))
  
  )