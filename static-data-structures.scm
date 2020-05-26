(module static-data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;;;;;

  (define-datatype type-environment type-environment?
    (empty-tenv)
    (extend-tenv
     (syms (list-of symbol?))
     (vals (list-of type?))
     (tenv type-environment?))
    (extend-tenv-with-self-and-super
     (self type?)
     (super-name symbol?)
     (tenv type-environment?)))

  (define init-tenv
    (lambda ()
      (empty-tenv)))

  (define apply-tenv
    (lambda (env search-sym)
      (cases type-environment env
        (empty-tenv ()
          (eopl:error 'apply-env "No type found for ~s" search-sym))
        (extend-tenv (bvars types saved-tenv)
          (cond
            ((location search-sym bvars) => (lambda (n) (list-ref types n)))
            (else
              (apply-tenv saved-tenv search-sym))))
        (extend-tenv-with-self-and-super (self-name super-name saved-tenv)
          (case search-sym
            ((%self) self-name)
            ((%super) super-name)
            (else (apply-tenv saved-tenv search-sym)))))))

  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms)) => (lambda (n) (+ n 1)))
        (else #f))))
  
  )