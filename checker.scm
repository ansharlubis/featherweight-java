(module checker (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "static-classes.scm")
  (require "static-data-structures.scm")

  (provide type-of type-of-program)

  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (class-decls exp1)
          (initialize-static-class-env! class-decls)
          (for-each check-class-decl! class-decls)
          (type-of exp1 (init-tenv)))))
    )

  (define type-of
    (lambda (exp tenv)
      (cases expression exp

        (var-exp (var) (apply-tenv tenv var))

        (field-access-exp (exp f-name))

        (method-call-exp (obj-exp method-name rands))

        (new-object-exp (class-name rands))

        (cast-exp (exp class-name))

        ;; constructor's expressions

        (begin-exp (exp1 exps))

        (assign-exp (id rhs))

        (super-call-exp (method-name rands))
        
        )))
  
  )