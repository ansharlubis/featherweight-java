(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  
  (require "lang.scm")
  (require "classes.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of instrument-let)

  ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program: Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (class-decls body)
          (initialize-class-env! class-decls)
          (value-of body (init-env))))))

  ;; value-of: Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (var-exp (var) (apply-env env var))

        (field-access-exp (exp1 f-name)
          (let ((obj (expval->val (value-of exp1 env))))
            (obj-val
              (access-field
                (find-field (object->class-name obj) f-name)
                obj))))

        (method-call-exp (obj-exp method-name rands)
          (let ((args (map expval->val (values-of-exps rands env)))
                (obj (expval->val (value-of obj-exp env))))
            (apply-method
              (find-method (object->class-name obj) method-name)
              obj
              args)))

        (new-object-exp (class-name rands)
          (let ((fields (map expval->val (values-of-exps rands env))))
            (obj-val
              (new-object
                class-name
                fields))))

        (cast-exp (exp1 class-name)
          (let ((objval (value-of exp1 env)))
            (if (is-subclass? (object->class-name (expval->obj objval)) class-name)
                objval
                (report-cast-error class-name objval))))

        )))

  (define access-field
    (lambda (loc obj)
      (list-ref (object->fields obj) loc)))
  
  (define apply-method
    (lambda (m self args)
      (cases method m
        (a-method (vars body super-name)
          (value-of body
            (extend-env vars args
              (extend-env 'this self (empty-env))))))))

  (define values-of-exps
    (lambda (exps env)
      (map
        (lambda (exp) (value-of exp env))
        exps)))

  ;; is-subclass?: ClassName * ClassName -> Bool
  (define is-subclass?
    (lambda (c-name1 c-name2)
      (if (eqv? c-name1 c-name2)
        #t
        (let ((s-name (class->super-name (lookup-class c-name1))))
          (if s-name
            (is-subclass? s-name c-name2)
            #f)))))

  ;;;;;;;;;;;;;;;;;; errors ;;;;;;;;;;;;;;;;;;

  (define report-cast-error
    (lambda (c-name obj)
      (eopl:error 'value-of "can't cast object to type ~s:~%~s" c-name obj)))

  )