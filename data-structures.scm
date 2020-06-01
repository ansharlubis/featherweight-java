(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")
  (require "classes.scm")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

  ;;; an expressed value is an object
  (define-datatype expval expval?
    (obj-val
      (obj object?)))

  ;;; extractors
  (define expval->val
    (lambda (v)
      (cases expval v
        (obj-val (obj) obj))))
  
  (define expval->obj
    (lambda (v)
      (cases expval v
        (obj-val (obj) obj))))

  
  ;;;;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env
      (bvars (list-of symbol?))
      (bvals (list-of expval?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  (define env->list
    (lambda (env)
      (cases environment env
        (empty-env () '())
        (extend-env (sym val saved-env)
          (cons
            (list sym val)
            (env->list saved-env))))))
  
  )