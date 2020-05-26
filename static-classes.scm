(module static-classes (lib "eopl.ss" "eopl")

  (require "lang.scm")
  (require "static-data-structures.scm")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;; static classes ;;;;;;;;;;;;;;;;;;

  (define identifier? symbol?)

  (define method-tenv?
    (list-of
      (lambda (p)
        (and
          (pair? p)
          (symbol? (car p))
          (type? (cadr p))))))

  (define-datatype static-class static-class?
    (a-static-class
      (super-name (maybe identifier?))
      (field-names (list-of identifier?))
      (field-types (list-of type?))
      (method-tenv method-tenv?)))

  ;; method-tenv * id -> (maybe type)
  (define maybe-find-method-type
    (lambda (m-env id)
      (cond
        ((assq id m-env) => cadr)
        (else #f))))

  ;; class-name * id -> type OR fail
  (define find-method-type
    (lambda (class-name id)
      (let ((m (maybe-find-method-type
                (class-name->method-tenv class-name)
                id)))
        (if m m
            (eopl:error 'find-method "unknown method ~s in class ~s" id class-name)))))
  
  ;;;;;;;;;;;;;;;; the static class environment ;;;;;;;;;;;;;;;;

  ;; the-static-class-env will look like ((class-name static-class) ...)

  (define the-static-class-env '())

  (define is-static-class?
    (lambda (name)
      (or
        (assq name the-static-class-env)
        (equal? name 'object))))

  (define lookup-static-class
    (lambda (name)
      (cond
        ((assq name the-static-class-env) => (lambda (pair) (cadr pair)))
        (else
           (eopl:error 'lookup-static-class "unknown class: ~s" name)))))

  (define empty-the-static-class-env!
    (lambda ()
      (set! the-static-class-env '())))

  (define add-static-class-binding!
    (lambda (name sc)
      (set! the-static-class-env
        (cons
           (list name sc)
           the-static-class-env))))

  ;;;;;;;;;;;;;;;; class declarations, etc. ;;;;;;;;;;;;;;;;

  ;; first, pull out all the types and put them in
  ;; the-static-class-env.

  ;; initialize-static-class-env! : Listof(ClassDecl) -> Unspecified
  (define initialize-static-class-env!
    (lambda (c-decls)
      (empty-the-static-class-env!)
      (for-each add-class-decl-to-static-class-env! c-decls)))


  ;; add-class-decl-to-static-class-env! : ClassDecl -> Unspecified
  (define add-class-decl-to-static-class-env!
    (lambda (c-decl)
      (cases class-decl c-decl
        (a-class-decl (c-name s-name f-types f-names m-decls)
          (let ((super-f-names (class-name->field-names s-name))
                (super-f-types (class-name->field-types s-name)))
            (check-no-shadowing! super-f-names f-names c-name)
            (let ((f-names (append super-f-names f-names))
                  (f-types (append super-f-types f-types))
                  (method-tenv
                    (let ((local-method-tenv (method-decls->method-tenv m-decls)))
                      (check-no-dups! (map car local-method-tenv) c-name)
                      (merge-method-tenvs (class-name->method-tenv s-name) local-method-tenv))))
              (check-no-dups! f-names c-name)
              (check-for-initialize! method-tenv c-name)
              (add-static-class-binding! c-name
                (a-static-class (s-name f-names f-types method-tenv)))))))))

  (define method-decls->method-tenv
    (lambda (m-decls)
      (map
        (lambda (m-decl)
          (cases method-decl m-decl
            (a-method-decl (result-type m-name arg-ids arg-types body)
              (list m-name (proc-type arg-types result-type)))))
        m-decls)))

  ;; new methods override old ones.  
  (define merge-method-tenvs
    (lambda (super-tenv local-tenv)
      (append local-tenv super-tenv)))

  ;;;;;;;;;;;;;;;; extractors ;;;;;;;;;;;;;;;;

  (define class-name->field-names
    (lambda (c-name)
      (cond
        ((equal? c-name 'object) '())
        (else
          (static-class->field-names (lookup-static-class c-name))))))

  (define class-name->field-types
    (lambda (c-name)
      (cond
        ((equal? c-name 'object) '())
        (else
          (static-class->field-types (lookup-static-class c-name))))))

  (define class-name->method-tenv
    (lambda (c-name)
      (cond
        ((equal? 'object c-name) (eopl:error 'find-method "no method defined in object"))
        (else
          (static-class->method-tenv (lookup-static-class c-name))))))
  
  ;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

  (define static-class->super-name
    (lambda (sc)
      (cases static-class sc
        (a-static-class (s-name f-names f-types m-tenv)
          s-name))))

  (define static-class->field-names
    (lambda (sc)
      (cases static-class sc
        (a-static-class (s-name f-names f-types m-tenv)
          f-names))))

  (define static-class->field-types
    (lambda (sc)
      (cases static-class sc
        (a-static-class (s-name f-names f-types m-tenv)
          f-types))))

  (define static-class->method-tenv
    (lambda (sc)
      (cases static-class sc
        (a-static-class (s-name f-names f-types m-tenv)
          m-tenv))))

  (define check-for-initialize!
    (lambda (method-tenv class-name)
      (when (not (maybe-find-method-type method-tenv 'initialize))
        (eopl:error 'check-for-initialize!
          "no initialize method in class ~s"
          class-name))))
  
  ;; Listof(SchemeVal) * SchemeVal -> Unspecified
  (define check-no-dups!
    (lambda (lst blamee)
      (let loop ((rest lst))
        (cond
          ((null? rest) #t)
          ((memv (car rest) (cdr rest))
           (eopl:error 'check-no-dups! "duplicate found among ~s in class ~s" lst
             blamee))
          (else (loop (cdr rest)))))))
  
  (define check-no-shadowing!
    (lambda (super-f-names local-f-names blamee)
      (cond
        ((null? local-f-names) #t)
        ((memv (car local-f-names) super-f-names)
         (eopl:error 'check-no-shadowing! "field name ~s redeclared in class ~s"
           (car local-f-names) blamee))
        (else (check-no-shadowing! super-f-names (cdr local-f-names blamee))))))

  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))  
  )