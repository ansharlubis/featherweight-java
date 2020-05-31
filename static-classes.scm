(module static-classes (lib "eopl.ss" "eopl")

  (require "lang.scm")
  (require "static-data-structures.scm")

  (provide (all-defined-out))

;;;;;;;;;;;;;;;; method type environments ;;;;;;;;;;;;;;;;

  ;; a method tenv looks like ((method-name type) ...)
  ;; each method will have a proc-type

  ;;;;;;;;;;;;;;;; static classes ;;;;;;;;;;;;;;;;

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
                 (static-class->method-tenv (lookup-static-class class-name))
                 id)))
        (if m m
          (eopl:error 'find-method 
            "unknown method ~s in class ~s"
            id class-name)))))
  
  ;;;;;;;;;;;;;;;; the static class environment ;;;;;;;;;;;;;;;;

  ;; the-static-class-env will look like ((class-name static-class) ...)

  (define the-static-class-env '())

  (define is-static-class?
    (lambda (name)
      (assq name the-static-class-env)))

  (define lookup-static-class                    
    (lambda (name)
      (cond
        ((assq name the-static-class-env)
         => (lambda (pair) (cadr pair)))
        (else (eopl:error 'lookup-static-class
                "Unknown class: ~s" name)))))

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
  ;; Page: 362
  (define initialize-static-class-env!
    (lambda (c-decls)
      (empty-the-static-class-env!)
      (add-static-class-binding!
        'object (a-static-class #f '() '() '()))
      (for-each add-class-decl-to-static-class-env! c-decls)))

  ;; add-class-decl-to-static-class-env! : ClassDecl -> Unspecified
  ;; Page 366
  (define add-class-decl-to-static-class-env!
    (lambda (c-decl)
      (cases class-decl c-decl 
        (a-class-decl (c-name s-name
                        f-types f-names cons-decl m-decls)
          (let ((super-f-names
                  (static-class->field-names
                    (lookup-static-class s-name)))
                (super-f-types
                  (static-class->field-types
                    (lookup-static-class s-name))))
            (check-no-shadowing! super-f-names f-names c-name)
            (let ((f-names (append super-f-names f-names))
                  (f-types (append super-f-types f-types))
                  (method-tenv
                    (let ((local-method-tenv
                            (method-decls->method-tenv m-decls)))
                      (check-no-dups!
                        (map car local-method-tenv) c-name)
                      (merge-method-tenvs
                        (static-class->method-tenv
                          (lookup-static-class s-name))
                        local-method-tenv))))
              (check-no-dups! f-names c-name)
              (add-static-class-binding! c-name
                (a-static-class s-name f-names f-types method-tenv))))))))
  
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
    (lambda (super-tenv new-tenv)
      (append new-tenv super-tenv)))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

  (define static-class->super-name
    (lambda (sc)
      (cases static-class sc
        (a-static-class (super-name
                          field-names field-types method-types)
          super-name))))

  (define static-class->field-names
    (lambda (sc)
      (cases static-class sc
        (a-static-class (super-name
                          field-names field-types method-types)
          field-names))))

  (define static-class->field-types
    (lambda (sc)
      (cases static-class sc
        (a-static-class (super-name
                          field-names field-types method-types)
          field-types))))

  (define static-class->method-tenv
    (lambda (sc)
      (cases static-class sc
        (a-static-class (super-name
                          field-names field-types method-tenv)
          method-tenv))))

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
        (else (check-no-shadowing! super-f-names (cdr local-f-names) blamee)))))

  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))

  )
