(module classes (lib "eopl.ss" "eopl")

  (require "lang.scm")

  ;; object interface
  (provide object object? object-to-external-form new-object object->class-name object->fields)

  ;; field interface
  (provide find-field)
  
  ;; method interface
  (provide method method? a-method find-method)

  ;; class-interface
  (provide lookup-class initialize-class-env! class->super-name)

  ;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

  ;; an object consists of a symbol denoting its class, and a list of
  ;; references representing the managed storage for the all the fields. 

  (define-datatype object object?
    (an-object
      (class-name symbol?)
      (fields (list-of object?))))

  (define object-to-external-form
    (lambda (obj)
      (cases object obj
        (an-object (c-name fields)
          (if (null? fields)
            (list c-name '())
            (list c-name (map object-to-external-form fields)))))))

  (define new-object
    (lambda (class-name fields)
      (an-object class-name fields)))

  ;;;;;;;;;;;;;;;; fields ;;;;;;;;;;;;;;;;;;;;

  (define find-field
    (lambda (c-name f-name)
      (let ((c-fields (class->field-names (lookup-class c-name))))
        (letrec
          ((find-inner
             (lambda (fields search-f n)
               (if (null? fields)
                 (report-field-not-found search-f)
                 (if (eq? (car fields) search-f)
                   n
                   (find-inner (cdr fields) search-f (+ n 1)))))))
          (find-inner c-fields f-name 0)))))
  
  ;;;;;;;;;;;;;;;; methods and method environments ;;;;;;;;;;;;;;;;

  (define-datatype method method?
    (a-method
      (vars (list-of symbol?))
      (body expression?)
      (super-name symbol?)))

  ;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

  (define method-environment?
    (list-of
      (lambda (p)
        (and
          (pair? p)
          (symbol? (car p))
          (method? (cadr p))))))

  ;; find-method: Sym * Sym -> Method
  (define find-method
    (lambda (c-name m-name)
      (let ((m-env (class->method-env (lookup-class c-name))))
        (let ((maybe-pair (assq m-name m-env)))
          (if (pair? maybe-pair)
            (cadr maybe-pair)
            (report-method-not-found m-name))))))

  ;; merge-method-envs: MethodEnv * MethodEnv -> MethodEnv
  (define merge-method-envs
    (lambda (super-m-env new-m-env)
      (append new-m-env super-m-env)))

  ;; method-decls->method-env:
  ;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
  (define method-decls->method-env
    (lambda (m-decls super-name)
      (map
        (lambda (m-decl)
          (cases method-decl m-decl
            (a-method-decl (result-type method-name vars var-types body)
              (list method-name
                (a-method vars body super-name)))))
        m-decls)))

  ;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

  (define-datatype class class?
    (a-class
      (super-name (maybe symbol?))
      (field-names (list-of symbol?))
      (method-env method-environment?)))

  ;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

  ;; the-class-env: ClassEnv
  (define the-class-env '())

  ;; add-to-class-env!: ClassName * Class -> Unspecified
  (define add-to-class-env!
    (lambda (class-name class)
      (set! the-class-env
        (cons
          (list class-name class)
          the-class-env))))

  ;; lookup-class: ClassName -> Class
  (define lookup-class
    (lambda (name)
      (let ((maybe-pair (assq name the-class-env)))
        (if maybe-pair (cadr maybe-pair)
          (report-unknown-class name)))))

  ;; initialize-class-env!: Listof(ClassDecl) -> Unspecified
  (define initialize-class-env!
    (lambda (c-decls)
      (set! the-class-env
        (list
          (list 'object (a-class #f '() '()))))
      (for-each initialize-class-decl! c-decls)))

  ;; initialize-class-decl: ClassDecl -> Unspecified
  (define initialize-class-decl!
    (lambda (c-decl)
      (cases class-decl c-decl
        (a-class-decl (class-name super-name field-types field-names constructor-decl method-decls)
          (let ((field-names
                  (append
                    (class->field-names (lookup-class super-name))
                    field-names)))
            (add-to-class-env!
              class-name
              (a-class
                super-name
                field-names
                (merge-method-envs
                  (class->method-env (lookup-class super-name))
                  (method-decls->method-env
                    method-decls super-name)))))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

  (define class->super-name
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name  field-names method-env)
          super-name))))

  (define class->field-names
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names method-env)
          field-names))))

  (define class->method-env
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names method-env)
          method-env))))


  (define object->class-name
    (lambda (obj)
      (cases object obj
        (an-object (class-name fields)
          class-name))))

  (define object->fields
    (lambda (obj)
      (cases object obj
        (an-object (class-decl fields)
          fields))))

  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))

  ;;;;;;;;;;;;;;;;;;;;;; reports ;;;;;;;;;;;;;;;;;;;;;;;;

  (define report-field-not-found
    (lambda (name)
      (eopl:error 'find-field "unknown field ~s" name)))
  
  (define report-method-not-found
    (lambda (name)
      (eopl:error 'find-method "unknown method ~s" name)))

  (define report-unknown-class
    (lambda (name)
      (eopl:error 'lookup-class "unknown class ~s" name)))
  
  )