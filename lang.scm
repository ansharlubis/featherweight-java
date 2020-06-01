(module lang (lib "eopl.ss" "eopl")

  ;; grammar for Featherweight Java
  (require "drscheme-init.scm")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;;;;

  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
        (letter (arbno (or letter digit "_" "-" "?")))
        symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program ((arbno class-decl) expression) a-program)
      
      ;; expressions consists of
      ;; -variable
      ;; -field access
      ;; -method invocation
      ;; -object creation
      ;; -casting
      
      (expression (identifier) var-exp)

      (expression
       ("access" expression identifier)
       field-access-exp)
      
      (expression
       ("send" expression identifier
        "(" (separated-list expression ",") ")")
       method-call-exp)

      (expression
       ("new" identifier "(" (separated-list expression ",") ")")
       new-object-exp)

      (expression
       ("cast" expression identifier)
       cast-exp)

      ;; the following expressions are only allowed in constructor
      (cons-expression
       ("set" expression "=" expression)
       assign-exp)
      
      (super-cons-expression
       ("super" "(" (separated-list expression ",") ")")
       super-exp)

      ;; declarations

      (class-decl
        ("class" identifier "extends" identifier "{"
           (arbno "field" type identifier)
           constructor-decl
           (arbno method-decl)
         "}")
        a-class-decl)

      (constructor-decl
        ("constructor" "(" (separated-list identifier ":" type ",") ")" "{"
           super-cons-expression
           (arbno cons-expression)
         "}")
       a-constructor-decl)

      (method-decl
       ("method" type identifier
        "(" (separated-list identifier ":" type ",") ")" "{"
          expression
        "}")
       a-method-decl)

      ;; types
      (type                               
        ("(" (separated-list type "*") "->" type ")")
        proc-type)
      (type ("void") void-type)
      (type (identifier) class-type)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  ;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;

  (define type->class-name
    (lambda (ty)
      (cases type ty
        (class-type (name) name)
        (else (eopl:error 'type->class-name
                "Not a class type: ~s"
                ty)))))

  (define class-type?
    (lambda (ty)
      (cases type ty
        (class-type (name) #t)
        (else #f))))

  (define type-to-external-form
    (lambda (ty)
      (cases type ty
        (void-type () 'void)
        (class-type (name) name)
        (proc-type (arg-types result-type)
          (append
            (formal-types-to-external-form arg-types)
            '(->)
            (list (type-to-external-form result-type)))))))

  (define formal-types-to-external-form
    (lambda (types)
      (if (null? types)
        '()
        (if (null? (cdr types))
          (list (type-to-external-form (car types)))
          (cons
            (type-to-external-form (car types))
            (cons '*
              (formal-types-to-external-form (cdr types))))))))

  )