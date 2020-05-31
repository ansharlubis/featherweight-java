(module checker (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "static-classes.scm")
  (require "static-data-structures.scm")

  (provide type-of type-of-program)

  ;; type-of-program : Program -> Type  
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (class-decls exp1)
          (initialize-static-class-env! class-decls)
          (for-each check-class-decl! class-decls)
          (type-of exp1 (init-tenv))))))
  
  ;; type-of : Exp -> Tenv
  (define type-of
    (lambda (exp tenv)
      (cases expression exp

        (var-exp (var) (apply-tenv tenv var))

        (field-access-exp (exp1 f-name)
          (let ((receiver-class (type->class-name (type-of exp1 tenv))))
            (let ((f-names (static-class->field-names (lookup-static-class receiver-class)))
                  (f-types (static-class->field-types (lookup-static-class receiver-class))))
              (find-field-type f-names f-types f-name exp))))

        (method-call-exp (obj-exp method-name rands)
          (let ((arg-types (types-of-exps rands tenv))
                (receiver-type (type-of obj-exp tenv)))
            (type-of-call
              (find-method-type (type->class-name receiver-type) method-name)
              arg-types
              rands
              exp)))

        (new-object-exp (class-name rands)
          (let ((rand-types (types-of-exps rands tenv))
                (f-types (static-class->field-types (lookup-static-class class-name))))
            (when (not (= (length rand-types) (length f-types)))
              (report-wrong-number-of-arguments f-types rand-types exp))
            (for-each check-is-subtype! rand-types f-types rands)
            (class-type class-name)))

        (cast-exp (exp1 class-name)
          (let ((obj-type (type-of exp1 tenv))
                (cast-type (class-type class-name))
                (cast-static (lookup-static-class class-name)))
            (if (class-type? obj-type)
              (if (or (is-subtype? obj-type cast-type)
                      (is-subtype? cast-type obj-type))
                cast-type
                (begin
                  (eopl:printf
                    "stupid cast ~s to ~s"
                    (type-to-external-form obj-type)
                    (type-to-external-form cast-type))
                  cast-type))
              (report-bad-type-to-cast obj-type exp))))
        
        )))

  ;;; auxillary functions ;;;
  
  (define types-of-exps
    (lambda (rands tenv)
      (map (lambda (exp) (type-of exp tenv)) rands)))

  ;; type-of-call : Type * Listof(Type) * Listof(Exp) -> Type  
  (define type-of-call
    (lambda (rator-type rand-types rands exp)
      (cases type rator-type
        (proc-type (arg-types result-type)
          (when (not (= (length arg-types) (length rand-types)))
            (report-wrong-number-of-arguments arg-types rand-types exp))
          (for-each check-is-subtype! rand-types arg-types rands)
          result-type)
        (else
          (report-rator-not-of-proc-type
            (type-to-external-form rator-type)
            exp)))))
  
  (define find-field-type
    (lambda (f-names f-types search-f exp)
      (if (null? f-names)
        (report-field-not-found search-f exp)
        (if (eq? (car f-names) search-f)
          (car f-types)
          (find-field-type (cdr f-names) (cdr f-types) search-f exp)))))
  
  ;; is-subtype? : Type * Type -> Bool
  (define is-subtype?
    (lambda (ty1 ty2)
      (cases type ty1
        (class-type (name1)
          (cases type ty2
            (class-type (name2)
              (statically-is-subclass? name1 name2))
            (else #f)))
        (proc-type (args1 res1)
          (cases type ty2
            (proc-type (args2 res2)
              (and
                (every2? is-subtype? args2 args1)
                (is-subtype? res1 res2)))
            (else #f)))
        (else (equal? ty1 ty2)))))

  ;; statically-is-subclass? : ClassName * ClassName -> Bool  
  (define statically-is-subclass?
    (lambda (name1 name2)
      (or
        (eqv? name1 name2)
        (let ((super-name (static-class->super-name (lookup-static-class name1))))
          (if super-name
            (statically-is-subclass? super-name name2)
            #f)))))

  (define andmap
    (lambda (pred lst1 lst2)
      (cond
        ((and (null? lst1) (null? lst2)) #t)
        ((or (null? lst1) (null? lst2)) #f) ; or maybe throw error
        ((pred (car lst1) (car lst2))
         (andmap pred (cdr lst1) (cdr lst2)))
        (else #f))))

  (define every2? andmap)
  
  ;;; well-definedness check ;;;

  ;; check-class-decl! : ClassDecl -> Unspecified  
  (define check-class-decl!
    (lambda (c-decl)
      (cases class-decl c-decl
        (a-class-decl (class-name super-name field-types field-names cons-decl method-decls)
          (check-cons-decl! class-name cons-decl
            field-types field-names
            (static-class->field-types (lookup-static-class super-name))
            (static-class->field-names (lookup-static-class super-name)))
          (for-each
            (lambda (method-decl)
              (check-method-decl!
                method-decl class-name super-name))
            method-decls)))))

  ;; check-cons-decl! :
  ;;   ConstructorDecl * Listof(FieldType) * Listof(FieldName) * Listof(FieldType) * Listof(FieldName)
  ;;    -> Unspecified 
  (define check-cons-decl!
    (lambda (c-name cons-decl local-f-types local-f-names super-f-types super-f-names)
      (cases constructor-decl cons-decl
        (a-constructor-decl (vars var-types super-exp assign-exps)
          (check-cons-params!
            c-name vars var-types
            super-f-names super-f-types
            local-f-names local-f-types)
          (check-super-exp!
            c-name super-exp super-f-names)
          (for-each check-assign-exp! assign-exps local-f-names)))))

  (define check-cons-params!
    (lambda (c-name cons-vars cons-var-types super-f-names super-f-types local-f-names local-f-types)
      (if (and (equal? (append super-f-names local-f-names) cons-vars)
               (equal? (append super-f-types local-f-types) cons-var-types))
        #t
        (report-wrong-cons-params c-name))))

  (define check-super-exp!
    (lambda (c-name s-exp super-f-names)
      (cases super-cons-expression s-exp
        (super-exp (exps)
         (when (not (= (length exps) (length super-f-names)))
           (eopl:error 'wrong-super
             "~s and ~s are not the same length in ~s"
             exps super-f-names c-name))
         (for-each check-var-exp! exps super-f-names)
         #t))))

  (define check-var-exp!
    (lambda (exp1 f-name)
      (cases expression exp1
        (var-exp (identifier)
          (if (eq? identifier f-name)
              #t
              (eopl:error 'wrong-var
                "cons param ~s and field ~s don't match"
                identifier f-name)))
        (else (eopl:error 'check-var-exp "~s not a var expression" exp1)))))

  (define check-assign-exp!
    (lambda (exp1 f-name)
      (cases cons-expression exp1
        (assign-exp (l-exp r-exp)
         (cases expression l-exp
           (field-access-exp (receiver-exp acc-field)
             (cases expression receiver-exp
               (var-exp (receiver-var)
                 (if (eq? receiver-var 'this)
                   (cases expression r-exp
                     (var-exp (assigned-var)
                       (if (and (eq? acc-field f-name) (eq? assigned-var f-name))
                         #t
                         (eopl:error
                           'wrong-field "~s and ~s should be ~s in ~s"
                           acc-field assigned-var f-name exp1)))
                     (else (eopl:error 'not-var-exp "~s should be a var exp" r-exp)))
                   (eopl:error 'not-this-access "~s should be this" receiver-exp)))
               (else (eopl:error 'not-var-exp "~s should be a var exp" receiver-exp))))
           (else (eopl:error 'not-field-access "~s should be a field access" l-exp)))))))
  
  ;; check-method-decl! :
  ;;   MethodDecl * ClassName * ClassName * Listof(FieldName) * \Listof(Type) 
  ;;    -> Unspecified    
  (define check-method-decl!
    (lambda (m-decl self-name super-name)
      (cases method-decl m-decl
        (a-method-decl (res-type m-name vars var-types body)
         (let ((tenv
                 (extend-tenv vars var-types
                   (extend-tenv (list 'this) (list (class-type self-name))
                     (init-tenv)))))
           (let ((body-type (type-of body tenv)))
             (check-is-subtype! body-type res-type m-decl)
             (let ((maybe-super-method-type
                     (maybe-find-method-type
                       (static-class->method-tenv (lookup-static-class super-name))
                       m-name)))
               (if maybe-super-method-type
                   (check-equal-type!
                     (proc-type var-types res-type)
                     maybe-super-method-type
                     body)
                   #t))))))))
  
  ;;; types ;;;

  ;; check-is-equal-type! : Type * Type * Exp -> Unspecified  
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (if (equal? ty1 ty2)
        #t
        (eopl:error 'type-of
          "Types didn't match: ~s !- ~s in ~%~s"
          (type-to-external-form ty1)
          (type-to-external-form ty2)
          exp))))

  ;; check-is-subtype! : Type * Type * Exp -> Unspecified
  (define check-is-subtype!
    (lambda (ty1 ty2 exp)
      (if (is-subtype? ty1 ty2)
        #t
        (report-subtype-failure
          (type-to-external-form ty1)
          (type-to-external-form ty2)
          exp))))

  ;;; report ;;;

  (define report-rator-not-of-proc-type
    (lambda (external-form-rator-type exp)
      (eopl:error 'type-of-call
         "rator ~s is not of proc-type ~s"
         exp external-form-rator-type)))

  (define report-wrong-number-of-arguments
    (lambda (arg-types rand-types exp)
      (eopl:error 'type-of-call
        "These are not the same: ~s and ~s in ~s"
        (map type-to-external-form arg-types)
        (map type-to-external-form rand-types)
        exp)))
  
  (define report-field-not-found
    (lambda (search-f exp)
      (eopl:error 'field-not-found
        "~s has no field ~s"
        exp search-f)))

  (define report-subtype-failure
    (lambda (external-form-ty1 external-form-ty2 exp)
      (eopl:error 'check-is-subtype!
        "~s is not a subtype of ~s in ~%~s"
        external-form-ty1
        external-form-ty2
        exp)))

  (define report-bad-type-to-cast 
    (lambda (type exp)
      (eopl:error 'bad-type-to-case
        "can't cast non-object; ~s had type ~s"
        exp
        (type-to-external-form type))))

  (define report-wrong-cons-params
    (lambda (c-name)
      (eopl:error 'check-cons-params!
        "wrong parameters for constructor of ~s"
        c-name)))
 
  
  )