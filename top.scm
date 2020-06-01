(module top (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")                     ; for scan&parse
  (require "checker.scm")                  ; for type-of-program
  (require "tests.scm")                    ; for tests-for-check
  (require "interp.scm")
  (require "classes.scm")
  (require "data-structures.scm")

  (provide check check-all)

  (provide test-all)
  (define (test-all) 
    (check-all))

  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

  ;; check : String -> ExternalType
  (define check
    (lambda (string)
      (type-to-external-form
        (type-of-program (scan&parse string)))))

  ;; check-all : () -> Unspecified
  ;; checks all the tests in test-list, comparing the results with
  ;; equal-answer?
  (define check-all
    (lambda ()
      (run-tests! check equal? tests-for-check)))

  ;; check-one : Sym -> ExpVal
  ;; (check-one sym) checks the test whose name is sym
  (define check-one
    (lambda (test-name)
      (let ((the-test (assoc test-name tests-for-check)))
        (cond
          (the-test
           => (lambda (test)
                (check (cadr test))))
          (else (eopl:error 'check-one "no such test: ~s" test-name))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  
  ;;(stop-after-first-error #t)
  ;;(check-all)
  
  )