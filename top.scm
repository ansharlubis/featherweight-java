(module top (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")                     ; for scan&parse
  ;(require "checker.scm")                  ; for type-of-program
  (require "tests.scm")                    ; for tests-for-check

  (provide check check-all)

  (provide test-all)
  (define (test-all) 
    (check-all)
    ;;(run-all)
    )

  
  )