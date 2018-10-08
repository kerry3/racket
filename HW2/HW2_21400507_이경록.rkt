#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

; Solved by myself: N (JC gave me some hints)
; Time taken: about 5 hours
; [contract] mile->km: list -> list
; [purpose] To convert list to abstract syntax

(define (parse sexp)

  (match sexp
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (sub (parse lhs) (parse rhs))]
    [(? number? sexp) (num sexp)] ;; number만 받을 때랑 list받는 거를 구분하는게 제일 힘들었음 
    [else (error 'parse "bad syntax: ~a" sexp)]
          ))
                

(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '(- 4 3)) (sub (num 4) (num 3)))
(test (parse '(+ 5 2)) (add (num 5) (num 2)))
(test (parse '(+ 3 (- 4 3))) (add (num 3) (sub (num 4) (num 3)))) 
(test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
