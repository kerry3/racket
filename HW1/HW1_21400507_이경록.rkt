
#lang plai
; Problem 1
; Solved by myself: Y
; Time taken: about 2 mins
; [contract] mile->km: number ->number
; [purpose] To convert mile to km
; [tests](test(mile->km 1) 1.6)
;        (test(mile->km 2) 3.2)

(define(mile->km a)
	(* a 1.6))
(test(mile->km 1) 1.6)
(test(mile->km 2) 3.2)


; Problem 2
; Solved by myself: Y
; Time taken: about 6 mins
; [contract] volume-cuboid: number ->number
; [purpose] To calculate volume with three lengths
; [tests] (test(volume-cuboid 1 2 3) 6)
;         (test(volume-cuboid 2 4 5) 40)


(define(volume-cuboid a b c)
  (* a b c))
(test(volume-cuboid 1 2 3) 6)
(test(volume-cuboid 2 4 5) 40)


; Problem 3
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] is-odd: number ->boolean
; [purpose] To check if the number is odd number
; [tests] (test(is-odd? 2) false)
;          (test(is-odd? 3) true)

(define(is-odd? a)
  (= (remainder a 2) 1))
(test(is-odd? 2) false)
(test(is-odd? 3) true)

; Problem 4
; Solved by myself: Y
; Time taken: about 5 hours
; [contract] gcd: number number->number
; [purpose] to find the greatest common divisor
; [tests] (test(gcd 1480 540) 20)
; [tests] (test(gcd 24 5) 1)


;유클리드 호제법
; 두 수 중에 작은 수 구해서
; 그 수와 그 수로 큰 수 나눈 수의 나머지를
; 다시 함수에 집어넣음
; 재귀 깨는 공식은 나머지가 0이 될때

;problem
; I was trying to use loop like C and Java but racket is different from them.
; Thus, I had to come up with something else where I don't have to use loop and I googled like that
; Eventually, I found Eucledean algorithm and I implemented that algorithm on my own and my code worked at once 
; url : http://codingstarter.tistory.com/21

(define(smaller a b)
 (cond
   [(< a b) a]
   [(> a b) b]))


(define(bigger a b)
 (cond
   [(< a b) b]
   [(> a b) a]))

(define(gcd a b)
  (cond
    [(= 0 (remainder (bigger a b) (smaller a b))) (smaller a b)]
    [else(gcd (smaller a b) (remainder (bigger a b) (smaller a b)))]
       ))

(test(gcd 1480 540) 20)
(test(gcd 24 5) 1)


; Problem 5
; Solved by myself: Y
; Time taken: about 3 mins
; [contract] lcm: number number->number
; [purpose] to find the least common multiple
; [tests] (test (lcm 3 6) 6)
; [tests] (test (lcm 4 16) 16)


; I found out the least common multiple of number a and b is a*b/gcd(a,b) by googling
; url : https://twpower.github.io/69-how-to-get-gcd-and-lcm



(define (lcm a b)
  (/ (* a b) (gcd a b))
  )

(test (lcm 3 6) 6)
(test (lcm 4 16) 16)

; Problem 6-a
; Solved by myself: Y
; Time taken: about 5 hours
; [contract] have-homework: COURSE -> number
; [purpose] to find the number of assignments for the given course

;1. define the type COURSE

(define-type COURSE
  [ECE20016(lab integer?)
           (homework integer?)]
  [ITP20005(homework integer?)]
  [ITP40001(projects integer?)
          (homework integer?)])

;create instances
(define ECE(ECE20016 2 4))
(define ITPS(ITP20005 3))
(define ITPT(ITP40001 5 8))

;check values first
(ECE20016-homework ECE)
(ITP20005-homework ITPS)
(ITP40001-homework ITPT)



(define (have-homework course)
  (cond
    [(ECE20016? course) (ECE20016-homework course)]
    [(ITP20005? course) (ITP20005-homework course)]
    [(ITP40001? course) (ITP40001-homework course)]
))

;test
(test(have-homework ECE)4)
(test(have-homework ITPS)3)
(test(have-homework ITPT)8)

; Problem 6-b
; Solved by myself: Y
; Time taken: about 30 mins
; [contract] have-homework: COURSE -> boolean
; [purpose] to check if the course is ITP40001 and the number of project it is more than or equal to two.

(define (have-homework? course)
  (and (ITP40001? course) ( >= (ITP40001-projects course) 2))
)

;test
(test (have-homework? ITPT) true)
(test (have-homework? ECE) false)


; Problem 7
; Solved by myself: Y
; Time taken: about 3 hours
; [contract] name-pets: list->list
; [purpose] to change dog, cat, pig into happy, smart, pinky and the other into empty

(define (name-pets lst)
  (for/list ([i lst])
    (cond
      [(symbol=? i 'dog) 'happy]
      [(symbol=? i 'cat) 'smart]
      [(symbol=? i 'pig) 'pinky]
      [else empty]
      )
    )
)

;test
(test (name-pets '(dog monkey pig cat)) '(happy () pinky smart))
(test (name-pets '(dog lion pig cat)) '(happy () pinky smart))



; Problem 8
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] give-name : symbol symbol list -> list
; [purpose] in the list, to change all the old names into new names

(define (give-name old new lst)
  (for/list ([i lst])
    (cond
      [(symbol=? i old) new]
      [else i]
      )
    )
  )

;test

(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty)))) '(pig cat pooh))
(test (give-name 'bear 'pooh (cons 'lion (cons 'dog (cons 'bear empty)))) '(lion dog pooh))

