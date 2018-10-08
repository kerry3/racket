#lang plai

(define (my-length lst) (cond [(empty? lst) 0] [else (+ 1 (my-length (rest lst)))] ))


(define (cod a)
  (cond
      [(symbol=? a 'dog) 'happy]
      [(symbol=? a 'cat) 'smart]
      [(symbol=? a 'pig) 'pinky]
      [else empty]
      )
  )

; cons 의 특성상 pair일 경우 리스트와 구분을 하기 위해 두 요소 사이에 . 을 찍어서 pair임을 명시적으로 보여줘서 그래. Racket이 pair를 그렇게 표현해서 그래.

(define (name-pets lst)
  (cond
    [(= 1 (my-length lst)) (cons (cod (first lst)) empty)] ; cons 랑 empty 추가
    [else (cons (cod (first lst)) (name-pets (rest lst)))]
    )
    
)

(test (name-pets '(dog monkey pig cat)) '(happy () pinky smart))
(test (name-pets '(dog lion pig cat)) '(happy () pinky smart))