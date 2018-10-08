#lang plai

; Solved by myself: Y
; Time taken: 4 days
; [contract] free-ids: WAE -> list-of-symbols 
;            binding-ids: WAE -> list-of-symbols           
; [purpose]  free-ids: to get the free identifiers.
;            binding-ids: to get the binding-ids.

(define-type WAE
[num (n number?)]
[add (lhs WAE?) (rhs WAE?)]
[sub (lhs WAE?) (rhs WAE?)]
[with (name symbol?) (named-expr WAE?) (body WAE?)]
[id (name symbol?)])

; [contract] subst: WAE symbol number -> WAE
(define (subst wae idtf val) ; wae, identifier, value
  (type-case WAE wae
    [num (n) wae] ;subst 함수의 parameter wae로 (num 1) 과 같은 게 들어올 경우 (num 1) 출력
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))] ; duplicate 삭제했으므로 이제 val그냥 둬도 됨 unlike first.
    [with (i v e) (with i (subst v idtf val) ;e : target expression we want to compute
                        (if (symbol=? i idtf) e ; i랑 idef랑 같으면 e를 출력하고 아니면 (subst e idtf val) 실행
                            (subst e idtf val)))]
    [id (s) (cond
              [(symbol? val) (if (symbol=? s idtf) (id val) wae)] 
              [else (if (symbol=? s idtf) (num val) wae)] ; s랑 idtf랑 같으면 (num val)을 출력하고 아니면 wae(인스턴스 전체) 출력
    )]
    ))

; free-ids: WAE -> list-of-symbols
(define (free-ids wae)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (cond
                 [(and (num? l) (num? r)) empty]
                 [(num? l)(remove-duplicates (sort (free-ids r) symbol<?))]
                 [(num? r)(remove-duplicates (sort (free-ids l) symbol<?))]
                 [else (remove-duplicates (sort (append (free-ids l) (free-ids r)) symbol<?))]
                 )]
    [sub (l r) (cond
                 [(and (num? l) (num? r)) empty]
                 [(num? l) (remove-duplicates (sort (free-ids r) symbol<?))]
                 [(num? r) (remove-duplicates (sort (free-ids l) symbol<?))]
                 [else (remove-duplicates (sort (append (free-ids l) (free-ids r)) symbol<?))]
                 )]
    [with (i v e) (cond
                    [(id? v)
                     (cond
                       [(eq? i (first (free-ids e))) (remove-duplicates (sort (free-ids (subst e i (first (free-ids v)))) symbol<?))]
                       [else (remove-duplicates (sort (append (free-ids (subst e i (first (free-ids v)))) (free-ids v)) symbol<?))]
                               )]
                    [(with? v) (remove-duplicates (sort (append (free-ids (subst e i 12)) (free-ids v)) symbol<?))] ; 11번 케이스 때문에 어차피 free identifier가 다 bosdy에 있어서 일단 아무 상수 v에 집어넣고 나중에 따로 체크하기기
                    [else (remove-duplicates (sort (free-ids (subst e i (free-ids v))) symbol<?))]
                    )]
    [id (s) (list s)]))

(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))




;binding-ids: WAE -> list-of-symbols
(define (binding-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [add (l r) empty]
    [sub (l r) empty]
    [with (i v e)  (cond
                     [(with? v) (remove-duplicates (sort (append (list i) (binding-ids v) ) symbol<?))]
                     [else (remove-duplicates (sort (append (list i) (binding-ids e)) symbol<?))]
                     )]
    [id (s) empty]
    ))

(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))



