#lang racket
(require plai-typed)

;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S
;; Data Definition of msl expression
;named each arithmetic expressions correspond to the formal grammar
; lhs -> left hand side
; rhs -> right hand side

(define-type msl
[msl-num (n : number)]
[msl-add (lhs : msl) (rhs : msl)]
[msl-sub (lhs : msl) (rhs : msl)]
[msl-mul (lhs : msl) (rhs : msl)]
[msl-expt (lhs : msl) (rhs : msl)]
)
;This code help to find  exponential number
(define (expt x y)
(cond
((= y 0) 1)
(else
(* x (expt x (- y 1))))))
;Contract
;; eval msl -> number
;Purpose 
;; evaluate an msl expression

;; examples
;; (msl-num 7) -> 7
;; (msl-add (msl-num 3) (msl-num 4)) -> 7
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42
(define (eval [expr : msl])
(type-case msl expr
[msl-num (n) n]
[msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
[msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
[msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
[msl-expt (lhs rhs) (expt (eval lhs) (eval rhs))]
))
;Tests
(test(eval (msl-num 7))7)
(test (eval (msl-num 12)) 12)
(test(eval (msl-add (msl-num 40) (msl-num 10)))50)
(test(eval (msl-expt (msl-num 5) (msl-num 5)))3125)
(test (eval (msl-sub (msl-num 22) (msl-num 13))) 9)
(test (eval (msl-sub (msl-num 71) (msl-num 70))) 1)
(test (eval (msl-expt (msl-num 4) (msl-num 3))) 64)
(test (eval (msl-expt (msl-num 5) (msl-num 4))) 625)
(test (eval (msl-mul (msl-expt (msl-num 2) (msl-num 5)) (msl-sub (msl-num 12) (msl-num 8)))) 128)
(test (eval (msl-add (msl-sub (msl-num 6) (msl-num 4)) (msl-mul (msl-num 2) (msl-num 7)))) 16)
;; parse infix s-expression -> msl
;; convert a infix arithmetic expression into the equivalent msl form.
;; examples
;; '7 -> (msl-num 7)
;; '(3 + 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '((3 + 4) + 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))

(define (in-parse [s : s-expression]) : msl
(cond
[(s-exp-number? s) (msl-num (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(case (s-exp->symbol (second sl))
[(+) (msl-add (in-parse (first sl)) (in-parse (third sl)))]
[(*) (msl-mul (in-parse (first sl)) (in-parse (third sl)))]
[(-) (msl-sub (in-parse (first sl)) (in-parse (third sl)))]
[(**) (msl-expt (in-parse (first sl)) (in-parse (third sl)))]
[else (error 'parse "invalid list input")]))]
[else (error 'parse "invalid input")]))
; Tests for in-parse.
(in-parse '(3 + 4))
(test (in-parse '7) (msl-num 7))
(test (in-parse '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (in-parse '((3 + 4) + 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))

;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
(define (parse [s : s-expression]) : msl
(cond
[(s-exp-number? s) (msl-num (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(case (s-exp->symbol (first sl))
[(+) (msl-add (parse (second sl)) (parse (third sl)))]
[(*) (msl-mul (parse (second sl)) (parse (third sl)))]
[(-) (msl-sub (parse (second sl)) (parse (third sl)))]
[(**) (msl-expt (parse (second sl)) (parse (third sl)))]
[else (error 'parse "invalid list input")]))]
[else (error 'parse "invalid input")]))
(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))
;;defines a new data type
; S -> number
; S -> + S S
; S -> - S S
; S -> * S S
; S -> ^ S S
; S -> -1 * S (The best one without problem) (Uniary Minus)
; S -> (S)
;uminusS --> -b = -1 × b
;bminusS --> a - b = a + -1 × b
;e is a mslS term.
(define-type mslS
[numS (n : number)]
[plusS (l : mslS) (r : mslS)]
[bminusS (l : mslS) (r : mslS)]
[mulS (l : mslS) (r : mslS)]
[expS (l : mslS) (r : mslS)]
[uminusS (e : mslS)])

;;pre- parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (numFS 7)
;; '(+ 3 4) -> (plusFS (numFS 3) (numFS 4))
;; '(+ (+ 3 4) 35) -> (plusFS (plusFS (numFS 3) (numFS 4)) (numFS 35))
(define (preS-parse [s : s-expression]) : mslS
(cond
[(s-exp-number? s) (numS (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(cond
[(= (length sl) 2) (case (s-exp->symbol (first sl)) [(u-) (uminusS (preS-parse (second sl)))]) ]
[else (case (s-exp->symbol (first sl))
[(+) (plusS (preS-parse (second sl)) (preS-parse (third sl)))]
[(*) (mulS (preS-parse (second sl)) (preS-parse (third sl)))]
[(-) (bminusS (preS-parse (second sl)) (preS-parse (third sl)))]
[(**) (expS (preS-parse (second sl)) (preS-parse (third sl)))]
[else (error 'preF-parse "invalid list input")])]))]
[else (error 'preS-parse "invalid input")]))

;Tests

(test (preS-parse '7) (numS 7))
(test (preS-parse '(+ 3 4)) (plusS (numS 3) (numS 4)))
(test (preS-parse '(+ (* (u- 7) 5) 7))(plusS (mulS (uminusS (numS 7)) (numS 5)) (numS 7)))
(test (preS-parse '(u- 7)) (uminusS (numS 7)))
(test (preS-parse '(** 3 7)) (expS (numS 3) (numS 7)))
(test (preS-parse '(** 5 6)) (expS (numS 5) (numS 6)))
(test (preS-parse '(u- 12)) (uminusS (numS 12)))
(test (preS-parse '(u- 23)) (uminusS (numS 23)))
;; desugar mslS -> msl
;; convert new data type to old data type
;; Examples
;; (numS 5) -> (msl-num 9)
;; (plusS (numS 3) (numS 4)) -> (msl-add (msl-num 3) (msl-num 4))
;; (uminusS (numS 7)) -> (msl-mul (msl-num -1) (msl-num 7))
(define (desugar [as : mslS]) : msl
(type-case mslS as
[numS (n)(msl-num n)]
[plusS (lhs rhs) (msl-add (desugar lhs) (desugar rhs))]
[bminusS (lhs rhs) (msl-sub (desugar lhs) (desugar rhs))]
[mulS (lhs rhs) (msl-mul (desugar lhs) (desugar rhs))]
[expS (lhs rhs) (msl-expt (desugar lhs) (desugar rhs))]
[uminusS (e) (msl-mul (msl-num -1) (desugar e))]))

;test
(test (desugar (numS 5)) (msl-num 5))
(test (desugar (plusS (numS 3) (numS 4))) (msl-add (msl-num 3)(msl-num 4)))
(test (desugar (numS 6)) (msl-num 6))
(test (desugar (plusS (numS 5) (numS 5))) (msl-add (msl-num 5) (msl-num 5)))
(test (desugar (bminusS (numS 7) (numS 7))) (msl-sub (msl-num 7) (msl-num 7)))
(test (desugar (mulS (numS 8) (numS 8))) (msl-mul (msl-num 8) (msl-num 8)))
(test (desugar(uminusS (numS 87))) (msl-mul (msl-num -1) (msl-num 87)))
(test(desugar(uminusS (numS 7)))(msl-mul (msl-num -1) (msl-num 7)))
;; Parse infix s-expression -> mslS
;; Convert a infix arithmetic expression into the equivalent mslS form.
;; Examples
;; '7 -> (numFS 7)
;; '(3 + 4) -> (plusFS (numFS 3) (numFS 4))
;; '((3 + 4) + 35) -> (plusFS (plusFS (numFS 3) (numFS 4)) (numFS 35))
(define (inS-parse [s : s-expression]) : mslS
(cond
[(s-exp-number? s) (numS (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(cond
[(= (length sl) 2) (case (s-exp->symbol (first sl)) [(u-) (uminusS (inS-parse (second sl)))]) ]
[else (case (s-exp->symbol (second sl))
[(+) (plusS (inS-parse (first sl)) (inS-parse (third sl)))]
[(*) (mulS (inS-parse (first sl)) (inS-parse (third sl)))]
[(-) (bminusS (inS-parse (first sl)) (inS-parse (third sl)))]
[(**) (expS (inS-parse (first sl)) (inS-parse (third sl)))]
[else (error 'inS-parse "invalid list input")])]))]
[else (error 'inS-parse "invalid input")]))

(test (inS-parse '(3 + 4)) (plusS (numS 3) (numS 4)))
(test (inS-parse '((3 + 4) + 3)) (plusS (plusS (numS 3) (numS 4)) (numS 3)))
(test (inS-parse '(u- 44)) (uminusS (numS 44)))
(test (inS-parse '7) (numS 7))