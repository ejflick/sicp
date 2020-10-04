#lang sicp

(define hello 3)

(define (square x)
  (* x x))

(define (halve x)
  (/ x 2))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))

#|

EX 1.6
Defining in terms of the new if causes the clauses to first be evaluated.

EX 1.7

|#

#| Second go at good enough |#
(define (good-enough? guess x)
  (= (improve guess x) guess))


#| EX 1.8 |#

(define (cube-root x)
  (define (cube x)
    (* x x x))

  (define (cube-good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))

  (define (improve-cube-root guess)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))

  (define (cube-root-iter guess)
    (if (cube-good-enough? guess)
        guess
        (cube-root-iter (improve-cube-root guess))))

  (cube-root-iter 1.0))

#| --- |#
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

  (fact-iter 1 1 n))

#| EX 1.9
(define (+ a b)
(if (= a 0)
b
(inc (+ (dec a ) b))))

(define (+ a b)
(if (= a 0)
b
(+ (dec a) (inc b))))

First function:
(+ 3 4)
(inc (+ (dec 3) 4))
(inc (+ (inc (+ (2) ... ))))
^ recursive

Second:
(+ 3 4)
(+ (dec 3) (inc 4))
(+ 2 5)
(+ (dec 2) (inc 5))
(+ 1 6)
(+ (dec 1) (dec 6))
(+ 0 7)
7
^ iterative...no hidden state
|#

#| EX 1.10 |#
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|
(f n) = (A 0 n) = (* n 2)

(g n) = (A 1 n)
(A 1 10)
(A (- x 1) (A 1 (- y 10)))

(h n) = (A 2 n)

(k n) = (* 5 n n)
|#

(print "hi")
(define lol "lol")

#| 1.2.2 Tree Recursion |#
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

#|
(fib 5)
(fib-iter 1 0 4)
(fib-iter 1 1 3)
(fib-iter 2 1 2)
(fib-iter 3 2 1)
(fib-iter 5 3 0)
5
|#

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

#| EX 1.11 |#
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
