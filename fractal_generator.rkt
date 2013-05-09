#lang racket

(define (iterate p n)
  (cond
    ((zero? n) p)
    (#t (iterate (+ p (* p p)) (- n 1)))))

(define (num-iter p max-iter threshold)
  (define (num-iter-c n q)
    (cond
      ((< threshold (dist 0 q)) n)
      ((= n max-iter) 0)
      (#t (num-iter-c (+ n 1) (iterate q 1)))))
  (num-iter-c 0 p))

(define (dist p q)
  (sqrt (+ (expt (- (real-part p)(real-part q)) 2)(expt (- (imag-part p)(imag-part q)) 2))))

;We will start with a 10^4 length byte string, representing the interval [(0+0i),(1+1i)).
;The first byte will be for the pixel (0+0.99i), and the last for (0.99+0i).
;Let's write the function that finds the byte corresponding to a point.

;(define (bs-index p)
;  (inexact->exact (* 100 (+ (real-part p) (* 100 (- 0.99 (imag-part p)))))))

(define (byte-to-point b)
  (+ (/ (remainder b 100) 100) (* 0+1i (- 0.99 (/ (quotient b 100) 100)))))

;(define (store-bs bs p r)
;  (bytes-set! bs (bs-index p) r))

(define buffer (make-bytes 10000))

(define (run-byte b)
  (bytes-set! buffer b (num-iter (byte-to-point b) 255 50)))

;(define (run-point p)
;  (store-bs buffer p (num-iter p 255 50)))

(define (run-whole)
  (define (run-whole-c byte)
    (cond
      ((eq? byte 10000) (void))
      (#t (run-byte byte) (run-whole-c (+ byte 1)))))
  (run-whole-c 0))

