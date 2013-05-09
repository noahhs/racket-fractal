#lang slideshow
(require slideshow/code)

(define (is-in el l)
  (cond
    ((equal? l (quote())) #f)
    (#t (or (equal? el (car l)) (is-in el (cdr l))))))
  
;Given a width, height, and list of (col row) coordinates,
;return a function of pictures p and q that assembles a (w x h) composite picture,
;with q in place of the listed coordinates, and p in place of the rest.

(define (func1-maker w h coord-list)
  (lambda (p q)
    (define (slot-pic coord)
      (cond
        ((is-in coord coord-list) q)
        (#t p)))
    (define (row-pics r)
      (define (slot-pics c)
        (cond
          ((> c w)(quote()))
          (#t (cons (slot-pic (list c r))(slot-pics (+ c 1))))))
      (define row-pic (apply hc-append (slot-pics 1)))
      (cond
        ((> r h)(quote()))
        (#t (cons row-pic (row-pics (+ r 1))))))
    (apply vc-append (row-pics 1))))

; Returns a list of tiling functions, based on
; a list of carpet dimensions and a list of lists of coordinates
(define (func1-list-maker lcd llc)
  (cond
    ((equal? lcd (quote()))(quote()))
    (#t (let* 
            ((w (car (car lcd)))
             (h (car (cdr (car lcd))))
             (coord-list (car llc))
             (func1 (func1-maker w h coord-list)))
          (cons func1 (func1-list-maker (cdr lcd)(cdr llc)))))))
                      
; Returns a white tile drawing function, that takes an iteration number
; and a list of carpet dimensions.
(define (new-func2-maker lcd td)
  (define tw (car td))
  (define th (car (cdr td)))
  (define (wfact iter lcd)
    (cond
      ((= iter 1) 1)
      (#t (let
              ((cw (car (index lcd iter))))
            (* cw (wfact (- iter 1) lcd))))))
  (define (hfact iter lcd)
    (cond
      ((= iter 1) 1)
      (#t (let
              ((ch (car (cdr (index lcd iter)))))
            (* ch (hfact (- iter 1) lcd))))))
  (lambda (iter)
    (let*
        ((w (* tw (wfact iter lcd)))
         (h (* th (hfact iter lcd))))
      (colorize (filled-rectangle w h) "white"))))

; Returns a new-style figure-struct, containing
;   - A list of carpet dimensions (each w x h)
;   - A tile dimension (w x h)
;   - A list of lists of coordinates
(define (new-figure-struct lcd td llc)
  (let*
      ((func1-list (func1-list-maker lcd llc))
       (func2 (new-func2-maker lcd td))
       (tw (car td))
       (th (car (cdr td)))
       (p (colorize (filled-rectangle tw th) "red")))
    (list func1-list func2 p)))

(define (length l)
  (cond
    ((equal? l (quote())) 0)
    (#t (+ 1 (length (cdr l))))))

; Returns the nth element of a list, modulo the length of the list
(define (index l n)
  (define len (length l))
  (cond
    ((> n len) (index l (- n len)))
    ((= n 1)(car l))
    (#t (index (cdr l)(- n 1)))))

(define (new-carpet iter new-fig-st)
  (define p (car (cdr (cdr new-fig-st))))
    (cond
      ((zero? iter) p)
      (#t (let*
              ((func1-list (car new-fig-st))
               (func1 (index func1-list iter))
               (func2 (car (cdr new-fig-st)))
               (prev-carpet (new-carpet (- iter 1) new-fig-st)))
            (func1 prev-carpet (func2 iter))))))

; Create a shortcut for multiple reps of a tiling function.
; Maybe instead of just having a list of tiling functions, have a struct
; that also includes a list of reps for each function.
; Also, a parameter that specifies where in the list you start, and at what rep.

; What I really want is an efficient process to explore with.
; The way to get that is to have a structure that specifies my figure,
; a function that draws it, and several functions that modify it.
; For instance, one function changes the iteration number;
; one function re-specifies the starting point of the iterative cycle;
; one function changes the length of the cycle, specifying the position
; of the added or deleted member--and in the case of an added member, specifying its nature.

; Basically, I am looking for a whole "vocabulary" of functions to bring to bear.
; This is a lot of work, but could be very rewarding.
; To get there, I need to come up with a rational structure, fairly straightforward to understand.
; Then I have to name its pieces, and its modifying functions, in a consistent way.
; Only with consistency will I easily use my "exploration system".

; List the pieces of my carpet spec:
; - Figure struct
;    - func1 list(p q)
;    - func2(iter)
;    - p
; - Iter

; Let's improve this. We really have carpet(p0 iter)