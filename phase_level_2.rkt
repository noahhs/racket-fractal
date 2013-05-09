#lang racket
(require unstable/define)
 
;one more time. parse a form, define some items. how hard can it be?
;oh, well, we have to have the parent there, and get its children.
;and then we have to access them.
;let's test the basic principle of this, before moving on to anything too advanced.
;(define-syntax defs*
;  (syntax-rules ()
;    ((_ par ((a b g) ...))(begin (defs*
;                                   
;                                   (define g1 1) ...))))

;(define-for-syntax (gc* obj)(send obj get-children))
;it's this damn left-right order of operations.
;we really need to be in phase level 2. How do I get it there?
;(define-syntax apply-to-parent*
;  (syntax-rules ()
;    ((_ par form)(apply-to-children* (gc* obj)

;(define a*
;  ((_ par (



;(define-syntax objects*
;  (syntax-rules ()
;    ((_

;(define-syntax combined*
;  (syntax-rules ()
;    ((_ form)(begin (objects* form)(defs* form)))))

;(combined* (("name1" "val1" g1)("name2" "val2" g2)))

 
(define-syntax a*
  (syntax-rules ()
    ((_ ((a b) c))((a a)(a a)))))

(define-syntax picky*
  (syntax-rules ()
    ((_ ((a b)(c d))) "yes!")))

 (picky* (in-phase1 (a* ((1 2) 3))))