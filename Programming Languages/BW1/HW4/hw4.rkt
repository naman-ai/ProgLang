
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1
(define (sequence low high stride)
  (if (> low high) null (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(= (remainder n (length xs)) 0) (car xs)]
        [#t (list-nth-mod (cdr xs) (- (remainder n (length xs)) 1))]))

;4
(define (stream-for-n-steps s n)
  (if (= n 0) null (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if (= (remainder x 2) 0) "dog.jpg" "dan.jpg") (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;8
(define (cycle-lists xs ys)
  (lambda () (cons
              (cons (car xs) (car ys))
              (cycle-lists (append (cdr xs) (list (car xs))) (append (cdr ys) (list (car ys)))))))

;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (if (< pos (vector-length vec))
                    (let ([v-ref (vector-ref vec pos)])
                      (if (and (pair? v-ref) (equal? (car v-ref) v))
                          v-ref
                          (f (+ 1 pos))))
                      #f))])
    (f 0)))

;10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-pos 0])
    (lambda (v)
      (if (vector-assoc v cache)
          (vector-assoc v cache)
          (let ([ass (assoc v xs)])
            (if ass
                (begin
                  (vector-set! cache cache-pos ass)
                  (set! cache-pos (remainder (+ cache-pos 1) n))
                  (assoc v xs))
                #f))))))