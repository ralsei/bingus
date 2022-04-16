#lang racket
(provide (struct-out queue)
         empty-queue
         queue-empty?
         enqueue
         dequeue)

(struct queue (pop push) #:transparent)

(define empty-queue (queue '() '()))

(define (queue-empty? q)
  (match q
    [(queue '() '()) #t]
    [(queue _ _) #f]))

(define (enqueue x q)
  (queue (queue-pop q) (cons x (queue-push q))))

(define (dequeue q)
  (match q
    [(queue '() '()) (error 'dequeue "empty queue")]
    [(queue (cons x xs) push) (values x (queue xs push))]
    [(queue '() push) (dequeue (queue (reverse push) '()))]))

(module+ test
  (require rackunit)

  (define q (enqueue 1 (enqueue 2 (enqueue 3 empty-queue))))
  (define-values (v1 q1) (dequeue q))
  (check-equal? v1 3)
  (define-values (v2 q2) (dequeue q1))
  (check-equal? v2 2)
  (define-values (v3 q3) (dequeue q2))
  (check-equal? v3 1))
