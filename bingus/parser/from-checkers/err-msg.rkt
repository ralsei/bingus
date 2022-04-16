#lang racket/base

(provide err-msg-incorrect-def
         err-msg-signature-nonexistent
         err-msg-signature-incorrect
         err-msg-signature-multiple
         err-msg-steps
         err-msg-all-input
         err-msg-all-output
         err-msg-defined
         err-msg-use-abstractions
         err-msg-dont-process-list)

(define (err-msg-incorrect-def fn)
  (format "The \"~a\" function doesn't work correctly. Test more examples." fn))

(define (err-msg-signature-nonexistent fn)
  (format "You are missing a signature for the \"~a\" function! See step 2 of the design recipe in the videos for lecture 4." fn))

(define (err-msg-signature-incorrect fn)
  (format "Your signature for the \"~a\" function is incorrect. Does it have the right number of inputs? Do you need to support it with a data definition? See step 1 of the design recipe." fn))

(define (err-msg-signature-multiple fn)
  (format "There are multiple different signatures for the \"~a\" function!" fn))

(define (err-msg-steps location)
  (format "The step-by-step calculations in ~a seem to be incomplete or incorrect." location))

(define (err-msg-all-input name)
  (format "Test \"~a\" with every possible kind of input." name))

(define (err-msg-all-output name)
  (format "Test \"~a\" with every possible kind of output." name))

(define (err-msg-defined name)
  (format "The function named \"~a\" is not defined." name))

(define (err-msg-use-abstractions name abstractions)
  (format "Make sure the function \"~a\" uses the abstractions ~a" name abstractions))

(define (err-msg-dont-process-list name abstractions)
  (format "The definition of the function \"~a\" should not process the input list using \"empty?\", \"cons?\", \"first\", or \"rest\". Instead, give the list to ~a." name abstractions))
