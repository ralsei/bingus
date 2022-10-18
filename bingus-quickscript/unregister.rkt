#lang racket/base
(require quickscript/library
         racket/runtime-path)

;;; To remove the script directory from Quickscript's library,
;;; run this file in DrRacket, or on the command line with
;;; $ racket -l bingus-quickscript/register

(define-runtime-path script-dir "scripts")
(remove-third-party-script-directory! script-dir)
