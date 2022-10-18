#lang racket/base
(require (for-syntax racket/base
                     racket/runtime-path
                     (only-in quickscript/library add-third-party-script-directory!)))

(begin-for-syntax
  (define-runtime-path script-dir "scripts")
  (add-third-party-script-directory! script-dir))
