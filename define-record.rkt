#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(provide
 define-record : Int Str)

(define-syntax (: stx)
  (raise-syntax-error stx "invalid use of :"))
(define-syntax (Int stx)
  (raise-syntax-error stx "invalid use of Int"))
(define-syntax (Str stx)
  (raise-syntax-error stx "invalid use of Str"))

(begin-for-syntax
  (define-syntax-class record-field
    #:literals (: Int Str)
    (pattern [id:id : {~or Int Str}])))

(define-syntax (define-record stx)
  (syntax-parse stx
    [(_ record-id:id (fld:record-field ...))
     #:with make-id (format-id #'record-id "make-~a" #'record-id)
     #:with (set-fld-id ...) (for/list ([fld-id-stx (in-list (syntax-e #'(fld.id ...)))])
                               (format-id fld-id-stx "set-~a" fld-id-stx))
     #'(begin
         (struct record-id (fld.id ...))
         (define make-id record-id)
         (define (set-fld-id r v)
           (struct-copy record-id r [fld.id v])) ...)]))
