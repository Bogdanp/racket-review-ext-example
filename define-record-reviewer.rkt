#lang racket/base

#|review: ignore|#

(require review/ext
         syntax/parse/pre)

(provide
 should-review?
 review-syntax)

(define (should-review? stx)
  (syntax-parse stx
    #:datum-literals (define-record)
    [(define-record . _rest) #t]
    [_ #f]))

(define-syntax-class field-definition
  #:datum-literals (: Int Str)
  (pattern [id:id : {~or Int Str}])
  (pattern [id:id : type] #:do [(track-error #'type "invalid type")])
  (pattern e
           #:attr id #'stub
           #:do [(track-error this-syntax "invalid field definition")]))

(define-syntax-class record-definition
  (pattern (define-record record-id:id
             (record-field:field-definition ...))
           #:do [(track-binding #'record-id "make-~a")
                 (track-binding #'record-id "~a?")
                 (define record-id-sym (syntax->datum #'record-id))
                 (for ([field-id-stx (in-list (syntax-e #'(record-field.id ...)))])
                   (track-binding field-id-stx (format "~a-~~a" record-id-sym))
                   (track-binding field-id-stx (format "set-~a-~~a" record-id-sym)))]))

(define (review-syntax stx)
  (syntax-parse stx
    [d:record-definition #'d]
    [_ (begin0 stx
         (track-error stx "invalid record definition"))]))
