#lang info

(define collection "review")
(define deps '("base" "review"))
(define review-exts
  '((review/define-record-reviewer should-review? review-syntax)))
