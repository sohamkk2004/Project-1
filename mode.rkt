#lang racket

(provide prompt?)  ;; Make prompt? available to other modules

(define prompt?
  (let ((args (current-command-line-arguments)))
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))


