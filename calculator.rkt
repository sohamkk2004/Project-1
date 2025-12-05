#lang racket

(require "mode.rkt")  ;; imports prompt? from mode.rkt

;; history: most recent first (cons new results)
(define history '())

;; helper to print/store result
(define (add-to-history result)
  (set! history (cons result history))
  (let ((id (length history)))
    (displayln (string-append (number->string id) ": "
                              (number->string (real->double-flonum result))))))

;; tokenize: walk the string, produce tokens like "+", "*", "/", "-", "$3", "42"
(define (tokenize s)
  (define n (string-length s))
  (define (next i acc)
    (if (>= i n)
        (reverse acc)
        (let ((ch (string-ref s i)))
          (cond
            [(char-whitespace? ch) (next (+ i 1) acc)]
            [(or (char=? ch #\+) (char=? ch #\*) (char=? ch #\/) (char=? ch #\-))
             (next (+ i 1) (cons (string ch) acc))]
            [(char=? ch #\$)
             ;; read digits after $
             (let loop ((j (+ i 1)))
               (if (and (< j n) (char-numeric? (string-ref s j)))
                   (loop (+ j 1))
                   (let ((tok (substring s i j)))
                     (next j (cons tok acc)))))]
            [(char-numeric? ch)
             ;; read multi-digit number
             (let loop ((j (+ i 1)))
               (if (and (< j n) (char-numeric? (string-ref s j)))
                   (loop (+ j 1))
                   (let ((tok (substring s i j)))
                     (next j (cons tok acc)))))]
            [else
             ;; invalid single char token: return as-is (will be flagged as invalid later)
             (next (+ i 1) (cons (string ch) acc))]))))
  (next 0 '()))

;; eval-tokens: returns (list 'ok value remaining-tokens) or (list 'err "Error: ...")
(define (eval-tokens tokens)
  (if (null? tokens)
      (list 'err "Error: Invalid Expression")
      (let* ((token (car tokens))
             (rest  (cdr tokens)))
        (cond
          ;; binary +
          [(string=? token "+")
           (let ((l (eval-tokens rest)))
             (if (eq? (car l) 'err) l
                 (let ((lval (cadr l))
                       (lrest (caddr l)))
                   (let ((r (eval-tokens lrest)))
                     (if (eq? (car r) 'err) r
                         (list 'ok (+ lval (cadr r)) (caddr r)))))))]
          ;; binary *
          [(string=? token "*")
           (let ((l (eval-tokens rest)))
             (if (eq? (car l) 'err) l
                 (let ((lval (cadr l))
                       (lrest (caddr l)))
                   (let ((r (eval-tokens lrest)))
                     (if (eq? (car r) 'err) r
                         (list 'ok (* lval (cadr r)) (caddr r)))))))]
          ;; binary / (integer division, error on divide by zero)
          [(string=? token "/")
           (let ((l (eval-tokens rest)))
             (if (eq? (car l) 'err) l
                 (let ((lval (cadr l))
                       (lrest (caddr l)))
                   (let ((r (eval-tokens lrest)))
                     (if (eq? (car r) 'err) r
                         (let ((rval (cadr r)))
                           (if (= rval 0)
                               (list 'err "Error: Division by zero")
                               (list 'ok (quotient lval rval) (caddr r)))))))))]
          ;; unary -
          [(string=? token "-")
           (let ((v (eval-tokens rest)))
             (if (eq? (car v) 'err) v
                 (list 'ok (-(cadr v)) (caddr v))))]
          ;; history reference $n
          [(and (>= (string-length token) 2) (char=? (string-ref token 0) #\$))
           (let ((num-str (substring token 1)))
             (let ((id (string->number num-str)))
               (if (or (not id) (<= id 0) (> id (length history)))
                   (list 'err "Error: Invalid history ID")
                   ;; history is most-recent-first, so reverse to get insertion order
                   (list 'ok (list-ref (reverse history) (- id 1)) rest))))]
          ;; number literal
          [else
           (let ((num (string->number token)))
             (if (not num)
                 (list 'err "Error: Invalid Expression")
                 (list 'ok num rest)))]))))

;; eval-prefix: top-level evaluation: ensure no leftover tokens after full parse
(define (eval-prefix expr)
  (let* ((tokens (tokenize expr))
         (res (eval-tokens tokens)))
    (if (eq? (car res) 'err)
        (cadr res)
        (let ((val (cadr res))
              (rem (caddr res)))
          (if (null? rem)
              (list 'ok val)
              (list 'err "Error: Invalid Expression"))))))

;; main loop
(define (calculator-loop)
  (when prompt? (display "> "))
  (let ((input (read-line)))
    (unless (string=? input "quit")
      (let ((out (eval-prefix input)))
        (if (eq? (car out) 'err)
            (displayln (cadr out))
            (add-to-history (cadr out))))
      (calculator-loop))))

(calculator-loop)

