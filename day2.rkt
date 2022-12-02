#lang racket

(define (read-inp) (file->lines "day2.txt"))

(define (translate move)
  (match move
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]
    ["X" 'rock]
    ["Y" 'paper]
    ["Z" 'scissors]))

(define (score them me)
  (define result-score (play them me))
  (define shape-score (hash-ref (hash 'rock 1 'paper 2 'scissors 3) me))
  (+ result-score shape-score))

(define (get-score them result)
  ;; Soo lazy :D
  (define my-play
    (match result
      ["X"
       (for/last ([me '(rock paper scissors)])
         #:final (beats? them me) me)]
      ["Y"
       (for/last ([me '(rock paper scissors)])
         #:final (draw? me them) me)]
      ["Z"
       (for/last ([me '(rock paper scissors)])
         #:final (beats? me them) me)]))
  (score them my-play))

(define (play them me)
  (cond [(draw? me them) 3]
        [(beats? me them) 6]
        [else 0]))

(define (draw? me them)
  (equal? me them))

(define (beats? me them)
  (match (cons me them)
    [(cons 'rock 'scissors) #t]
    [(cons 'paper 'rock) #t]
    [(cons 'scissors 'paper) #t]
    [_ #f]))

(define (sum lst)
  (foldl + 0 lst))

(define (part1 game-list)
  (sum (map (λ(line)
              (let ([hands (string-split line)])
                (apply score (map translate hands))))
            game-list)))

(define (part2 game-list)
  (sum (map (λ(line)
              (match-define (list them result) (string-split line))
              (get-score (translate them) result))
            game-list)))

(module+ test
  (require rackunit)
  (check-equal?
   (part1 '("A Y"
            "B X"
            "C Z")) 15)
  (check-equal?
   (part2 '("A Y"
            "B X"
            "C Z")) 12))

(module+ main
  (define inp (read-inp))
  (part1 inp)
  (part2 inp))
