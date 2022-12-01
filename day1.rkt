#lang racket

(define inp (file->lines "day1-p1.txt"))

(define (parse inp)
  (if (null? inp)
      '()
      (let-values ([(l r)
                    (splitf-at inp (λ(x) (not (equal? x ""))))])
        (cons (map string->number l)
              (parse (dropf r (λ(x) (equal? x ""))))))))

(define (sum lst)
  (foldl + 0 lst))

(define (part1 inp)
  (define parsed (parse inp))
  (define sums (map sum parsed))
  (apply max sums))

(part1 inp)

(define (part2 inp)
  (define parsed (parse inp))
  (define sums (map sum parsed))
  (define sorted (sort sums >))
  (sum (take sorted 3)))

(part2 inp)
