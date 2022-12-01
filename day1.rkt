#lang racket

(define inp (file->string "day1-p1.txt"))

(define (parse inp)
  (map (λ(bunch)
         (map string->number (string-split bunch "\n")))
       (string-split inp "\n\n")))

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
