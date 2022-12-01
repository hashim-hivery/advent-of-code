#lang racket

(define inp (file->lines "day1-p1.txt"))

(define (parse inp)
  (let loop ([gathered '()]
             [current '()]
             [remaining inp])
    (cond [(null? remaining) (cons current gathered)]
          [(equal? ""  (first remaining))
           (loop (cons current gathered) '() (rest remaining))]
          [else
           (loop gathered
                 (cons (string->number (first remaining)) current)
                 (rest remaining))])))

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
