#lang racket

(define (read-inp) (file->string "day6.txt"))

(define (no-dups? lst) (= (length lst) (length (remove-duplicates lst))))

(define (solve inp n)
  (define (acc remaining idx)
    (if (no-dups? (take remaining n))
        (+ idx n)
        (acc (rest remaining) (add1 idx))))
  (acc (string->list inp) 0))

(define (part1 inp) (solve inp 4))
(define (part2 inp) (solve inp 14))

(module+ main
  (part1 (read-inp))
  (part2 (read-inp)))
