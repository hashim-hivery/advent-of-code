#lang racket

(define (read-inp)
  (define as-lists (map string->list (file->lines "day12.txt")))
  (define as-nums
    (map (λ(chars)
           (map (λ(c) (cond [(equal? c #\S) 0]
                            [(equal? c #\E) 27]
                            [else (- (char->integer c) 96)]))
            chars))
     as-lists))
  (list->vector (map list->vector as-nums)))

(define (grid-ref grid x y)
  (vector-ref (vector-ref grid y) x))

(define (within-limits grid x y)
  (and (>= x 0) (>= y 0) (< y (vector-length grid)) (< x (vector-length (vector-ref grid 0)))))

(struct pt (x y) #:transparent)

(define (possible-moves grid visited x y)
  (define cur-height (grid-ref grid x y))
  (define down (pt x (add1 y)))
  (define up (pt x (sub1 y)))
  (define left (pt (sub1 x) y))
  (define right (pt (add1 x) y))
  (for/list ([move (list down up left right)]
             #:when (and (within-limits grid (pt-x move) (pt-y move))
                         (<= (grid-ref grid (pt-x move) (pt-y move)) (add1 cur-height))
                         (not (set-member? visited move))))
    move))

(define (search grid)
  (define visited (mutable-set))
  (match-define (pt start-x start-y)
    (for*/last ([(row y) (in-indexed grid)] [(height x) (in-indexed row)])
      #:final (= height 0)
      (pt x y)))
  (set-add! visited (pt start-x start-y))
  (displayln (* (vector-length grid) (vector-length (vector-ref grid 0))))
  (let loop ([cur-posns (list (pt start-x start-y))]
             [step-num 1])
    (define next-moves
      (remove-duplicates
       (append-map (λ(p) (possible-moves grid visited (pt-x p) (pt-y p)))
                   cur-posns)))
    (for ([p next-moves]) (set-add! visited p))
    (if (member 27 (map (λ(p) (grid-ref grid (pt-x p) (pt-y p))) next-moves))
        step-num
        (loop next-moves
              (add1 step-num)))))

(search (read-inp))

(search '#(#(0 1 2 17 16 15 14 13)
           #(1 2 3 18 25 24 24 12)
           #(1 3 3 19 26 27 24 11)
           #(1 3 3 20 21 22 23 10)
           #(1 2 4 5 6 7 8 9)))
