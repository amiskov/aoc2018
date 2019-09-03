#lang racket
(require rackunit)

;; How many square inches of fabric are within two or more claims?


(struct point (x y) #:transparent)
(struct claim (id points))


;; Fill x coordinates for pointers in a row
(define (fill-xs x y end-x)
  (cond
    [(= x end-x) (set (point end-x y))]
    [else
     (set-union (set (point x y))
                (fill-xs (add1 x) y end-x))]))


;; Create list of all points in claim area
(define (make-set-of-points x y end-x end-y)
  (cond
    [(and (= y end-y))
     (fill-xs x y end-x)]
    [else
     (set-union (fill-xs x y end-x)
                (make-set-of-points x (add1 y) end-x end-y))]))
(check-equal? (make-set-of-points 1 2 5 6)
              (set (point 1 2) (point 2 2) (point 3 2) (point 4 2) (point 5 2)
                   (point 1 3) (point 2 3) (point 3 3) (point 4 3) (point 5 3)
                   (point 1 4) (point 2 4) (point 3 4) (point 4 4) (point 5 4)
                   (point 1 5) (point 2 5) (point 3 5) (point 4 5) (point 5 5)
                   (point 1 6) (point 2 6) (point 3 6) (point 4 6) (point 5 6)))


;; Produce set with all points in the claim area from the claim string
(define (str->claim str)
  (match-define (list id left top width height)
    (map string->number
         (regexp-split #rx" @ |,|: |x" str 1)))
  (claim id (make-set-of-points (+ 1 left)
                      (+ 1 top)
                      (+ left width)
                      (+ top height))))


;; Compares points from one claim againts others
(define (find-intersects-for-claim cl cl-list acc)
  (if (empty? cl-list)
      acc
      (find-intersects-for-claim cl
                     (rest cl-list)
                     (set-union acc (set-intersect (claim-points cl)
                                                   (claim-points (first cl-list)))))))

(define (find-all-intersects data acc)
  (if (empty? data) acc
      (let ([f (first data)]
            [r (rest data)])
        (display (claim-id f)) (display " ")
        (find-all-intersects r
                      (find-intersects-for-claim f r acc)))))


(define data (map str->claim (file->lines "input.txt")))

(define start-time (current-seconds))
(set-count (find-all-intersects data (set)))
(quotient/remainder (- (current-seconds) start-time) 60)
