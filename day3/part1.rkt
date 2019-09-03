#lang racket
(require rackunit)

;; How many square inches of fabric are within two or more claims?


(struct point (x y))
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


;; Produce set with all points in the claim area from the claim string
(define (str->claim str)
  (match-define (list id left top width height)
    (map string->number
         (regexp-split #rx" @ |,|: |x" str 1)))
  (claim id (make-set-of-points
             (+ 1 left)
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
