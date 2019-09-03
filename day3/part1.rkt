#lang racket

(struct point (x y) #:transparent)
(struct line (begin end) #:transparent)
(struct claim (id lpad tpad end-x end-y) #:transparent)

(define (str->claim str)
  (match-define (list id lpad tpad width height)
    (map string->number
         (regexp-split #rx" @ |,|: |x" str 1)))
  (claim id lpad tpad (+ lpad width) (+ tpad height)))


(define (line-intersect? l1 l2)
  (match-define (list left right)
    (if (<= (line-begin l1) (line-begin l2))
        (list l1 l2)
        (list l2 l1)))
  (> (line-end left) (line-begin right)))


(define (claim-intersect? c1 c2)
  (and (line-intersect? (line (claim-lpad c1) (claim-end-x c1))
                        (line (claim-lpad c2) (claim-end-x c2)))
       (line-intersect? (line (claim-tpad c1) (claim-end-y c1))
                        (line (claim-tpad c2) (claim-end-y c2)))))


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


(define (claim->points c)
  (make-set-of-points (+ 1 (claim-lpad c))
                      (+ 1 (claim-tpad c))
                      (claim-end-x c)
                      (claim-end-y c)))


;; Claim, Claim -> Set of Points
(define (get-intersection c1 c2)
  (if (not (claim-intersect? c1 c2))
      (set)
      (set-intersect (claim->points c1) (claim->points c2))))
  
(define (find-intersects-for-claim cl cl-list acc)
  (if (empty? cl-list)
      acc
      (find-intersects-for-claim cl
                                 (rest cl-list)
                                 (set-union acc
                                            (get-intersection cl (first cl-list))))))


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
(- (current-seconds) start-time)
