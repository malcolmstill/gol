#lang racket/gui

(require racket/gui)

(define neighbours '((-1 -1)
                     (0 -1)
                     (1 -1)
                     (-1 0)
                     (1 0)
                     (-1 1)
                     (0 1)
                     (1 1)))

(define (make-grid m n)
  (define grid (make-vector m 0))
  (for ([i (in-range n)])
    (vector-set! grid i (make-vector n 0)))
  grid)

(define (matrix-set m x y value)
  (vector-set! (vector-ref m x) y value))

(define (matrix-ref m x y)
  (vector-ref (vector-ref m x) y))

(define (matrix-shape m)
  (values (vector-length m)
          (vector-length (vector-ref m 0))))

(define (neighbour-xy M N x y xn yn)
  (list (cond 
          [(= (+ x xn) -1) (- M 1)]
          [(= (+ x xn) M) 0]
          [else (+ x xn)])
        (cond 
          [(= (+ y yn) -1) (- N 1)]
          [(= (+ y yn) N) 0]
          [else (+ y yn)])))

#|
Rules are based on the sum of neighbour cells if we represent
a live cell as 1 and an empty cell as 0. Here we wrap around the
grid.
|#
(define (sum-neighbours grid x y)
  (define-values (M N) (matrix-shape grid))
  (foldl (λ (xyn sum)
           (+ (match xyn
                [(list xn yn) (apply matrix-ref grid (neighbour-xy M N x y xn yn))]) sum))
         0
         neighbours))

(define (update-location grid x y)
  (let ([sum (sum-neighbours grid x y)]
        [current-cell (matrix-ref grid x y)])
    (if (= current-cell 1)
        (cond
          [(< sum 2) 0]
          [(> sum 3) 0]
          [else 1])
        (cond
          [(= sum 3) 1]
          [else 0]))))

(define (update-grid grid-old grid-new)
  (define-values (M N) (matrix-shape grid-old))
  (for* ([x (in-range M)]
         [y (in-range N)])
   (matrix-set grid-new x y (update-location grid-old x y))))

(define (draw-cell dc dim x y grid)
  (define state (matrix-ref grid x y))
  (send dc set-brush (cond
                       [(= state 1) "black"]
                       [(= state 0) "white"]) 'solid)
  (send dc set-pen (cond
                       [(= state 1) "black"]
                       [(= state 0) "white"]) 1 'solid)
  (send dc draw-rectangle
        (* dim x) (* dim y)
        dim dim))

(define (single-frame grid-old grid-new canvas dc dim)
  (define-values (M N) (matrix-shape grid-old))
  (for* ([x (in-range M)]
         [y (in-range N)])
    (when (not (= (matrix-ref grid-old x y)
                  (matrix-ref grid-new x y)))
      (begin
        (draw-cell dc dim x y grid-new)))))

(define myframe%
  (class frame%
    (super-new)
    (define/override (on-exit)
      (exit))
    (define (on-close)
      (exit))
    (augment on-close)))
               
(define (gol n)
  (define width 300)
  (define height 322)
  (define dim (/ width n))
  (define grid1 (make-grid n n))
  (matrix-set grid1 1 0 1)
  (matrix-set grid1 2 1 1)
  (matrix-set grid1 0 2 1)
  (matrix-set grid1 1 2 1)
  (matrix-set grid1 2 2 1)
  (define grid2 (make-grid n n))
  (define frame (new myframe%
                     [label "Game of Life"]
                     [width width]
                     [height height]))
  (define canvas (new canvas% [parent frame] [style '(no-autoclear)]))

  (define dc (send canvas get-dc))
  (define timer-counter 0)
  (define swap #t)
  (define timer
    (new timer%
         (interval 50)
         (notify-callback
          (lambda ()
            ;(send canvas refresh-now)
            (set! timer-counter (add1 timer-counter))
            (set! swap (not swap))
            (if swap
                (begin
                  (update-grid grid2 grid1)
                  (single-frame grid2 grid1 canvas dc dim))
                (begin
                  (update-grid grid1 grid2)
                  (single-frame grid1 grid2 canvas dc dim)))))))
            
  (send frame show #t))

(gol 50)

  
