#lang racket
(require racket/draw)
(require racket/list)

;; inspired by the cover of this ep:
;;     https://trekkietrax.bandcamp.com/album/make-me-feel-remixes
;;     Make Me Feel Remixes - Lolica Tonica
;; but the result aren't even half as good. might revisit this someday.

(define *width* 1920)
(define *height* 1200)
(define displacement 400)

(define a (new svg-dc%
               [width *width*]
               [height *height*]
               [output "wallpaper3.svg"]
               [exists 'replace]))

(send a start-doc "")
(send a start-page)

(send a set-pen (make-object color% 255 255 255) 3 'transparent)

(define (make-triangle w-range h-range side-limit)
  (define (find-point stpx stpy nxtx nxty)
    (let* ((x (- (random 0 (+ displacement displacement w-range)) displacement))
           (y (- (random 0 (+ displacement displacement w-range)) displacement))
           (xd1 (abs (- x stpx)))
           (yd1 (abs (- y stpy)))
           (xd2 (abs (- x nxtx)))
           (yd2 (abs (- y stpy)))
           (sl (* 4 side-limit side-limit)))

      (if (and (<= (+ (* xd1 xd1) (* yd1 yd1)) sl)
               (<= (+ (* xd2 xd2) (* yd2 yd2)) sl)
               )
          (cons x y)
          (find-point stpx stpy nxtx nxty))))
  (let* ((stpx (- (random 0 (+ displacement displacement w-range)) displacement))
         (stpy (- (random 0 (+ displacement displacement h-range)) displacement))
         (a0 (* (random) 2 pi.f))
         ;; dx / dl = cos a1
         ;; dy / dl = sin a1
         (nxtx (+ stpx (* (+ (* (random) 5) -2.5 side-limit) (cos a0))))
         (nxty (+ stpy (* (+ (* (random) 5) -2.5 side-limit) (sin a0))))
         (p (find-point stpx stpy nxtx nxty)))
    (list (cons stpx stpy)
          (cons nxtx nxty)
          p)))

(define *color-list*
  (list
   (make-object color% 0 0 255 1)
   (make-object color% 255 0 255 1)
   (make-object color% 0 255 255 1))
  )
(define *color-list-list*
  (permutations *color-list*))

(define (get-gradient)
  (let* ((a (* (random) 2 pi.f))
         (dx (* *width* (cos a)))
         (dy (* *height* (sin a))))
    (cons (cons 0 0) (cons dx dy))))
        
#;(define *gradient-list*
  (list (cons (cons 0 0) (cons *width* *height*))
        (cons (cons *width* 0) (cons 0 *height*))
        (cons (cons 0 *height*) (cons *width* 0))
        (cons (cons *width* *height*) (cons 0 0))
        (cons (cons 0 0) (cons *width* 0))
        (cons (cons 0 0) (cons 0 *height*))
        (cons (cons *width* 0) (cons 0 0))
        (cons (cons 0 *height*) (cons 0 0))
        ))


(for ([i (in-range 0 7000)])
  (let ((t (make-triangle *width* *height* 200))
        #;(g (list-ref *gradient-list* (random 0 (length *gradient-list*))))
        (g (get-gradient))
        (*color-list* (list-ref *color-list-list* (random 0 (length *color-list-list*)))))
    (send a set-brush
          (new brush%
               [gradient
                (new linear-gradient%
                     [x0 (car (car g))]
                     [y0 (cdr (car g))]
                     [x1 (car (cdr g))]
                     [y1 (cdr (cdr g))]
                     [stops
                      (list
                       (list 0 (list-ref *color-list* 0))
                       (list 0.05 (list-ref *color-list* 1))
                       (list 0.1 (list-ref *color-list* 2))
                       (list 0.15 (list-ref *color-list* 0))
                       (list 0.2 (list-ref *color-list* 1))
                       (list 0.25 (list-ref *color-list* 2))
                       (list 0.3 (list-ref *color-list* 1))
                       (list 0.35 (list-ref *color-list* 2))
                       (list 0.4 (list-ref *color-list* 0))
                       (list 0.45 (list-ref *color-list* 1))
                       (list 0.5 (list-ref *color-list* 1))
                       (list 0.55 (list-ref *color-list* 2))
                       (list 0.6 (list-ref *color-list* 0))
                       (list 0.65 (list-ref *color-list* 1))
                       (list 0.7 (list-ref *color-list* 2))
                       (list 0.75 (list-ref *color-list* 1))
                       (list 0.8 (list-ref *color-list* 2))
                       (list 0.85 (list-ref *color-list* 0))
                       (list 0.9 (list-ref *color-list* 1))
                       (list 0.95 (list-ref *color-list* 2))
                       (list 1 (list-ref *color-list* 0))

                       )])]))
    (send a draw-polygon (make-triangle *width* *height* 200))))

(send a end-page)
(send a end-doc)

