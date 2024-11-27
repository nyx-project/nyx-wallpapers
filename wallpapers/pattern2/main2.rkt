#lang racket
(require racket/draw)

(define *width* 1920)
(define *height* 1200)
(define displacement 400)

(define a (new svg-dc%
               [width *width*]
               [height *height*]
               [output "wallpaper2.svg"]
               [exists 'replace]))

(send a start-doc "")
(send a start-page)

(send a set-pen (make-object color% 255 255 255) 3 'solid)

(define (make-triangle w-range h-range side-limit)
  (define (find-point stpx stpy nxtx nxty)
    (let* ((x (- (random 0 (+ displacement displacement w-range)) displacement))
           (y (- (random 0 (+ displacement displacement w-range)) displacement))
           (xd1 (abs (- x stpx)))
           (yd1 (abs (- y stpy)))
           (xd2 (abs (- x nxtx)))
           (yd2 (abs (- y stpy)))
           (sl (* side-limit side-limit)))

      (if (and (<= (+ (* xd1 xd1) (* yd1 yd1)) sl)
               (<= (+ (* xd2 xd2) (* yd2 yd2)) sl)
               (< (/ (atan xd1 yd1) (* 2 pi.f)) 0.25)
               (< (/ (atan xd2 yd2) (* 2 pi.f)) 0.25)
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

(define *gradient-list*
  (list (cons (cons 0 0) (cons *width* *height*))
        (cons (cons *width* 0) (cons 0 *height*))
        (cons (cons 0 *height*) (cons *width* 0))
        (cons (cons *width* *height*) (cons 0 0))))


(for ([i (in-range 0 7000)])
  (let ((t (make-triangle *width* *height* 200))
        (g (list-ref *gradient-list* (random 0 (length *gradient-list*)))))
    (send a set-brush
          (new brush%
               [gradient
                (new linear-gradient%
                     [x0 0]
                     [y0 0]
                     [x1 *width*]
                     [y1 *height*]
                     [stops
                      (list
                       (list 0 (list-ref *color-list* 0))
                       (list 0.5 (list-ref *color-list* 1))
                       (list 1 (list-ref *color-list* 2)))])]))
    (send a draw-polygon (make-triangle *width* *height* 200))))

(send a end-page)
(send a end-doc)

