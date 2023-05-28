;; pelusica 0.0.6

(in-package #:calm)

;;
;; CALM version check
;; version check won't work on JSCL, since the lack of ASDF
;;
#-jscl
(let ((required-version "0.1.2")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t
            "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%"
            required-version calm-version)
    (uiop:quit)))

;;
;; the swank server is for debugging, for usage please check
;; Emacs:
;;        https://slime.common-lisp.dev/
;; Visual Studio Code
;;        https://lispcookbook.github.io/cl-cookbook/vscode-alive.html
;;
;; uncomment the following line to enable SWANK Server
#-jscl
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
;; (setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for more of this, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
(setf *calm-window-width* 600)
(setf *calm-window-height* 600)
(setf *calm-window-title* "Pelúsica")
#+jscl
(setf *calm-fps* 0)
#-jscl
(setf *calm-delay* 4)

(defparameter *music-list* '(
                             g3 b3 d4
                             g3 b3 d4
                             g3 b3 d4
                             g3 b3 d4

                             f+3 b3 d4
                             f+3 b3 d4
                             f+3 b3 d4
                             f+3 b3 d4

                             g3 b3 d4
                             g3 b3 d4
                             g3 b3 d4
                             g3 b3 d4

                             f+3 a3 c4
                             f+3 a3 c4
                             f+3 a3 c4
                             f+3 a3 c4

                             g3  b3  d4 e4 d4 c4 b3 a3
                             f+3 a3  c4 d4 c4 b3 a3 b3
                             g3  b3  d4 e4 d4 c4 b3 a3
                             f+3 a3  c4 d4 c4 b3 a3 g3

                             g3  b3  d4 e4 d4 c4 b3 a3
                             f+3 a3  c4 d4 c4 b3 a3 b3
                             g3  b3  d4 e4 d4 c4 b3 c4
                             f+3 a3  c4 d4 c4 b3 a3 g3

                             g3  b3  d4 f4 e4 c4 b3 c4
                             f+3 a3  c4 e4 d4 a3 g3 b3
                             g3  b3  d4 f4 e4 d4 c4 b3
                             f+3 a3  c4 d4 c4 b3 a3 g3
                             ))

(defparameter *started* nil)
(defparameter *paused* nil)
(defparameter *died* nil)
(defparameter *suffocated* nil)
(defparameter *music-played* 0)
(defparameter *music-index* 0)
(defparameter *level* 0)
(defparameter *ball-x* 100)
(defparameter *ball-y* 570)
(defparameter *enemy-list* (list (list 1 200) (list 2 100) (list 3 0) (list 4 -100) (list 5 -200)))

(defparameter *enemy-move-per-ms* 0.2)
(defparameter *enemy-move-tick* nil
  "to detect should we move the enemies,
the value were set when running, leave this nil while def")

(defparameter *bg-color* '(1 1 1))
(defparameter *enemy-color* '(0 0 0))
(defparameter *ball-color* '(0 0.35 0.59))
(defparameter *health-color* '(0.89 0.12 0.17))
(defparameter *not-health-color* '(0.83 0.82 0.84))

(defparameter *bg-color-light* '(1 1 1))
(defparameter *enemy-color-light* '(0 0 0))
(defparameter *ball-color-light* '(0 0.35 0.59))
(defparameter *health-color-light* '(0.89 0.12 0.17))

(defparameter *bg-color-dark* '(0.1 0.1 0.1))
;; (defparameter *bg-color-dark* '(0 0 0))

(defparameter *enemy-color-dark* '(1 1 1))

;; (defparameter *ball-color-dark* '(0.027 1 0.89))
(defparameter *ball-color-dark* '(0.1 0.45 0.69))

(defparameter *health-color-dark* '(0.89 0.12 0.17))


(defun switch-color ()
  (if (= (mod *music-played* 2) 0)
      (setf *bg-color* *bg-color-light*
            *enemy-color* *enemy-color-light*
            *ball-color* *ball-color-light*
            *health-color* *health-color-light*)
      (setf *bg-color* *bg-color-dark*
            *enemy-color* *enemy-color-dark*
            *ball-color* *ball-color-dark*
            *health-color* *health-color-dark*)))

(defun play-note ()
  (if (< (c:playing) 8)
      (progn
        (c:play-wav (concatenate 'string "assets/" (string-downcase (nth *music-index* *music-list*)) ".wav"))
        (if (>= *music-index* (1- (length *music-list*)))
            (progn
              (setf *music-index* 0)
              (incf *music-played*)
              (setf *ball-y* (- *ball-y* (* *music-played* 42))))
            (incf *music-index*)))
      (progn
        (setf *died* t
              *suffocated* t)
        (c:play-music "assets/block.wav"))))

(defun reset-game ()
  (setf
   *enemy-list* (list (list 1 200) (list 2 100) (list 3 0) (list 4 -100) (list 5 -200))
   *enemy-move-per-ms* 0.2
   *enemy-move-tick* nil
   *ball-x* 100
   *ball-y* 570
   *music-played* 0
   *music-index* 0
   *level* 0
   *died* nil
   *suffocated* nil
   *calm-redraw* t
   ))

(defun on-keydown (key)
  (cond
    ((c:keq key :SCANCODE-P)
     (setf
      *paused* (not *paused*)
      *enemy-move-tick* nil)))

  (when (and (not *died*) (not *paused*) *started*)
    (cond
      ((c:keq key :SCANCODE-RIGHT :SCANCODE-K :SCANCODE-L)
       (when (and (< *ball-x* 450) (play-note))
         (incf *ball-x* 100)))
      ((c:keq key :SCANCODE-LEFT :SCANCODE-J :SCANCODE-H)
       (when (and (> *ball-x* 100) (play-note))
         (decf *ball-x* 100)))
      (t (format t "~%KEY PRESSED: ~A~%" key))))

  (unless *started*
    (cond
      ((c:keq key :SCANCODE-RETURN :SCANCODE-LEFT :SCANCODE-RIGHT)
       (setf *started* t))))

  (when *died*
    (cond
      ((c:keq key :SCANCODE-RETURN)
       (reset-game)))))

(defun on-fingerup (&key x  y dx dy pressure finger-id)
  (declare (ignore y dx dy pressure finger-id))



  (when (and (not *died*) (not *paused*) *started*)
    (cond
      ((> x 0.5)
       (when (and (< *ball-x* 450) (play-note))
         (incf *ball-x* 100)))
      ((< x 0.5)
       (when (and (> *ball-x* 100) (play-note))
         (decf *ball-x* 100)))
      ((= x 0.5) ;; I wonder who can actually do this?
       (setf
        *paused* (not *paused*)
        *enemy-move-tick* nil))
      (t (format t "~%Finger pressed, x: ~A~%" x))))

  (unless *started* (setf *started* t))

  (when *died*
    (reset-game)))

(defun draw-welcome-screen ()
  (c:set-source-rgb 1 1 1)
  (c:paint)
  (c:select-font-family "Open Sans" :normal :normal)
  (c:set-font-size 24)
  (apply #'c:set-source-rgb *ball-color*)
  (c:move-to 120 240)
  (c:show-text "press ")
  (apply #'c:set-source-rgb *health-color*)
  #-jscl
  (cond
    ((uiop:os-macosx-p) (c:show-text "return"))
    (t (c:show-text "Enter")))
  #+jscl
  (if (= (#j:window:navigator:userAgent:indexOf "Mac OS") -1)
      (c:show-text "Enter")
      (c:show-text "return"))
  (apply #'c:set-source-rgb *ball-color*)
  (c:show-text " to start or restart")
  (c:move-to 120 290)
  (c:show-text "press ")
  (apply #'c:set-source-rgb *health-color*)
  (c:show-text "left")
  (apply #'c:set-source-rgb *ball-color*)
  (c:show-text " or ")
  (apply #'c:set-source-rgb *health-color*)
  (c:show-text "right")
  (apply #'c:set-source-rgb *ball-color*)
  (c:show-text " to move")

  #+jscl
  (when (or (<= #j:screen:width 800) (<= #j:screen:height 600))
    (c:move-to 120 370)
    (c:show-text "If you don't have keyboard now,")
    (c:move-to 120 420)
    (c:show-text "touch the left or right half of screen")))

(defun draw-died-screen ()
  (if *suffocated*
      (apply #'c:set-source-rgb *not-health-color*)
      (apply #'c:set-source-rgb *health-color*))
  (c:arc 300 300 50 0 (* 2 pi))
  (c:fill-path)
  (if *suffocated*
      (apply #'c:set-source-rgb *health-color*)
      (c:set-source-rgb 1 1 1))
  (c:move-to 280 280)
  (c:line-to 320 320)
  (c:stroke)
  (c:move-to 320 280)
  (c:line-to 280 320)
  (c:stroke)
  )

(defun draw-health-bar ()
  (c:save)
  (c:set-line-width 10)
  (let ((bar-points 8)
        (bar-width 60))

    (c:move-to 580 20)

    (if (or *died* (= (c:playing) 8))
        (apply #'c:set-source-rgb *not-health-color*)
        (apply #'c:set-source-rgb *health-color*))
    (c:line-to (- 580 (* (/ bar-width bar-points)
                         (if *died* 0 (- 8 (c:playing))))) 20)
    (c:stroke))
  (c:restore))

(defun recal-level ()
  (setf
   *level* (* 40 (/ *music-index* (length *music-list*)))
   *enemy-move-per-ms* (max 0.22 (/
                                  (log (1+ *level*)) 4))))

(defun draw-level-border ()
  (apply #'c:set-source-rgb *ball-color*)
  (c:set-line-width 10)
  (c:set-line-cap :round)
  (cond
    ((> *level* 30)
     (c:move-to 0 0)
     (c:line-to 0 600)
     (c:line-to 600 600)
     (c:line-to 600 0)

     (c:move-to 600 0)
     (c:line-to (- 600 (* 600 (/ (- *level* 30) 10))) 0)
     (c:stroke))

    ((> *level* 20)
     (c:move-to 0 0)
     (c:line-to 0 600)
     (c:line-to 600 600)

     (c:move-to 600 600)
     (c:line-to 600 (- 600 (* 600 (/ (- *level* 20) 10))))
     (c:stroke))

    ((> *level* 10)
     (c:move-to 0 0)
     (c:line-to 0 600)

     (c:move-to 0 600)
     (c:line-to (* 600 (/ (- *level* 10) 10)) 600)
     (c:stroke))

    ((> *level* 0)
     (c:move-to 0 0)
     (c:line-to 0 (* 600 (/ (- *level* 0) 10)))
     (c:stroke))
    ))


(defun draw-enemy (x y)
  (c:save)
  (apply #'c:set-source-rgb *enemy-color*)
  (c:move-to x y)
  (c:line-to x (- y 1))
  (c:stroke)
  (c:restore))

(defun draw-enemies ()
  (when (< *music-played* 2)
    (let ((should-move
            (and
             (not *died*)
             (not *paused*))))
      (loop
        for enemy in *enemy-list*
        for x = (* 100 (car enemy))
        for y = (cadr enemy)
        for i = 0 then (incf i)
        if (< y 630)
          do
             (draw-enemy x y)
             ;; test if hit the ball, mercy 10
             (when (and (not *died*) (= *ball-x* x) (< (abs (- *ball-y* y)) 10))
               (setf *died* t)
               (c:play-music "assets/chord.wav"))
             (when should-move
               ;; move downward
               (incf
                (cadr (nth i *enemy-list*))
                (if *enemy-move-tick*
                    (* (- (c:get-ticks) *enemy-move-tick*) *enemy-move-per-ms*)
                    1)))
        else
          do
             ;; randomly reset the Y position
             (setf (cadr (nth i *enemy-list*)) (* (1+ (random 400)) -1)))
      (when should-move (setf *enemy-move-tick* (c:get-ticks))))))

(defun draw-ball ()
  (c:save)
  (c:set-line-width 30)
  (apply #'c:set-source-rgb *ball-color*)
  (c:move-to *ball-x*  *ball-y*)
  (c:line-to *ball-x* (- *ball-y* 1))
  (c:stroke)
  (c:restore))


(defun draw-forever ()
  (unless *started*
    (draw-welcome-screen)
    (setf *calm-redraw* nil)
    (return-from draw-forever))

  (apply #'c:set-source-rgb *bg-color*)
  (c:paint)

  ;; lines
  (c:set-source-rgb 0.83 0.82 0.84)
  (loop for x from 1 to 5
        do
           (c:move-to (* 100 x) 0)
           (c:line-to (* 100 x) 600)
           (c:stroke))

  (c:set-line-cap :round)
  (c:set-line-width 30)
  (switch-color)
  (draw-health-bar)
  (draw-enemies)
  (recal-level)
  (draw-level-border)
  (draw-ball)
  (when *died*
    (draw-died-screen))

  (when (or *paused* *died* *suffocated*)
    (setf *calm-redraw* nil)))
