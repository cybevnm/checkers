;;;; Checkers game with AI
;;;; Copyright (C) 2014 cybevnm

(defpackage checkers
  (:use :cl
        :anaphora
        :alexandria
        :optima))
(in-package checkers)

(defun handle-slime-requests ()
  #+swank
  (let ((connection (or swank::*emacs-connection* 
                        (swank::default-connection))))
    ;;(when (and connection (not (eql swank:*communication-style* :spawn)))
    ;;  (swank::handle-requests connection t))))
    (when connection      
      (swank::handle-requests connection t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass board ()
  ((squares :initarg :squares :reader board/squares)))
(defun board/square (board x y)
  (aref (board/squares board) x y))
(defun board/square-or-nil (board x y)
  (when (and (< -1 x (board/width board))
             (< -1 y (board/height board)))
    (board/square board x y)))
(defun (setf board/square) (val board x y)
  (setf (aref (board/squares board) x y) val))
(defun board/kill-check (board x y)
  (symbol-macrolet ((square (board/square board x y)))
    (assert (not (eq square :empty)))
    (setf square :empty)))
(defun board/opposite-check (square)
  (ecase square
    (:white-check :black-check)
    (:black-check :white-check)))
(defun board/color (x y)
  (if (xor (evenp x) (evenp y)) :white :black))
(defun $board/dimension (board axis-number)
  (array-dimension (board/squares board) axis-number))
(defun board/width (board)
  ($board/dimension board 0))
(defun board/height (board)
  ($board/dimension board 1))
(defmacro doboard ((x y square board) &body body)
  `(dotimes (,y (board/height ,board))
     (symbol-macrolet ((,square (board/square ,board x y)))
       (dotimes (,x (board/width ,board))
         ,@body))))
(defun board/make-empty (&optional (w 10) (h 10))
  (make-instance 'board 
                 :squares (make-array `(,w ,h) :initial-element :empty)))
;;; TODO: rename board/make
(defun make-board ()
  (alet (board/make-empty)
    (doboard (x y square it)
      (setf square 
            (cond
              ((and (< y 4) (eql (board/color x y) :black)) :white-check)
              ((and (> y 5) (eql (board/color x y) :black)) :black-check)
              (t :empty))))
    it))
(defun make-board-2 ()
  (let ((board (board/make-empty 7 7))
        (squares #2A((:empty :empty :empty :empty :empty :empty :empty)
                     (:empty :empty :empty :empty :empty :empty :empty)
                     (:empty :empty :empty :empty :white-check :empty :empty)
                     (:empty :empty :empty :black-check :empty :empty :empty)
                     (:empty :empty :empty :empty :white-check :empty :empty)
                     (:empty :empty :empty :empty :empty :empty :empty)
                     (:empty :empty :empty :empty :empty :empty :white-check))))
    (doboard (x y square board)
      (setf square (aref squares x y)))
    board))
(defun board/clone (board)
  (alet (board/make-empty (board/width board) (board/height board))
    (doboard (x y square it)
      (setf square (board/square board x y)))
    it))
(defparameter *squares-chars* '(:white "□" :black "■"))
(defparameter *inverted-squares-chars* '(:white "■" :black "□"))
(defun $board/square-char (color &key invert-colors)
  (getf (if invert-colors *inverted-squares-chars* *squares-chars*) color))
(defparameter *check-chars* '(:white-check "○" :black-check "●"))
(defparameter *inverted-check-chars* '(:white-check "●" :black-check "○"))
(defun $board/check-char (check &key invert-colors)
  (getf (if invert-colors *inverted-check-chars* *check-chars*) check))
(defun $board/char (board x y &key invert-colors)
  (alet (board/square board x y)
    (if (eql it :empty)
        ($board/square-char (board/color x y) :invert-colors invert-colors)
        ($board/check-char it :invert-colors invert-colors))))
(defun board/print (board &key invert-colors)
  (doboard (x y square board)
    (let ((inv-y (- (board/height board) y 1)))
      (when (zerop x) (format t "~&"))
      (format t "~A " ($board/char board x inv-y :invert-colors invert-colors)))))
(defun board/move-check (board src-x src-y tgt-x tgt-y)
  (symbol-macrolet ((src-square (board/square board src-x src-y))
                    (tgt-square (board/square board tgt-x tgt-y)))
    (assert (not (eq src-square :empty)))
    (assert (eq tgt-square :empty))
    (setf tgt-square src-square)
    (setf src-square :empty)))
(defun board/check-direction (square)
  (ecase square
    (:white-check :up)
    (:black-check :down)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun square-has-same-check-p (board curr-check pos)
  (eql (board/square board (car pos) (cdr pos)) curr-check))
(defun square-empty-p (board curr-check pos)
  (eql (board/square board (car pos) (cdr pos)) :empty))
(defun distance (src-pos tgt-pos)
  (cons (- (car tgt-pos) (car src-pos))
        (- (cdr tgt-pos) (cdr src-pos))))
(defun hor-distance (board curr-check src-pos tgt-pos)
  (abs (car (distance src-pos tgt-pos))))
(defun ver-forward-distance (board curr-check src-pos tgt-pos)
  (alet (cdr (distance src-pos tgt-pos))
    (ecase (board/check-direction 
            (board/square board (car src-pos) (cdr src-pos)))
      (:up it)
      (:down (- it)))))
(defun ver-distance (board curr-check src-pos tgt-pos)
  (abs (cdr (distance src-pos tgt-pos))))
(defun half-distance (src-pos tgt-pos)
  (alet (distance src-pos tgt-pos)
    (cons (/ (car it) 2) (/ (cdr it) 2))))
(defun shifted-pos (src-pos vec)
  (cons (+ (car src-pos) (car vec)) 
        (+ (cdr src-pos) (cdr vec))))
(defun half-way-square-has-opposite-check-p (board curr-check src-pos tgt-pos)
  (alet (shifted-pos src-pos (half-distance src-pos tgt-pos))
    (eql (board/square-or-nil board (car it) (cdr it))
         (board/opposite-check curr-check))))
(defun move-check (board curr-check src-pos tgt-pos)
  (board/move-check board 
                    (car src-pos) (cdr src-pos) 
                    (car tgt-pos) (cdr tgt-pos)))
(defun kill-half-way-check (board curr-check src-pos tgt-pos)
  (alet (shifted-pos src-pos (half-distance src-pos tgt-pos))
    (board/kill-check board (car it) (cdr it))))
;; TODO: seems like it's easire just define moves hierarcy
;;
;;            move
;;           /    \
;;   basic-move  attack-move
;;
;; and so on...   
(defclass move ()
  ((predicate :initarg :predicate :reader move/predicate)
   (action :initarg :action :reader move/action)))
(defparameter *moves* nil)
(defun move/validp (move board curr-check src-pos tgt-pos)
  (funcall (move/predicate move) board curr-check src-pos tgt-pos))
(defun move/apply (move board curr-check src-pos tgt-pos)
  (funcall (move/action move) board curr-check src-pos tgt-pos))
(defun move/remove (name)
  (remf *moves* name))
(eval-when (:compile-toplevel :load-toplevel)
  (defun move/parse-pred-func-def (def)
    (values (first def) (cdr (butlast def)) (lastcar def)))
  (defun move/gen-pred-func-call (def)
    (multiple-value-bind (name args-rest result) (move/parse-pred-func-def def)
      `(eql ,(append (list name) 
                     '(board curr-check) 
                     args-rest)
            ,result)))
  (defun move/parse-act-func-def (def)
    (values (first def) (cdr def)))
  (defun move/gen-act-func-call (def)
    (multiple-value-bind (name args-rest) (move/parse-act-func-def def)
      (append (list name) '(board curr-check) args-rest))))
(defmacro defmove (name type (&rest patterns) (&rest actions))
  `(setf (getf *moves* ',name)  
         (make-instance 
          'move 
          :predicate (lambda (board curr-check source target)
                       (and ,@(loop for p in patterns
                                 collecting (move/gen-pred-func-call p))))
          :action (lambda (board curr-check source target)
                   ,@(loop for a in actions
                        collecting (move/gen-act-func-call a))))))
(defmove basic :simple
    ((square-has-same-check-p source t) ;; can be removed ?
     (square-empty-p target t)     ;; 
     (hor-distance source target 1)
     (ver-forward-distance source target 1))
    ((move-check source target)))
(defmove attack :recursive
    ((square-has-same-check-p source t) ;; can be removed ?
     (square-empty-p target t)         ;; 
     (hor-distance source target 2)
     (ver-distance source target 2)
     (half-way-square-has-opposite-check-p source target t))
    ((kill-half-way-check source target)
     (move-check source target)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ai ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass action ()
  ((parent :initarg :parent :initform nil)
   (board :initarg :board :reader action/board)
   (check :initarg :check :reader action/check)
   (children :initarg :children :initform nil :accessor action/children)
   (depth :initarg :depth :initform 0 :reader action/depth)
   (rate :initarg :rate :reader action/rate)
   (src :initarg :src :reader action/src)
   (tgt :initarg :tgt :reader action/tgt)
   (move :initarg :move :reader action/move)))
(defun ai/rate-board (board curr-check)
  (let ((rate 0))
    (doboard (x y square board)
      (when (eql curr-check square)
        (incf rate)))
    rate))
(defun ai/enum-moves (parent)
  (let ((moves)
        (board (action/board parent))
        (curr-check (board/opposite-check (action/check parent)))
        (curr-depth (1+ (action/depth parent))))
    (doplist (key move *moves*)
      (doboard (src-x src-y src-square board)
        (doboard (tgt-x tgt-y tgt-square board)
          (let ((src (cons src-x src-y))
                (tgt (cons tgt-x tgt-y)))
            (when (move/validp move board curr-check src tgt)
              (alet (board/clone board)
                (move/apply move it curr-check src tgt)
                (push (make-instance 'action
                                     :parent parent
                                     :board it 
                                     :check curr-check
                                     :depth curr-depth
                                     :rate (ai/rate-board it curr-check)
                                     :src src
                                     :tgt tgt
                                     :move move)
                      moves)))))))
    moves))
(defparameter *max-depth* 4)
(defun ai/build-subtree (parent)
  (unless (eql (action/depth parent) *max-depth*)
    (let ((actions (ai/enum-moves parent)))
      (setf (action/children parent) actions) 
      (dolist (a actions)
        (ai/build-subtree a)))))

(defun ai/build-tree (board)
  (alet (make-instance 'action 
                        :check :white-check
                        :board board
                        :rate (ai/rate-board board :white-check))
    (ai/build-subtree it)
    it))
(defun ai/max-rate-action (a b)
  (if (> (action/rate a) (action/rate b))
      a
      b))
(defun ai/rate-subtree (parent)
  (let* ((children (action/children parent)))
    (if children
        (ai/rate-subtree (reduce #'ai/max-rate-action children))
        (if (eql (action/check parent) :black-check)
            (action/rate parent)
            0))))
(defun ai/max-rate-subtree (a b)
  (if (> (ai/rate-subtree a) (ai/rate-subtree b))
      a
      b))
(defun ai/find-best-action (parent)
  (alet (action/children parent)
    (reduce #'ai/max-rate-subtree it)))
(defun ai/process (board)
  (let* ((tree (ai/build-tree board))
         (action (ai/find-best-action tree)))
    (move/apply (action/move action) 
                board
                :black-check
                (action/src action)
                (action/tgt action))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *window-width* 400)
(defparameter *window-height* 400)
(defparameter *square-color* (list :white (sdl:color :r #xE8 :g #xD0 :b #xAA)
                                   :black (sdl:color :r #xA6 :g #x7D :b #x5D)))
(defparameter *check-color* (list :white-check (sdl:color :r #xF6 :g #xC6 :b #x48)
                                  :black-check (sdl:color :r #x7A :g #x50 :b #x44)))
(defparameter *check-pin-color* (list :white-check (sdl:color :r #xFF :g #xD6 :b #x58)
                                      :black-check (sdl:color :r #x8A :g #x60 :b #x54)))
(defparameter *src-square-x* 3)
(defparameter *src-square-y* 3)
(defparameter *tgt-square-x* nil)
(defparameter *tgt-square-y* nil)
(defun window/tgt-square-defined-p ()
  (and *tgt-square-x* *tgt-square-y*))
(defparameter *board* (make-board))
(defun window/calc-square-width (board)
  (round (/ *window-width* (board/width board))))
(defun window/calc-square-height (board)
  (round (/ *window-height* (board/height board))))
(defun window/draw-check (x y w h square)
  (sdl:draw-filled-circle-* (+ (* x w) (round (/ w 2)))
                            (+ (* y h) (round (/ h 2)))
                            (round (/ (/ (+ w h) 2) 3))
                            :color (getf *check-color* square))
  (sdl:draw-filled-circle-* (+ (* x w) (round (/ w 2))) 
                            (+ (* y h) (round (/ h 2)))
                            (round (/ (/ (+ w h) 2) 6))
                            :color (getf *check-pin-color* square)))
(defun window/calc-inv-y (board y)
  (- (board/height board) y 1))
(defun window/draw-selected-square (board x y square-w square-h color)
  (sdl:draw-filled-circle-* (+ (* x square-w) (round (/ square-w 2))) 
                            (+ (* (window/calc-inv-y board y) square-h) 
                               (round (/ square-h 2)))
                            (round (/ (/ (+ square-w square-h) 2) 5))
                            :color color))
(defun window/draw-src-square (board square-w square-h)
  (window/draw-selected-square board 
                               *src-square-x* *src-square-y* 
                               square-w square-h 
                               sdl:*blue*))
(defun window/draw-tgt-square (board square-w square-h)
  (when (and *tgt-square-x* *tgt-square-y*)
    (window/draw-selected-square board
                                 *tgt-square-x* *tgt-square-y*
                                 square-w square-h
                                 sdl:*red*)))
(defun window/draw-move-variants (board square-w square-h)
  (doboard (x y square board)
    (doplist (key move *moves*)
      (let ((src-square (board/square board *src-square-x* *src-square-y*)))
        (when (and (eql src-square :white-check) 
                   (move/validp move 
                                board 
                                src-square
                                (cons *src-square-x* *src-square-y*) 
                                (cons x y)))
          (sdl:draw-box-* (* x square-w)
                          (* (window/calc-inv-y board y) square-h)
                          square-w 
                          square-h
                          :color (sdl:color :r 60 :g 60 :b 60)
                          :alpha 125))))))
(defun window/draw-background (board square-w square-h)
  (doboard (x y square board)
      (let* ((inv-y (window/calc-inv-y board y))
             (window-x (* x square-w))
             (window-y (* inv-y square-h)))
        (sdl:draw-box-* window-x  window-y square-w square-h
                        :color (getf *square-color* (board/color x y))))))
(defun window/draw-checks (board square-w square-h)
  (doboard (x y square board)
    (alet (window/calc-inv-y board y)
      (when (not (eql square :empty))
        (window/draw-check x it square-w square-h square)))))
(defun window/draw-game (board)
  (let ((square-w  (window/calc-square-width board))
        (square-h (window/calc-square-height board)))
    (window/draw-background board square-w square-h)
    (window/draw-move-variants board square-w square-h)
    (window/draw-checks board square-w square-h)
    (window/draw-src-square board square-w square-h)
    (window/draw-tgt-square board square-w square-h)))
(defun window/move-selection (board dx dy)
  (if (and *tgt-square-x* *tgt-square-y*)
      (setf *tgt-square-x* 
            (clamp (+ *tgt-square-x* dx) 0 (1- (board/width board)))
            *tgt-square-y*
            (clamp (+ *tgt-square-y* dy) 0 (1- (board/height board))))
      (setf *src-square-x* 
            (clamp (+ *src-square-x* dx) 0 (1- (board/width board)))
            *src-square-y*
            (clamp (+ *src-square-y* dy) 0 (1- (board/height board))))))
(defun window/init-move (board)
  (when (eql (board/square board *src-square-x* *src-square-y*) :white-check)
    (setf *tgt-square-x* *src-square-x*
          *tgt-square-y* *src-square-y*)))
(defun window/apply-move-if-possible (board)
  (doplist (key move *moves*)
    (let ((src (cons *src-square-x* *src-square-y*))
          (tgt (cons *tgt-square-x* *tgt-square-y*)))
      (when (move/validp move board :white-check src tgt)
        (move/apply move board :white-check src tgt)
        (setf *src-square-x* *tgt-square-x*
              *src-square-y* *tgt-square-y*)
        (ai/process *board*))))
  (window/finalize-move))
(defun window/init-or-apply-move (board)
  (if (window/tgt-square-defined-p)
      (window/apply-move-if-possible board)
      (window/init-move board)))
(defun window/cancel-move ()
  (window/finalize-move))
(defun window/finalize-move ()
  (setf *tgt-square-x* nil
        *tgt-square-y* nil))
(defun window/handle-input (board key)
  (case key
    (:sdl-key-left (window/move-selection board -1 0))
    (:sdl-key-up (window/move-selection board 0 1))
    (:sdl-key-right (window/move-selection board 1 0))
    (:sdl-key-down (window/move-selection board 0 -1))
    (:sdl-key-space (window/init-or-apply-move board))
    (:sdl-key-escape (window/cancel-move))
    (:sdl-key-q (sdl:push-quit-event))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun main ()
  (sdl:with-init ()
    (sdl:window *window-width* *window-height* :bpp 32)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key) 
        (restart-case
            (window/handle-input *board* key)
          (continue () :report "Continue interaction")))
      (:idle ()
        (restart-case
            (sdl:with-surface 
                (back-buffer (sdl:create-surface *window-width* 
                                                 *window-height*
                                                 :pixel-alpha 120))
              (handle-slime-requests)
              (sdl:clear-display sdl:*black*)
              (window/draw-game *board*)
              (sdl:blit-surface back-buffer sdl:*default-display*)
              (sdl:update-display))
          (continue () :report "Continue interaction"))))))
