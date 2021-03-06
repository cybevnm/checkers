;;;; Checkers game with AI
;;;; Copyright (C) 2014 cybevnm

(defpackage checkers
  (:use :cl
        :anaphora
        :alexandria
        :optima
        :arrow-macros
        :lisp-unit2)
  (:shadowing-import-from :alexandria :set-equal))
(in-package checkers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-slime-requests ()
  #+swank
  (let ((connection (or swank::*emacs-connection* 
                        (swank::default-connection))))
    ;;(when (and connection (not (eql swank:*communication-style* :spawn)))
    ;;  (swank::handle-requests connection t))))
    (when connection      
      (swank::handle-requests connection t))))
(defun stride (seq step &key (start-from 0))
  (loop for i from start-from below (length seq) by step
     collecting (nth i seq)))
(defun square-func (x period)
  "Returns -1, 0 or 1 depending on x"
  (signum (- (mod (- x (/ period 2)) period) (/ period 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *white-checks* '(:white-check :white-king))
(defparameter *black-checks* '(:black-check :black-king))
(defun check/checks-for-color (color)
  (ecase color
    (:white *white-checks*)
    (:black *black-checks*)))
(defun check/color (check)
  (case check
    ((:white-check :white-king) :white)
    ((:black-check :black-king) :black)
    (otherwise nil)))
(defun opposite-color (color)
  (ecase color
    (:white :black)
    (:black :white)
    (nil nil)))
(defun check/opposite-color (check)
  (opposite-color (check/color check)))
(defun check/king-for-color (color)
  (ecase color
    (:white :white-king)
    (:black :black-king)))
(defun check/king-for-check (check)
  (ecase check
    (:white-check :white-king)
    (:black-check :black-king)))
(defun check/kingp (check)
  (case check
    ((:white-king :black-king) t)
    (otherwise nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass board ()
  ((squares :initarg :squares :reader board/squares)))
(defun board/equalp (a b)
  (labels ((squares-equal (a b)
             (dotimes (y (board/height a))
               (dotimes (x (board/width a))
                 (unless (eq (board/square a x y)
                             (board/square b x y))
                   (return-from squares-equal nil))))
             t))
    (and (= (board/width a) (board/width b))
         (= (board/height a) (board/height b))
         (squares-equal a b))))
(defun board/square (board x y)
  (aref (board/squares board) x y))
(defun board/check (board x y)
  (alet (board/square board x y)
    (assert (not (eq it :empty)))
    it))
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
     (symbol-macrolet ((,square (board/square ,board ,x ,y)))
       (dotimes (,x (board/width ,board))
         ,@body))))
(defun board/make-empty (&optional (w 10) (h 10))
  (make-instance 'board 
                 :squares (make-array `(,w ,h) :initial-element :empty)))
(defun board/square-to-char (s)
  (ecase s
    (:white-check #\w)
    (:white-king  #\W)
    (:empty       #\.)
    (:black-check #\b)
    (:black-king  #\B)))
(defun board/char-to-square (c)
  (ecase c
    (#\w     :white-check)
    (#\W     :white-king)
    (#\Space :empty)
    (#\.     :empty)
    (#\b     :black-check)
    (#\B     :black-king)))
(defun board/to-string (board &key (insert-spaces-p t))
  (with-output-to-string (s)
    (dotimes (inv-y (board/height board))
      (dotimes (x (board/width board))
        (let ((y (- (board/height board) inv-y 1)))
          (write-char (board/square-to-char (board/square board x y)) s)
          (when insert-spaces-p
            (write-char #\Space s))))
      (terpri s))))
(defun board/print (board &key (insert-spaces-p t))
  (write-string (board/to-string board :insert-spaces-p insert-spaces-p))
  (values))
(defun board/make (src)
  (assert (> (length src) 0))
  (let* ((w (length (first src)))
         (h (length src))
         (board (board/make-empty w h)))
    (dolist (row src)
      (assert (= (length row) w)))
    (loop for y from 0 to (1- h)
       do (loop for x from 0 to (1- w)
             do (setf (board/square board x (- h y 1))
                      (board/char-to-square (char (nth y src) x)))))
    board))
(defun board/make-1 ()
  (board/make '(" b b b b b"
                "b b b b b "
                " b b b b b"
                "b b b b b "
                "          "
                "          "
                " w w w w w"
                "w w w w w "
                " w w w w w"
                "w w w w w ")))
(defun board/make-2 ()
  (board/make '(" b b b"
                "      "
                "      "
                "      "
                "      "
                "w w w ")))
(defun board/clone (board)
  (alet (board/make-empty (board/width board) (board/height board))
    (doboard (x y square it)
      (setf square (board/square board x y)))
    it))
(defun board/move-check (board src-x src-y tgt-x tgt-y)
  (symbol-macrolet ((src-square (board/square board src-x src-y))
                    (tgt-square (board/square board tgt-x tgt-y)))
    (assert (not (eq src-square :empty)))
    (assert (eq tgt-square :empty))
    (setf tgt-square src-square)
    (setf src-square :empty)))
(defun board/check-direction (square)
  (ecase square
    ((:white-check :white-king) :up)
    ((:black-check :black-king) :down)))
(defun board/kings-row (board color)
  (ecase color
    (:white 0)
    (:black (1- (board/height board)))))
(defun board/kings-row-p (board color y)
  "Return t if y is kings row"
  (= y (board/kings-row board color)))
(defun board/enum-actions (board color &key src tgt recursive-action)
  (let ((result))
    (labels ((moves-actions (src tgt)
               (doplist (_ move *moves*)
                 (when (move/validp move board color src tgt)
                   (push (make-instance 'action :move move :src src :tgt tgt)
                         result))))
             (actions ()
               (doboard (src-x src-y _ board)
                 (doboard (tgt-x tgt-y _ board)
                   (moves-actions (cons src-x src-y) 
                                  (cons tgt-x tgt-y))))))
      (actions)
      (when (member-if #'move/mandatoryp result :key #'action/move)
        (setf result (remove-if-not #'move/mandatoryp result :key #'action/move)))
      (when src
        (setf result 
              (remove-if-not (curry #'equal src) result :key #'action/src)))
      (when tgt
        (setf result 
              (remove-if-not (curry #'equal tgt) result :key #'action/tgt)))
      (when recursive-action
        (setf result
              (remove-if-not (lambda (a)
                               (and (eq (action/move recursive-action) (action/move a))
                                    (if (action/src recursive-action)
                                        (equal (action/src recursive-action) (action/src a))
                                        t)
                                    (if (action/tgt recursive-action)
                                        (equal (action/tgt recursive-action) (action/tgt a))
                                        t)))
                             result))))
    result))
(define-test ai/board-actions-test ()
  (labels ((check-action (as move-name src-x src-y tgt-x tgt-y)
             (when (find (make-instance 'action
                                        :move (getf *moves* move-name)
                                        :src (cons src-x src-y)
                                        :tgt (cons tgt-x tgt-y))
                         as
                         :test #'action/equalp)
               t)))
    ;; black check moves
    (let* ((as (board/enum-actions (board/make '("....."
                                                 "..b.."
                                                 ".....")) :black)))
      (assert-eql (length as) 2)
      (assert-true (check-action as 'check-move 2 1 1 0))
      (assert-true (check-action as 'check-move 2 1 3 0)))
    ;; white check moves
    (let* ((as (board/enum-actions (board/make '("....."
                                                 "..w.."
                                                 ".....")) :white)))
      (assert-eql (length as) 2)
      (assert-true (check-action as 'check-move 2 1 1 2))
      (assert-true (check-action as 'check-move 2 1 3 2)))
    ;; TODO: other tests should be added
    ))
(defun board/action-valid-p (move board color &key src tgt recursive-action)
  (member (make-instance 'action :move move :src src :tgt tgt)
          (board/enum-actions board
                              color
                              :src src
                              :tgt tgt
                              :recursive-action recursive-action)
          :test #'action/equalp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun square-contains-friendly-check-p (board curr-color pos)
  (alet (board/square board (car pos) (cdr pos))
    (and (eq (check/color it) curr-color)
         (not (check/kingp it)))))
(defun square-contains-friendly-king-p (board curr-color pos)
  (alet (board/square board (car pos) (cdr pos))
    (and (eq (check/color it) curr-color)
         (check/kingp it))))
(defun square-empty-p (board curr-color pos)
  (declare (ignore curr-color))
  (eql (board/square board (car pos) (cdr pos)) :empty))
(defun distance (src-pos tgt-pos)
  (cons (- (car tgt-pos) (car src-pos))
        (- (cdr tgt-pos) (cdr src-pos))))
(defun hor-distance (board curr-color src-pos tgt-pos)
  (declare (ignore board curr-color))
  (abs (car (distance src-pos tgt-pos))))
(defun ver-forward-distance (board curr-color src-pos tgt-pos)
  (declare (ignore curr-color))
  (alet (cdr (distance src-pos tgt-pos))
    (ecase (board/check-direction 
            (board/square board (car src-pos) (cdr src-pos)))
      (:up it)
      (:down (- it)))))
(defun ver-distance (board curr-color src-pos tgt-pos)
  (declare (ignore board curr-color))
  (abs (cdr (distance src-pos tgt-pos))))
(defun hor-and-ver-distances-are-the-same (board curr-color src-pos tgt-pos)
  (declare (ignore board curr-color))
  (= (abs (- (car src-pos) (car tgt-pos)))
     (abs (- (cdr src-pos) (cdr tgt-pos)))))
(defun half-distance (src-pos tgt-pos)
  (alet (distance src-pos tgt-pos)
    (cons (/ (car it) 2) (/ (cdr it) 2))))
(defun shifted-pos (src-pos vec)
  (cons (+ (car src-pos) (car vec)) 
        (+ (cdr src-pos) (cdr vec))))
(defun map-line-squares (board src-pos tgt-pos func)
  (labels ((calc-delta (a b)
             (cond
               ((> a b) -1)
               ((< a b)  1)
               (t 0))))
    ;; Itearte when distance is big enough.
    (when (or (> (abs (- (car src-pos) (car tgt-pos))) 1)
              (> (abs (- (cdr src-pos) (cdr tgt-pos))) 1))
      (let ((dx (calc-delta (car src-pos) (car tgt-pos)))
            (dy (calc-delta (cdr src-pos) (cdr tgt-pos))))
        (do ((x (+ (car src-pos) dx) (incf x dx))
             (y (+ (cdr src-pos) dy) (incf y dy)))
            ((or (= x (car tgt-pos))
                 (= y (cdr tgt-pos))))
          (funcall func x y (board/square board x y)))))))
(defun line-contains-one-enemy-check-p (board curr-color src-pos tgt-pos)
  (let ((enemy-checks-num 0))
    (map-line-squares board
                      src-pos
                      tgt-pos
                      (lambda (x y square)
                        (declare (ignore x y))
                        (when (and (eq (check/color square)
                                       (opposite-color curr-color)))
                          (incf enemy-checks-num))))
    (= enemy-checks-num 1)))
(defun line-contains-enemy-check-before-target-p (board curr-color
                                                  src-pos tgt-pos)
  (let ((dx (alet (- (car src-pos) (car tgt-pos)) (/ it (abs it))))
        (dy (alet (- (cdr src-pos) (cdr tgt-pos)) (/ it (abs it)))))
    (eq (check/color (board/square board
                                   (+ (car tgt-pos) dx)
                                   (+ (cdr tgt-pos) dy)))
        (opposite-color curr-color))))
(defun count-checks-on-line (board curr-color src-pos tgt-pos)
  "Returns cons cell where first value is number of friendly checks 
   and second value is number of enemy checks"
  (let ((friendly-checks-num 0)
        (enemy-checks-num 0))
    (map-line-squares board
                      src-pos
                      tgt-pos
                      (lambda (x y square)
                        (declare (ignore x y))
                        (when (and (eq (check/color square) curr-color))
                          (incf friendly-checks-num))
                        (when (and (eq (check/color square)
                                       (opposite-color curr-color)))
                          (incf enemy-checks-num))))
    (cons friendly-checks-num enemy-checks-num)))
(defun line-contains-no-friendly-checks-p (board curr-color src-pos tgt-pos)
  (zerop (car (count-checks-on-line board curr-color src-pos tgt-pos))))
(defun line-contains-no-enemy-checks-p (board curr-color src-pos tgt-pos)
  (zerop (cdr (count-checks-on-line board curr-color src-pos tgt-pos))))
(defun move-check (board curr-color src-pos tgt-pos)
  (alet (board/square board (car src-pos) (cdr src-pos))
    (board/move-check board 
                      (car src-pos) (cdr src-pos) 
                      (car tgt-pos) (cdr tgt-pos))
    (when (and (not (check/kingp it))
               (board/kings-row-p board
                                  (opposite-color curr-color)
                                  (cdr tgt-pos)))
      (setf (board/square board (car tgt-pos) (cdr tgt-pos))
            (check/king-for-color curr-color)))))
(defun kill-enemy-checks-on-line (board curr-color src-pos tgt-pos)
  (map-line-squares board
                    src-pos
                    tgt-pos
                    (lambda (x y square)
                      (when (and (eq (check/color square)
                                     (opposite-color curr-color)))
                        (board/kill-check board x y)))))
(defclass move ()
  ((recursive :initarg :recursive :reader move/recursivep)
   (mandatory :initarg :mandatory :reader move/mandatoryp)
   (predicate :initarg :predicate :reader move/predicate)
   (modifier :initarg :modifier :reader move/modifier)))
(defparameter *moves* nil)
(defun move/validp (move board curr-color src-pos tgt-pos)
  (funcall (move/predicate move) board curr-color src-pos tgt-pos))
(defun move/apply (move board curr-color src-pos tgt-pos)
  "Returns t if move is recursive and should be continued"
  (flet ((valid-for-any-pos-p (src-pos2)
           (doboard (x y _ board)
             (when (move/validp move board curr-color src-pos2 (cons x y))
               (return-from valid-for-any-pos-p t)))))
    (funcall (move/modifier move) board curr-color src-pos tgt-pos)
    (and (move/recursivep move) (valid-for-any-pos-p tgt-pos))))
(defun move/remove (name)
  (remf *moves* name))
(eval-when (:compile-toplevel :load-toplevel)
  (defun move/parse-pred-func-def (def)
    (values (first def) (cdr (butlast def)) (lastcar def)))
  (defun move/gen-pred-func-call (def)
    (multiple-value-bind (name args-rest result) (move/parse-pred-func-def def)
      `(eql ,(append (list name) 
                     '(board curr-color) 
                     args-rest)
            ,result)))
  (defun move/parse-mod-func-def (def)
    (values (first def) (cdr def)))
  (defun move/gen-mod-func-call (def)
    (multiple-value-bind (name args-rest) (move/parse-mod-func-def def)
      (append (list name) '(board curr-color) args-rest))))
(defmacro defmove (name (&rest attributes) (&rest predicates) (&rest modifiers))
  `(setf (getf *moves* ',name)  
         (make-instance 
          'move
          :recursive (member 'recursive ',attributes)
          :mandatory (member 'mandatory ',attributes)
          :predicate (lambda (board curr-color source target)
                       (and ,@(loop for p in predicates
                                 collecting (move/gen-pred-func-call p))))
          :modifier (lambda (board curr-color source target)
                      ,@(loop for m in modifiers
                           collecting (move/gen-mod-func-call m))))))
(defmove check-move ()
  ((square-contains-friendly-check-p source t)
   (square-empty-p target t)
   (hor-distance source target 1)
   (ver-forward-distance source target 1))
  ((move-check source target)))
(defmove king-move ()
  ((square-contains-friendly-king-p source t)
   (square-empty-p target t)
   (hor-and-ver-distances-are-the-same source target t)
   (line-contains-no-friendly-checks-p source target t)
   (line-contains-no-enemy-checks-p source target t))
  ((move-check source target)))
(defmove check-attack (recursive mandatory)
  ((square-contains-friendly-check-p source t)
   (square-empty-p target t)
   (hor-distance source target 2)
   (ver-distance source target 2)
   (line-contains-one-enemy-check-p source target t))
  ((kill-enemy-checks-on-line source target)
   (move-check source target)))
(defmove king-attack (recursive mandatory)
  ((square-contains-friendly-king-p source t)
   (square-empty-p target t)
   (hor-and-ver-distances-are-the-same source target t)
   (line-contains-one-enemy-check-p source target t)
   (line-contains-enemy-check-before-target-p source target t)
   (line-contains-no-friendly-checks-p source target t))
  ((kill-enemy-checks-on-line source target)
   (move-check source target)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass action ()
  ((move :initarg :move :reader action/move)
   (src :initarg :src :initform nil :reader action/src)
   (tgt :initarg :tgt :initform nil :reader action/tgt))
  (:documentation "Binds move to source and target positions.
                   Note that both source and target are optional
                   and can be nil"))
(defun action/equalp (a b)
  (and (eql (action/move a) (action/move b))
       (equal (action/src a) (action/src b))
       (equal (action/tgt a) (action/tgt b))))
(defun action/fullp (a)
  "Returns t if action is fully defined, i.e. has src and tgt
   defined."
  (and (action/src a) (action/tgt a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ai ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass node ()
  ((parent :initarg :parent :initform nil)
   (board :initarg :board :reader node/board)
   (color :initarg :color :reader node/color)
   (children :initarg :children :initform nil :accessor node/children)
   (depth :initarg :depth :initform 0 :reader node/depth)
   (action :initarg :action :reader node/action)
   (recursive :initarg :recursive :initform nil :reader node/recursivep)
   (alpha :initarg :alpha :accessor node/alpha)
   (beta :initarg :beta :accessor node/beta)))
(defun node/src (n)
  (action/src (node/action n)))
(defun node/tgt (n)
  (action/tgt (node/action n)))
(defun node/move (n)
  (action/move (node/action n)))
(defun ai/rate-board (board maximizing-color)
  (let ((rate 0))
    (doboard (x y square board)
      (let ((k (if (eql (check/color square) maximizing-color) 1 -1))
            (cost (ecase square
                    (:empty 0)
                    ((:black-check :white-check) 1)
                    ((:black-king :white-king) 5))))
        (incf rate (* k cost))))
    rate))
(define-test ai/rate-board-test ()
  (let ((board (board/make '("   b"
                             "w b "
                             " B  "))))
    (assert-eql (ai/rate-board board :white) -6)
    (assert-eql (ai/rate-board board :black) 6)))
(defun ai/build-tree (board)
  (let ((result (make-instance 'node 
                           :color :white
                           :board board)))
    (ai/negamax board :black :parent-node result)
    result))
(defun ai/minimax-impl (board depth color &key recursive-action parent-node)
  (when parent-node
    (assert (board/equalp board (node/board parent-node))))
  (labels ((branch (actions initial-extremum-rate rate-comparator)
             (let ((extremum-rate initial-extremum-rate)
                   (extremum-action nil))
               (dolist (curr-action actions
                        (values extremum-rate extremum-action))
                 (let* ((modified-board (board/clone board))
                        (src (action/src curr-action))
                        (tgt (action/tgt curr-action))
                        (recursivep (move/apply (action/move curr-action)
                                                modified-board
                                                color
                                                src
                                                tgt))
                        (curr-recursive-action
                         (and recursivep
                              (make-instance 'action
                                             :move (action/move curr-action)
                                             :src tgt)))
                        (curr-node (and parent-node
                                        (make-instance 'node
                                                       :parent parent-node
                                                       :board modified-board
                                                       :color color
                                                       :depth depth
                                                       :action curr-action
                                                       :recursive recursivep)))
                        ;; rate propagated from the leaf
                        (curr-rate
                         (ai/minimax-impl modified-board
                                          (1+ depth)
                                          (if recursivep
                                              color
                                              (opposite-color color))
                                          :recursive-action curr-recursive-action
                                          :parent-node curr-node)))
                   (when curr-node
                     (push curr-node (node/children parent-node)))
                   (when (funcall rate-comparator curr-rate extremum-rate)
                     (setf extremum-rate curr-rate
                           extremum-action curr-action))

                   )))))
    (let* ((actions (board/enum-actions board
                                        color
                                        :recursive-action recursive-action)))
      (if (and (<= depth *max-depth*) actions)
          (ecase color
            (:white (branch actions most-negative-fixnum #'>))
            (:black (branch actions most-positive-fixnum #'<)))
          (ai/rate-board board :white)))))
(defun ai/minimax (board color &key recursive-action parent-node)
  (ai/minimax-impl board 0 color
                   :recursive-action recursive-action
                   :parent-node parent-node))
(defun ai/negamax-impl (board depth color alpha beta
                        &key recursive-action parent-node)
  (when parent-node
    (assert (board/equalp board (node/board parent-node))))
  (let ((actions (board/enum-actions board
                                     color
                                     :recursive-action recursive-action))
        (max-rate most-negative-fixnum)
        (best-a))
    (if (and (< depth *max-depth*) actions)
        ;; we have actions and we descending further
        (dolist (a actions (values max-rate best-a))
          (let* ((modified-board (board/clone board))
                 (src (action/src a))
                 (tgt (action/tgt a))
                 (recursivep (move/apply (action/move a)
                                         modified-board
                                         color
                                         src
                                         tgt))
                 (curr-recursive-a (and recursivep
                                        (make-instance 'action
                                                       :move (action/move a)
                                                       :src tgt)))
                 (curr-node (and parent-node
                                 (make-instance 'node
                                                :parent parent-node
                                                :board modified-board
                                                :color color
                                                :depth depth
                                                :action a
                                                :recursive recursivep
                                                :alpha alpha
                                                :beta beta)))
                 ;; rate propagated from the leaf
                 (curr-rate (* (if recursivep 1 -1)
                               (ai/negamax-impl modified-board
                                                (1+ depth)
                                                (if recursivep
                                                    color
                                                    (opposite-color color))
                                                (if recursivep alpha (- beta))
                                                (if recursivep beta (- alpha))
                                                :recursive-action curr-recursive-a
                                                :parent-node curr-node))))
            (when (> curr-rate max-rate)
              (setf best-a a)
              (setf max-rate (max max-rate curr-rate)))
            (setf alpha (max alpha curr-rate)
                  ;; (node/alpha curr-node) alpha
                  ;; (node/beta curr-node) beta
                  )
            (when curr-node
              (push curr-node (node/children parent-node)))
            (when (>= alpha beta)
              (return (values max-rate best-a)))))
        ;; we at leaf node and returning board rate
        (* (ecase color (:white 1) (:black -1))
           (ai/rate-board board :white)))))
(defun ai/negamax (board color &key recursive-action parent-node)
  (ai/negamax-impl board
                   0
                   color
                   most-negative-fixnum
                   most-positive-fixnum
                   :recursive-action recursive-action
                   :parent-node parent-node))
(defparameter *max-depth* 4)
(define-test ai/build-tree-test ()
  (let* ((b (board/make '("....."
                          "...w."
                          "....."
                          ".w..."
                          "b....")))
         (tree (ai/build-tree b))
         (a tree)
         (b (first (node/children a)))
         (c (first (node/children b))))
    ;; tree is 1 node width all way long
    (assert-eql (length (node/children a)) 1)
    (assert-eql (length (node/children b)) 1)
    ;; c is the deepest node
    (assert-eql (length (node/children c)) 0)
    (assert-eql (node/color a) :white)
    ;; we have two consecutive black nodes for recursive
    ;; move
    (assert-eql (node/color b) :black)
    (assert-eql (node/color c) :black)))
(defun ai/process (board &key (processor #'ai/negamax))
  "Returns :action-applied if valid action was found and was 
  applied to board, :no-valid-action if no valid action was 
  found"
  (let ((curr-recursive-action))
    (loop
       :do (multiple-value-bind (rate action)
               (funcall processor
                        board
                        :black
                        :recursive-action curr-recursive-action)
             (declare (ignore rate))
             (unless action
               (return-from ai/process :no-valid-action))
             (assert (action/fullp action))
             (setf curr-recursive-action
                   (and (move/apply (action/move action)
                                    board
                                    :black
                                    (action/src action)
                                    (action/tgt action))
                        (make-instance 'action
                                       :move (action/move action)
                                       :src (action/tgt action)))))
       :while curr-recursive-action))
  :action-applied)
(defun test/make-boards (&rest chunks)
  (assert (evenp (length chunks)))
  (cons (board/make (stride chunks 2 :start-from 0))
        (board/make (stride chunks 2 :start-from 1))))
(defun test/check-ai-reactions (&rest chunks)
  (loop for processor in (list #'ai/minimax #'ai/negamax)
     for boards = (apply #'test/make-boards chunks)
     do (ai/process (car boards))
     collecting  (board/equalp (car boards) (cdr boards))))
(define-test ai/process-test ()
  ;; black king moves to attack position
  (assert-true (test/check-ai-reactions "..B.."  "....."
                                        "....."  "...B."
                                        "b...."  "b...."
                                        ".w..."  ".w..."
                                        "..b.."  "..b.."))
  ;; black check selects recursive attack to left
  (assert-true (test/check-ai-reactions "....b...." "........."
                                        "...w.w..." ".....w..."
                                        "........." "........."
                                        ".w......." "........."
                                        "........." "b........"
                                        "........." "........."))
    ;; black check selects recursive attack to right
  (assert-true (test/check-ai-reactions "....b...." "........."
                                        "...w.w..." "...w....."
                                        "........." "........."
                                        ".......w." "........."
                                        "........." "........b"
                                        "........." ".........")))
(defmethod cl-dot:graph-object-node ((graph (eql 'example)) (obj node))
  (labels ((format-label ()
             (with-output-to-string (s)
               (write-string (board/to-string (node/board obj)) s)
               (terpri s)
               (format s "(~A)" (ai/rate-board (node/board obj) :white))
               (terpri s)
               (when (slot-boundp obj 'alpha)
                 (format s "A=~A" (node/alpha obj))
                 (terpri s))
               (when (slot-boundp obj 'beta)
                 (format s "B=~A" (node/beta obj))))))
    (make-instance 'cl-dot:node
                   :attributes `(:label ,(format-label)
                                        :style :solid
                                        :color ,(ecase (node/color obj)
                                                       (:black :black)
                                                       (:white :gray))
                                        :shape :box
                                        :fontname "courier"))))
(defmethod cl-dot:graph-object-points-to ((graph (eql 'example)) (obj node))
  (assert obj)
  (node/children obj)
  ;; (list (car object)
  ;;       (make-instance 'cl-dot:attributed
  ;;                      :object (cdr object)
  ;;                      :attributes '(:weight 3)))
  )
(defun ai/render-tree (board filename)
  (cl-dot:dot-graph (cl-dot:generate-graph-from-roots 'example
                                                      (list (ai/build-tree board)))
                    filename
                    :format :png))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assets loading and related stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun executable-file-path ()
  #+linux (osicat:read-link #p"/proc/self/exe")
  #-linux (error (concat "executable-file-path is "
                         "not supported for this platform yet")))
(defparameter *sources-dir-path*
  (make-pathname :name nil :type nil
                 :defaults #.(or *compile-file-truename*
                                 *load-truename*)))
(defparameter *assets-dir-type* :development-dir
  "Specifies where to search for assets, can take two values:
   :development-dir - is directory where .lisp sources files are,
   :executable-dir - is directory where dumped image is.")
(defun assets-dir-path ()
  (ecase *assets-dir-type*
    (:development-dir *sources-dir-path*)
    (:executable-dir (make-pathname :name nil :type nil
                                    :defaults (executable-file-path)))))
(defun string-width (str font)
  (* (length str) (sdl:char-width font)))
(defun make-font (file-name char-width char-height)
  (let ((chars-num 48))
    (sdl:initialise-font
     (make-instance 'sdl:simple-font-definition
                    :width char-width :height char-height
                    :character-map "ABCDEFGHIJKLMNOPQRSTUVWXYZ:'!?_-,.()#~0123456789"
                    :character-mask (loop for y from 0 below 1
                                       append (loop for x from 0 below chars-num
                                                 collect (list (* x char-width)
                                                               (* y char-height)
                                                               char-width
                                                               char-height)))
                    :color-key (sdl:color :r 99 :g 0 :b 0)
                    :filename (sdl:create-path file-name
                                               (assets-dir-path))))))
(defvar *biggest-font* nil)
(defvar *big-font* nil)
(defvar *medium-font* nil)
(defun initialise-fonts ()
  (setf *biggest-font* (make-font "biggest-font.bmp" 32 40)
        *big-font*     (make-font "big-font.bmp" 24 30)
        *medium-font*  (make-font "medium-font.bmp" 16 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *window-width* 450)
(defparameter *window-height* 450)
(defparameter *square-color* (list :white (sdl:color :r #xE8 :g #xD0 :b #xAA)
                                   :black (sdl:color :r #xA6 :g #x7D :b #x5D)))
(defparameter *check-color* (list :white-check (sdl:color :r #xF6 :g #xC6 :b #x48)
                                  :white-king (sdl:color :r #xF6 :g #xC6 :b #x48)
                                  :black-check (sdl:color :r #x7A :g #x50 :b #x44)
                                  :black-king (sdl:color :r #x7A :g #x50 :b #x44)))
(defparameter *check-pin-color* (list :white-check (sdl:color :r #xFF :g #xD6 :b #x58)
                                      :white-king (sdl:color :r #xD0 :g #x00 :b #x00)
                                      :black-check (sdl:color :r #x8A :g #x60 :b #x54)
                                      :black-king (sdl:color :r #xD0 :g #x00 :b #x00)))
(defparameter *state* :board
  "State can be one of the next values:
   :intro - intro screen
   :board - board screen
   :win - human player wins
   :defeat - human player loses
   :draw - no one wins")
(defgeneric game/handle-input (state key))
(defgeneric game/draw (state frame-index))
(defmethod game/handle-input ((state (eql :intro)) key)
  (case key
    (:sdl-key-space (setf *state* :board))
    (:sdl-key-q (sdl:push-quit-event))))
(defparameter *orange* (sdl:color :r 255 :g 127 :b 0))
(defparameter *purple* (sdl:color :r 128 :g 0   :b 128))
(defmethod game/draw ((state (eql :intro)) frame-index)
  (labels ((calc-string-left-border (str font)
             (/ (- *window-height* (string-width str font)) 2)))
    ;; title
    (let* ((title "CHECKERS")
           (title-left (calc-string-left-border title *biggest-font*)))
      (loop          for char across title
         for index from 0
         do (sdl:draw-string-solid-*
             (string char)
             (+ (* index (sdl:char-width *biggest-font*))
                title-left)
             (if (evenp index) 30 45)
             :font *biggest-font*
             :color (getf *square-color* :black))))
    ;; press space
    (when (> (square-func frame-index 30) 0)
      (let* ((text "PRESS SPACE")
             (left (calc-string-left-border text *medium-font*)))
        (sdl:draw-string-solid-* text
                                 left
                                 (/ *window-height* 2)
                                 :font *medium-font*
                                 :color (getf *square-color* :black))))
    ;; author
    (let* ((text "BY CYBEVNM")
           (left (calc-string-left-border text *medium-font*)))
      (sdl:draw-string-solid-* text 
                               left
                               (- *window-height* 40)
                               :font *medium-font*
                               :color *purple*))))
(defparameter *src-square-x* 3)
(defparameter *src-square-y* 3)
(defparameter *tgt-square-x* nil)
(defparameter *tgt-square-y* nil)
(defparameter *player-recursive-action* nil)
(defparameter *board* (board/make-1))
(defun game/tgt-square-defined-p ()
  (and *tgt-square-x* *tgt-square-y*))
(defun game/move-selection (board dx dy)
  (if (and *tgt-square-x* *tgt-square-y*)
      (setf *tgt-square-x* 
            (clamp (+ *tgt-square-x* dx) 0 (1- (board/width board)))
            *tgt-square-y*
            (clamp (+ *tgt-square-y* dy) 0 (1- (board/height board))))
      (setf *src-square-x* 
            (clamp (+ *src-square-x* dx) 0 (1- (board/width board)))
            *src-square-y*
            (clamp (+ *src-square-y* dy) 0 (1- (board/height board))))))
(defun game/init-move (board)
  (when (eq (check/color (board/square board *src-square-x* *src-square-y*))
            :white)
    (setf *tgt-square-x* *src-square-x*
          *tgt-square-y* *src-square-y*)))
(defun game/find-first-valid-move (board src tgt)
  (find-if (lambda (move)
             (and (eq (type-of move) 'move)
                  (board/action-valid-p move
                                        board
                                        :white
                                        :src src
                                        :tgt tgt
                                        :recursive-action *player-recursive-action*)))
           *moves*))
(defmacro with-move-to-be-finalized (&body body)
  `(unwind-protect (progn ,@body)
     (game/finalize-move)))
(defun game/count-checks (board color)
  (let ((result 0))
    (doboard (x y square board)
      (when (and (not (eql square :empty))
                 (eql (check/color square) color))
        (incf result)))
    result))
(define-test game/count-checks-test ()
  (let ((board (board/make '("..w."
                             "..b."
                             "B..."
                             "WWW."))))
    (assert-eq (game/count-checks board :black) 2)
    (assert-eq (game/count-checks board :white) 4)))
(defun game/apply-move-if-possible (board)
  "Returns
   :win if move lead to human player win
   :defeat if ai response lead to human player lose
   :continue if game should be continued"
  (assert (> (game/count-checks board :white) 0))
  (with-move-to-be-finalized
    (let* ((src (cons *src-square-x* *src-square-y*))
           (tgt (cons *tgt-square-x* *tgt-square-y*))
           (move (game/find-first-valid-move board src tgt)))
      (unless move
        ;; no valid move for specified src and target,
        ;; let the player to select another move
        (return-from game/apply-move-if-possible :continue))
      (setf *src-square-x* *tgt-square-x*
            *src-square-y* *tgt-square-y*)
      (let ((recursive-move (move/apply move *board* :white src tgt)))
        (setf *player-recursive-action* (and recursive-move
                                             (make-instance 'action
                                                            :move move
                                                            :src tgt)))
        (cond
          ;; all black checks beaten - human player wins
          ((= (game/count-checks board :black) 0) :win)
          ;; human player is in middle of recursive move,
          ;; so continue
          (recursive-move :continue)
          ;; let the ai player do the move
          (t (ecase (ai/process *board*)
               (:action-applied (if (= (game/count-checks board :white) 0)
                                    :defeat
                                    :continue))
               (:no-valid-action :win))))))))
(defun game/init-or-apply-move (board)
  "Returns 
   :win if move lead to human player win
   :defeat if ai response lead to human player lose
   :continue if game should be continued"
  (if (game/tgt-square-defined-p)
      (game/apply-move-if-possible board)
      (progn
        (game/init-move board)
        :continue)))
(defun game/cancel-move ()
  (game/finalize-move))
(defun game/finalize-move ()
  (setf *tgt-square-x* nil
        *tgt-square-y* nil))
(defmethod game/handle-input ((state (eql :board)) key)
  (case key
    (:sdl-key-left (game/move-selection *board* -1 0))
    (:sdl-key-up (game/move-selection *board* 0 1))
    (:sdl-key-right (game/move-selection *board* 1 0))
    (:sdl-key-down (game/move-selection *board* 0 -1))
    (:sdl-key-space (case (game/init-or-apply-move *board*)
                      (:win (setf *state* :win))
                      (:defeat (setf *state* :defeat))))
    (:sdl-key-escape (game/cancel-move))
    (:sdl-key-q (sdl:push-quit-event))))
(defun game/calc-inv-y (board y)
  (- (board/height board) y 1))
(defun game/draw-selected-square (board labels-w labels-h
                                  x y square-w square-h color)
  (sdl:draw-filled-circle-* (+ labels-w (* x square-w) (round (/ square-w 2))) 
                            (+ (* (game/calc-inv-y board y) square-h) 
                               (round (/ square-h 2))
                               labels-h)
                            (round (/ (/ (+ square-w square-h) 2) 5))
                            :color color))
(defun game/draw-src-square (board labels-w labels-h square-w square-h)
  (declare (ignore labels-h))
  (game/draw-selected-square board
                             labels-w labels-h
                             *src-square-x* *src-square-y* 
                             square-w square-h 
                             sdl:*blue*))
(defun game/draw-tgt-square (board labels-w labels-h square-w square-h)
  (declare (ignore labels-h))
  (when (and *tgt-square-x* *tgt-square-y*)
    (game/draw-selected-square board
                               labels-w labels-h
                               *tgt-square-x* *tgt-square-y*
                               square-w square-h
                               sdl:*red*)))
(defun game/draw-move-variants (board labels-w labels-h square-w square-h)
  (let ((src-square (board/square board *src-square-x* *src-square-y*)))
    (when (and (not (eq src-square :empty))
               (eq (check/color src-square) :white))
      (dolist (a (board/enum-actions board 
                                     :white
                                     :src (cons *src-square-x* *src-square-y*)
                                     :recursive-action *player-recursive-action*))
        (sdl:draw-box-* (+ (* (car (action/tgt a)) square-w) labels-w)
                        (+ labels-h
                           (* (game/calc-inv-y board (cdr (action/tgt a)))
                              square-h))
                        square-w 
                        square-h
                        :color (sdl:color :r 60 :g 60 :b 60)
                        :alpha 125)))))
(defun game/draw-ver-labels (board x0 labels-w labels-h square-w square-h
                             &key line)
  (declare (ignore square-w))
  (dotimes (y (board/height board))
    (let ((y0 (+ (* y square-h) labels-h))
          (text (write-to-string (- (board/width board) y))))
      (sdl:draw-string-solid-* text
                               (round (+ x0
                                         (/ (- labels-w
                                               (string-width text *medium-font*))
                                            2)))
                               (round (+ y0
                                         (/ (- square-h
                                               (sdl:char-height *medium-font*))
                                            2)))
                               :font *medium-font*
                               :color sdl:*black*)
      (ecase line
        (:left (sdl:draw-line-* x0 y0
                                x0 (+ y0 square-h)
                                :color sdl:*black*))
        (:right (sdl:draw-line-* (+ x0 labels-w -1) y0
                                 (+ x0 labels-w -1) (+ y0 square-h)
                                 :color sdl:*black*))))))
(defun game/draw-hor-labels (board y0 labels-w labels-h square-w square-h
                             &key line)
  (declare (ignore square-h))
  (dotimes (x (board/width board))
    (let ((x0 (+ (* x square-w) labels-w)))
      (sdl:draw-string-solid-* (string (code-char (+ (char-code #\A) x)))
                               (round (+ x0
                                         (/ (- square-w
                                               (sdl:char-width *medium-font*))
                                            2)))
                               (round (+ y0
                                         (/ (- labels-h
                                               (sdl:char-height *medium-font*))
                                             2)))
                               :font *medium-font*
                               :color sdl:*black*)
      (ecase line
        (:top (sdl:draw-line-* x0 y0
                               (+ x0 square-w) y0
                               :color sdl:*black*))
        (:bottom (sdl:draw-line-* x0 (+ y0 labels-h -1)
                                  (+ x0 square-w) (+ y0 labels-h -1)
                                  :color sdl:*black*))))))
(defun game/draw-board (board labels-w labels-h square-w square-h)
  (game/draw-ver-labels board 0
                        labels-w labels-h square-w square-h
                        :line :right)
  (game/draw-ver-labels board (- *window-width* labels-w)
                        labels-w labels-h square-w square-h
                        :line :left)
  (game/draw-hor-labels board 0
                        labels-w labels-h square-w square-h
                        :line :bottom)
  (game/draw-hor-labels board (- *window-height* labels-h)
                        labels-w labels-h square-w square-h
                        :line :top)
  ;; squares
  (doboard (x y square board)
    (let* ((inv-y (game/calc-inv-y board y))
           (window-x (+ (* x square-w) labels-w))
           (window-y (+ labels-h (* inv-y square-h))))
      (sdl:draw-box-* window-x  window-y square-w square-h
                      :color (getf *square-color* (board/color x y))))))
(defun game/draw-check (labels-w labels-h x y w h square)
  (sdl:draw-filled-circle-* (+ labels-w (* x w) (round (/ w 2)))
                            (+ labels-h (* y h) (round (/ h 2)))
                            (round (/ (/ (+ w h) 2) 3))
                            :color (getf *check-color* square))
  (sdl:draw-filled-circle-* (+ labels-w (* x w) (round (/ w 2)))
                            (+ labels-h (* y h) (round (/ h 2)))
                            (round (/ (/ (+ w h) 2) 6))
                            :color (getf *check-pin-color* square)))
(defun game/draw-checks (board labels-w labels-h square-w square-h)
  (doboard (x y square board)
    (alet (game/calc-inv-y board y)
      (when (not (eql square :empty))
        (game/draw-check labels-w labels-h x it square-w square-h square)))))
(defmethod game/draw ((state (eql :board)) frame-index)
  (let* ((labels-w (round (* 2.2 (sdl:char-width *medium-font*))))
         (labels-h (round (* 2.2 (sdl:char-width *medium-font*))))
         (square-w (round (/ (- *window-width* (* 2 labels-w))
                             (board/width *board*))))
         (square-h (round (/ (- *window-height* (* 2  labels-h))
                             (board/height *board*)))))
    (game/draw-board *board* labels-w labels-h square-w square-h)
    (game/draw-move-variants *board* labels-w labels-h square-w square-h)
    (game/draw-checks *board* labels-w labels-h square-w square-h)
    (game/draw-src-square *board* labels-w labels-h square-w square-h)
    (game/draw-tgt-square *board* labels-w labels-h square-w square-h)))
(defmethod game/handle-input ((state (eql :win)) key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))))
(defun game/draw-ending-string (string)
  (labels ((calc-string-left-border (str font)
             (/ (- *window-height* (string-width str font)) 2)))
    ;; title
    (let* ((title string)
           (title-left (calc-string-left-border title *biggest-font*)))
      (sdl:draw-string-solid-* title title-left
                               (/ *window-height* 2)
                               :font *biggest-font*
                               :color (getf *square-color* :black)))))
(defmethod game/draw ((state (eql :win)) frame-index)
  (game/draw-ending-string "VICTORY!"))
(defmethod game/handle-input ((state (eql :defeat)) key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))))
(defmethod game/draw ((state (eql :defeat)) frame-index)
  (game/draw-ending-string "DEFEAT!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun main ()
  (sdl:with-init ()
     (let ((frame-index 0))
      (sdl:window *window-width* *window-height*
                  :fps (make-instance 'sdl:fps-fixed :target-frame-rate 30))
      (sdl:initialise-default-font sdl:*font-5x7*)
      (initialise-fonts)
      (sdl:with-events (:poll)
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:key-down-event (:key key)
                         (restart-case
                             (game/handle-input *state* key)
                           (continue () :report "Continue interaction")))
        (:idle ()
               (restart-case
                   (progn
                     (incf frame-index)
                     (handle-slime-requests)
                     (sdl:clear-display (getf *square-color* :white))
                     (game/draw *state* frame-index)
                     (sdl:update-display))
                 (continue () :report "Continue interaction")))))))
(defun image-main ()
  "Should be used to start application from the dumped image"
  (let ((*assets-dir-type* :executable-dir))
    (main)))
