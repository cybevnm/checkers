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
(defun cons-eql-p (a b)
  (and (eql (car a) (car b))
       (eql (cdr a) (cdr b))))
(defun stride (seq step &key (start-from 0))
  (loop for i from start-from below (length seq) by step
     collecting (nth i seq)))
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
(defun board/actions (board check &key src tgt recursive-action)
  (let ((result))
    (labels ((moves-actions (src tgt)
               (doplist (_ move *moves*)
                 (when (move/validp move board check src tgt)
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
              (remove-if-not (curry #'cons-eql-p src) result :key #'action/src)))
      (when tgt
        (setf result 
              (remove-if-not (curry #'cons-eql-p tgt) result :key #'action/tgt)))
      (when recursive-action
        (setf result
              (remove-if-not (lambda (a)
                               (and (eq (action/move recursive-action) (action/move a))
                                    (if (action/src recursive-action)
                                        (cons-eql-p (action/src recursive-action) (action/src a))
                                        t)
                                    (if (action/tgt recursive-action)
                                        (cons-eql-p (action/tgt recursive-action) (action/tgt a))
                                        t)))
                             result))))
    result))
(defun board/actions-for-color (board color &key src tgt recursive-action)
  ;; TODO: fix
  (board/actions board
                 (ecase color
                   (:white :white-check)
                   (:black :black-check))
                 :src src
                 :tgt tgt
                 :recursive-action recursive-action))
(defun $board/action-valid-p (actions move &key src tgt)
  (member (make-instance 'action :move move :src src :tgt tgt)
          actions
          :test #'action/eql))
(defun board/action-valid-p (move board check &key src tgt recursive-action)
  ($board/action-valid-p (board/actions board
                                        check
                                        :src src
                                        :tgt tgt
                                        :recursive-action recursive-action)
                         move
                         :src src
                         :tgt tgt))
(defun board/action-valid-for-color-p (move board color &key src tgt recursive-action)
  ($board/action-valid-p (board/actions-for-color board
                                                 color
                                                 :src src
                                                 :tgt tgt
                                                 :recursive-action recursive-action)
                         move
                         :src src
                         :tgt tgt))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun square-contains-friendly-check-p (board curr-check pos)
  (alet (board/square board (car pos) (cdr pos))
    (and (eq (check/color it) (check/color curr-check))
         (not (check/kingp it)))))
(defun square-contains-friendly-king-p (board curr-check pos)
  (alet (board/square board (car pos) (cdr pos))
    (and (eq (check/color it) (check/color curr-check))
         (check/kingp it))))
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
(defun hor-and-ver-distances-are-the-same (board curr-check src-pos tgt-pos)
  (declare (ignore board curr-check))
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
(defun line-contains-one-enemy-check-p (board curr-check src-pos tgt-pos)
  (let ((enemy-checks-num 0))
    (map-line-squares board
                      src-pos
                      tgt-pos
                      (lambda (x y square)
                        (declare (ignore x y))
                        (when (and (eq (check/color square)
                                       (check/opposite-color curr-check)))
                          (incf enemy-checks-num))))
    (= enemy-checks-num 1)))
(defun line-contains-enemy-check-before-target-p (board curr-check src-pos tgt-pos)
  (let ((dx (alet (- (car src-pos) (car tgt-pos)) (/ it (abs it))))
        (dy (alet (- (cdr src-pos) (cdr tgt-pos)) (/ it (abs it)))))
    (eq (check/color (board/square board (+ (car tgt-pos) dx) (+ (cdr tgt-pos) dy)))
        (check/opposite-color curr-check))))
(defun count-checks-on-line (board curr-check src-pos tgt-pos)
  "Returns cons cell where first value is number of friendly checks 
   and second value is number of enemy checks"
  (let ((friendly-checks-num 0)
        (enemy-checks-num 0))
    (map-line-squares board
                      src-pos
                      tgt-pos
                      (lambda (x y square)
                        (declare (ignore x y))
                        (when (and (eq (check/color square)
                                       (check/color curr-check)))
                          (incf friendly-checks-num))
                        (when (and (eq (check/color square)
                                       (check/opposite-color curr-check)))
                          (incf enemy-checks-num))))
    (cons friendly-checks-num enemy-checks-num)))
(defun line-contains-no-friendly-checks-p (board curr-check src-pos tgt-pos)
  (zerop (car (count-checks-on-line board curr-check src-pos tgt-pos))))
(defun line-contains-no-enemy-checks-p (board curr-check src-pos tgt-pos)
  (zerop (cdr (count-checks-on-line board curr-check src-pos tgt-pos))))
(defun move-check (board curr-check src-pos tgt-pos)
  (board/move-check board 
                    (car src-pos) (cdr src-pos) 
                    (car tgt-pos) (cdr tgt-pos))
  (when (and (not (check/kingp curr-check))
             (board/kings-row-p board
                                (check/opposite-color curr-check)
                                (cdr tgt-pos)))
    (setf (board/square board (car tgt-pos) (cdr tgt-pos))
          (check/king-for-check curr-check))))
(defun kill-enemy-checks-on-line (board curr-check src-pos tgt-pos)
  (map-line-squares board
                    src-pos
                    tgt-pos
                    (lambda (x y square)
                      (when (and (eq (check/color square)
                                     (check/opposite-color curr-check)))
                        (board/kill-check board x y)))))
(defclass move ()
  ((recursive :initarg :recursive :reader move/recursivep)
   (mandatory :initarg :mandatory :reader move/mandatoryp)
   (predicate :initarg :predicate :reader move/predicate)
   (modifier :initarg :modifier :reader move/modifier)))
(defparameter *moves* nil)
(defun move/validp (move board curr-check src-pos tgt-pos)
  (funcall (move/predicate move) board curr-check src-pos tgt-pos))
(defun move/apply (move board curr-check src-pos tgt-pos)
  "Returns t if move is recursive and should be continued"
  (flet ((valid-for-any-pos-p (src-pos2)
           (doboard (x y _ board)
             (when (move/validp move board curr-check src-pos2 (cons x y))
               (return-from valid-for-any-pos-p t)))))
    (funcall (move/modifier move) board curr-check src-pos tgt-pos)
    (and (move/recursivep move) (valid-for-any-pos-p tgt-pos))))
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
  (defun move/parse-mod-func-def (def)
    (values (first def) (cdr def)))
  (defun move/gen-mod-func-call (def)
    (multiple-value-bind (name args-rest) (move/parse-mod-func-def def)
      (append (list name) '(board curr-check) args-rest))))
(defmacro defmove (name (&rest attributes) (&rest predicates) (&rest modifiers))
  `(setf (getf *moves* ',name)  
         (make-instance 
          'move
          :recursive (member 'recursive ',attributes)
          :mandatory (member 'mandatory ',attributes)
          :predicate (lambda (board curr-check source target)
                       (and ,@(loop for p in predicates
                                 collecting (move/gen-pred-func-call p))))
          :modifier (lambda (board curr-check source target)
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
(defun action/eql (a b)
  (and (eql (action/move a) (action/move b))
       (cons-eql-p (action/src a) (action/src b))
       (cons-eql-p (action/tgt a) (action/tgt b))))
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
                                                (board/check modified-board
                                                             (car src)
                                                             (cdr src))
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
    (let* ((actions (board/actions-for-color board
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
  (let ((actions (board/actions-for-color board
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
                 (check (board/check modified-board
                                     (car src)
                                     (cdr src)))
                 (recursivep (move/apply (action/move a)
                                         modified-board
                                         check
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
  (let ((curr-recursive-action))
    (loop
       :do (multiple-value-bind (rate action)
               (funcall processor
                        board
                        :black
                        :recursive-action curr-recursive-action)
             (declare (ignorable rate))
             (assert (action/fullp action))
             (setf curr-recursive-action
                   (and (move/apply (action/move action)
                                    board
                                    (board/check board
                                                 (car (action/src action))
                                                 (cdr (action/src action)))
                                    (action/src action)
                                    (action/tgt action))
                        (make-instance 'action
                                       :move (action/move action)
                                       :src (action/tgt action)))))
       :while curr-recursive-action)))
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
;;; window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *window-width* 400)
(defparameter *window-height* 400)
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
(defparameter *src-square-x* 3)
(defparameter *src-square-y* 3)
(defparameter *tgt-square-x* nil)
(defparameter *tgt-square-y* nil)
(defparameter *player-recursive-action* nil)
(defparameter *board* (board/make-1))
(defun window/tgt-square-defined-p ()
  (and *tgt-square-x* *tgt-square-y*))
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
  (let ((src-square (board/square board *src-square-x* *src-square-y*)))
    (when (and (not (eq src-square :empty))
               (eq (check/color src-square) :white))
      (dolist (a (board/actions board 
                                src-square
                                :src (cons *src-square-x* *src-square-y*)
                                :recursive-action *player-recursive-action*))
        (sdl:draw-box-* (* (car (action/tgt a)) square-w)
                        (* (window/calc-inv-y board (cdr (action/tgt a))) square-h)
                        square-w 
                        square-h
                        :color (sdl:color :r 60 :g 60 :b 60)
                        :alpha 125)))))
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
  (when (eq (check/color (board/square board *src-square-x* *src-square-y*))
            :white)
    (setf *tgt-square-x* *src-square-x*
          *tgt-square-y* *src-square-y*)))
(defun window/find-first-valid-move (board src tgt)
  (find-if (lambda (move)
             (and (eq (type-of move) 'move)
                  (board/action-valid-p move
                                        board
                                        (board/check board
                                                     *src-square-x*
                                                     *src-square-y*)
                                        :src src
                                        :tgt tgt
                                        :recursive-action *player-recursive-action*)))
           *moves*))
(defun window/apply-move-if-possible (board)
  (let* ((src (cons *src-square-x* *src-square-y*))
         (tgt (cons *tgt-square-x* *tgt-square-y*))
         (move (window/find-first-valid-move board src tgt)))
    (when move
      (setf *src-square-x* *tgt-square-x*
            *src-square-y* *tgt-square-y*)
      (let ((recursive-move (move/apply move *board* :white-check src tgt)))
        (setf *player-recursive-action* (and recursive-move
                                             (make-instance 'action
                                                            :move move
                                                            :src tgt)))
        (unless recursive-move
          (ai/process *board*)))))
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
