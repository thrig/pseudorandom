; ported from my Game::PseudoRand Perl module with not much testing and
; some new features such as the CLAMP to restrict the odds from
; wandering too far off

(defpackage #:pseudorandom
  (:use #:common-lisp #:alexandria)
  (:export #:prd-bistep #:prd-bitable #:prd-step #:prd-table))
(in-package #:pseudorandom)

(defun prd-bistep
       (start step-hit step-miss &key (rand #'random) (min 0.0) (max 1.0))
  "bi-directional adjustment of odds after a hit or a miss"
  (let ((odds start))
    (list
     (lambda ()
       (setf odds (clamp odds min max))
       (if (<= (funcall rand 1.0) odds)
           (prog1 t (incf odds step-hit))
           (prog1 nil (incf odds step-miss))))
     (lambda () (setf odds start)))))

(defun prd-step
       (start step
        &key (rand #'random) (min 0.0) (max 1.0) (reset nil reset-p))
  "reset of odds after hit otherwise change by step"
  (let ((odds start) (rst (if reset-p reset start)))
    (list
     (lambda ()
       (setf odds (clamp odds min max))
       (if (<= (funcall rand 1.0) odds)
           (prog1 t (setf odds rst))
           (prog1 nil (incf odds step))))
     (lambda () (setf odds start)))))

(defun prd-bitable
       (start table-hit table-miss &key (rand #'random) (min 0.0) (max 1.0)
        (index-hit 0 index-hit-p)
        (index-miss 0 index-miss-p))
  "bi-directional adjustment of odds from tables after hit or miss"
  (let ((odds start)
        (idx-hit (if index-hit-p index-hit 0))
        (idx-miss (if index-miss-p index-miss 0)))
    (list
     (lambda ()
       (setf odds (clamp odds min max))
       (if (<= (funcall rand 1.0) odds)
           (prog1 t
             (setf idx-miss 0)
             (incf odds (nth idx-hit table-hit))
             (setf idx-hit (mod (1+ idx-hit) (list-length table-hit))))
           (prog1 nil
             (setf idx-hit 0)
             (incf odds (nth idx-miss table-miss))
             (setf idx-miss (mod (1+ idx-miss) (list-length table-miss))))))
     (lambda ()
       (setf odds start)
       (setf idx-hit (if index-hit-p index-hit 0))
       (setf idx-miss (if index-miss-p index-miss 0))))))

(defun prd-table
       (start table &key (rand #'random) (min 0.0) (max 1.0)
        (index 0 index-p) (reset nil reset-p))
  "reset of odds after hit otherwise change by table lookup"
  (let ((odds start)
        (idx (if index-p index 0))
        (rst (if reset-p reset start)))
    (list
     (lambda ()
       (setf odds (clamp odds min max))
       (if (<= (funcall rand 1.0) odds)
           (prog1 t (setf odds rst) (setf idx 0))
           (prog1 nil
             (incf odds (nth idx table))
             (setf idx (mod (1+ idx) (list-length table))))))
     (lambda ()
       (setf odds start)
       (setf idx (if index-p index 0))))))
