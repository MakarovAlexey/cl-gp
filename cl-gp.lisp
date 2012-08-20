;;;; cl-gp.lisp

(in-package #:cl-gp)

(defun make-population (count depth terminals functions)
  (let* ((individuals-in-group-count (/ count (- depth 1))))
    (loop for group-depth from 2 to depth append
	 (loop repeat (/ individuals-in-group-count 2) append
	      (list (make-full-individual terminals functions depth)
		    (make-grow-individual terminals functions group-depth))))))

(defun make-full-individual (terminals functions maximum-depth)
  (list (elt functions (random (- (length functions) 1)))
	(make-full-node terminals functions maximum-depth 2)
	(make-full-node terminals functions maximum-depth 2)))

(defun make-full-node (terminals functions maximum-depth current-depth)
  (if (< current-depth maximum-depth)
      (list* (elt functions (random (- (length functions) 1)))
	     (let ((depth (+ current-depth 1)))
	       (list (make-full-node terminals functions maximum-depth depth)
		     (make-full-node terminals functions maximum-depth depth))))
      (elt terminals (random (- (length terminals) 1)))))

(defun make-grow-individual (terminals functions maximum-depth)
  (let ((elements (append functions terminals)))
    (list (elt functions (random (- (length functions) 1)))
	  (make-grow-node elements terminals maximum-depth 2)
	  (make-grow-node elements terminals maximum-depth 2))))

(defun make-grow-node (elements terminals maximum-depth current-depth)
  (let* ((element (elt elements (random (length elements) ))))
    (if (functionp element)
	(list element
	      (make-grow-node elements terminals maximum-depth (+ current-depth 1))
	      (make-grow-node elements terminals maximum-depth (+ current-depth 1)))
	element)))

(defun node-of (tree path)
  (if (not (null path))
      (node (elt tree (first path)) (rest path))
      tree))

(defun (setf node-of) (node tree path)
  (let ((next-node (elt tree (first path)))
	(next-node-path (rest path)))
    (if (not (null next-node-path))
	(setf (node-of next-node next-node-path) node)
	(setf (elt tree (first path)) node)))
  tree)

(defun random-path (node &optional path)
  (let ((next-node-number (if (not (null path))
			      (random (length node))
			      (+ (random (- (length node) 1)) 1))))
    (if (> next-node-number 0)
	(let ((next-node (elt node next-node-number))
	      (next-node-path (cons next-node-number path)))
	  (if (listp next-node)
	      (random-path next-node next-node-path)
	      (reverse next-node-path)))
	(reverse path))))

(defun crossover (first-parent second-parent)
  (let* ((first-parent-node-path (random-path first-parent))
	 (second-parent-node-path (random-path second-parent))
	 (first-child (copy-tree first-parent))
	 (second-child (copy-tree second-parent)))
    (setf (node-of first-child first-parent-node-path)
	  (node-of second-parent second-parent-node-path)
	  (node-of second-child second-parent-node-path)
	  (node-of first-parent first-parent-node-path))
    (values first-child second-child)))

(defun mutate (individual probability terminals functions maximum-depth)
  (when (< (random 1.0) probability)
    (let ((path (random-path individual)))
      (setf (node-of individual path)
	    (make-grow-individual terminals
				  functions
				  (- maximum-depth (length path)))))))

(defvar *population*
  (make-population 600 6
		   (list* 'x (loop for x from -5 to 5 collect x))
		   '(+ - * /)))

(defparameter *terminals* (list* 'x (loop for x from -5 to 5 collect x)))

(defparameter *functions* '(+ - * /))

(defun fitness (ps os)
  (reduce #'+ (map 'list #'(lambda (p o)
			     (expt (+ p o) 2)) ps os)))

(defparameter *training-set* '((0.0 . 0.000)
			       (0.1 . 0.005)
			       (0.2 . 0.020)
			       (0.3 . 0.045)
			       (0.4 . 0.080)
			       (0.5 . 0.125)
			       (0.6 . 0.180)
			       (0.7 . 0.245)
			       (0.8 . 0.320)
			       (0.9 . 0.405)))

(defparameter *individuals-count* 600)

(defparameter *crossover-probability* 0.9)

(defparameter *mutation-probability* 0.05)

(defparameter *maximum-depth* 6)