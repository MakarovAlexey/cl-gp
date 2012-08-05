;;;; cl-gp.lisp

(in-package #:cl-gp)

(defun make-population (count depth terminals functions)
  (let* ((individuals-in-group-count (/ count (- depth 1))))
    (loop for group-depth from 2 to depth append
	 (loop repeat (/ individuals-in-group-count 2) append
	      (list (make-full-individual terminals functions group-depth)
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
  (let* ((element (elt elements (random (- (length elements) 1)))))
    (if (functionp element)
	(list element
	      (make-grow-node elements terminals maximum-depth (+ current-depth 1))
	      (make-grow-node elements terminals maximum-depth (+ current-depth 1)))
	element)))

(defun crossover (first-parent second-parent)
  (let* ((first-parent-node-id (random (nodes-count first-parent)))
	 (second-parent-node-id (random (nodes-count second-parent)))
	 (first-child (copy-tree first-parent))
	 (second-child (copy-tree second-parent)))
    (setf (node-of first-child first-parent-node-id) (node-of second-child second-parent-node-id)
	  (node-of second-child first-parent-node-id) (node-of first-child second-parent-node-id))
    (values first-child second-child)))

(defun nodes (node)
  (if (listp node)
      (list* node (apply #'concatenate 'list
			 (map 'list #'nodes (rest node))))
      (list node)))

(defun nodes-count (tree)
  (length (nodes tree)))

(defun random-node (individual)
  (node-of individaul (random (nodes-count individual))))

(defun individual-length (individual)
  (if (listp individual)
      (+ 1 (reduce #'max (mapcar #'individual-length individual))) 1))

(defun nodes-count (individual)
  (+ (mapcar #'nodes-countdual-length individual)







(defvar *population*
  (make-population 600 6
		   (list* 'x (loop for x from -5 to 5 collect x))
		   '(+ - * /)))



;; Популяция - вектор фиксированного рзмера с предвариательной инициализацией



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