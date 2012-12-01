;;;; cl-gp.lisp

(in-package #:cl-gp)

;; Структура данных - формальное описание программы
;; место генетичесокго программирования в генерации
;; программы при заданных структуре данных:
;; 1) инициализация объектов классов из входных данных (задача классификации)
;; 2) "выращивание" тела определенных в системе методов

;; когда метод специализируют для примитивных типов, то вопрос 
;; об инициализации объекта отпадает

;; 1) определить метакласс, поисать поведение так, чтобы
;;    класс объекта был всегда известен.
;;    Также 
;; 2) написать подсистему задания условий для методов - unit-тесты

(defvar *exceptions* '((/ . (0))))

(defun function-terminals (function terminals)
  (reduce #'(lambda (terminals exception)
	      (remove exception terminals))
	  (rest (assoc function *exceptions*))
	  :initial-value terminals))

(defvar *terminals*)
(defvar *functions*)
(defvar *maximum-depth*)
(defvar *fitness-function*)
(defvar *training-set*)
(defvar *mutation-probability*)
(defvar *crossover-probability*)
(defvar *tournament-size*)

(defclass individual ()
  ((code :initarg :code
	 :reader code-of)
   (fitness :reader fitness-of))
  (:metaclass closer-mop:funcallable-standard-class))

(defun used-p (terminal code)
  (reduce #'(lambda (p c)
	      (or p (if (listp c)
			(used-p terminal c)
			(eq c terminal))))
	  code :initial-value nil))

(defun make-function (code terminals)
  (let* ((arguments (remove-if-not #'symbolp terminals))
	 (ignored-arguments (remove-if #'(lambda (terminal)
					   (used-p terminal code)) arguments)))
    (eval `(lambda ,arguments
	     (declare (ignore ,@ignored-arguments))
	     ,code))))

(defmethod initialize-instance :after ((instance individual) &key code)
  (closer-mop:set-funcallable-instance-function instance
						(make-function code *terminals*))
  (setf (slot-value instance 'fitness)
	(handler-case
	    (funcall *fitness-function* instance *training-set*)
	  (error () most-positive-double-float))))

(defclass program ();; ввести константы и исключить их из терминалов
  ((terminals :initarg :terminals
	      :accessor terminals-of)
   (functions :initarg :functions
	      :accessor functions-of)
   (fitness-function :initarg :fitness-function
		     :accessor fitness-function-of)
   (training-set :initarg :training-set
		 :accessor training-set-of)
   (individuals-count :initarg :individuals-count
		      :accessor individuals-count-of)
   (crossover-probability :initarg :crossover-probability
			  :accessor crossover-probability-of)
   (mutation-probability :initarg :mutation-probability
			 :accessor mutation-probability-of)
   (maximum-depth :initarg :maximum-depth
		  :accessor maximum-depth-of)
   (generations-count :initarg :generations-count
		      :accessor generations-count-of)
   (tournament-size :initarg :tournament-size
		    :accessor tournament-size-of)))

(defun make-full-code (terminals functions maximum-depth)
  (let ((node-function (elt functions (random (- (length functions) 1)))))
    (list node-function
	  (make-full-node terminals functions maximum-depth 2)
	  (make-full-node (function-terminals node-function terminals)
			  functions maximum-depth 2))))

(defun make-full-node (terminals functions maximum-depth current-depth)
  (if (< current-depth maximum-depth)
      (let ((node-function (elt functions (random (- (length functions) 1)))))
	(list* node-function
	       (let ((depth (+ current-depth 1)))
		 (list (make-full-node terminals functions maximum-depth depth)
		       (make-full-node (function-terminals node-function terminals)
				       functions maximum-depth depth)))))
      (elt terminals (random (- (length terminals) 1)))))

(defmethod make-full-individual (depth terminals functions fitness-function training-set)
  (make-instance 'individual
		 :code (make-full-code terminals functions depth)))

(defun make-grow-code (terminals functions maximum-depth)
  (let ((node-function (elt functions (random (- (length functions) 1)))))
    (list (elt functions (random (- (length functions) 1)))
	  (make-grow-node terminals functions maximum-depth 2)
	  (make-grow-node (function-terminals node-function terminals)
			  functions maximum-depth 2))))

(defun make-grow-node (terminals functions maximum-depth current-depth)
  (let* ((elements (append functions terminals))
	 (element (elt elements (random (length elements)))))
    (if (not (null (find element functions)))
	(list element
	      (make-grow-node terminals functions maximum-depth (+ current-depth 1))
	      (make-grow-node (function-terminals element terminals)
			      functions maximum-depth (+ current-depth 1)))
	element)))

(defmethod make-grow-individual (depth terminals functions fitness-function training-set)
  (make-instance 'individual
		 :code (make-grow-code terminals functions depth)))

;;(defun make-population (count depth terminals functions fitness-function training-set)
;;  (let* ((individuals-in-group-count (/ count (- depth 1))))
;;    (loop for group-depth from 2 to depth append
;;	 (loop repeat (/ individuals-in-group-count 2) append
;;	      (list (make-full-individual
  ;;                       depth
;;                         terminals
;;                         functions
;;                         fitness-function
;;                         training-set)
;;                    (make-grow-individual
;;                         group-depth
;;                         terminals
;;                         functions
;;                         fitness-function
;;                         training-set))))))

(defun make-population (count depth terminals functions fitness-function training-set)
  (loop repeat count collecting
    (make-grow-individual depth terminals functions fitness-function training-set)))

(defun node-of (tree path)
  (if (not (null path))
      (node-of (elt tree (first path)) (rest path))
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
	 (second-child second-parent))
    (setf (node-of first-child first-parent-node-path)
	  (node-of second-parent second-parent-node-path)
	  (node-of second-child second-parent-node-path)
	  (node-of first-parent first-parent-node-path))
    (list first-child second-child)))

(defun reproduce (first-parent second-parent)
  (list (copy-tree first-parent)
        (copy-tree second-parent)))

(defun mutate (code)
  (let ((path (random-path code)))
    (setf (node-of code path)
          (make-grow-code *terminals*
                          *functions*
                          (- *maximum-depth* (length path)))))
  code)

(defun compete (population tournament-size)
  (let ((competitors (sort (loop repeat tournament-size
			      for individuals = population then (remove individual individuals)
			      for individual = (elt individuals (random (1- (length individuals))))
			      collect individual) #'> :key #'fitness-of)))
    (values (first competitors)
	    (elt competitors (1- tournament-size)))))

;;(defun select (population tournament-size)
;;  (multiple-value-bind (winner-1 looser-1) (compete population tournament-size)
;;    (multiple-value-bind (winner-2 looser-2) (compete (remove looser-1
;;							      (Remove winner-1 population))
;;						      tournament-size)
;;      (values (crossover winner-1 winner-2)
;;	      (list looser-1 looser-2)))))

(defun maybe (function probability default)
  (if (<= (random 100) probability)
      (apply function default)
      default))

(defun recombine-code (code1 code2)
  (maybe #'(lambda (&rest codes)
	     (mapcar #'mutate codes))
	 *mutation-probability*
	 (maybe #'crossover *crossover-probability*
		(reproduce code1 code2))))

(defun recombine (parent1 parent2)
  (mapcar #'(lambda (code)
	      (make-instance 'individual :code code))
	  (recombine-code (code-of parent1)
			  (code-of parent2))))

(defun best-individual (individuals)
  (reduce #'(lambda (previous current)
	      (if (and (not (null previous))
		       (> (fitness-of previous)
			  (fitness-of current)))
		  current
		  previous))
	  individuals))

(defun run (instance)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((*terminals* (terminals-of instance))
	(*functions* (functions-of instance))
	(*maximum-depth* (maximum-depth-of instance))
	(*fitness-function* (fitness-function-of instance))
	(*training-set* (training-set-of instance))
	(*mutation-probability* (mutation-probability-of instance))
	(*crossover-probability* (crossover-probability-of instance))
	(*tournament-size* (tournament-size-of instance)))
    (best-individual
     (loop repeat (generations-count-of instance)
	   for population = (make-population (individuals-count-of instance)
					     (maximum-depth-of instance)
					     (terminals-of instance)
					     (functions-of instance)
					     (fitness-function-of instance)
					     (training-set-of instance))
	     then (append (make-population (- (individuals-count-of instance)
					      (length result))
					   (maximum-depth-of instance)
					   (terminals-of instance)
					   (functions-of instance)
					   (fitness-function-of instance)
					   (training-set-of instance)) result)
	   for result = (recombine (compete population (tournament-size-of instance))
				   (compete population (tournament-size-of instance)))
	     then (recombine (compete population (tournament-size-of instance))
			     (compete population (tournament-size-of instance)))
	   for best-individual = (best-individual population)
	     then (best-individual population)
	   do (format t "best individual fitess ~a~%" (fitness-of best-individual))
	   collect best-individual))))

(defun squared-error-fitness (function training-set)
  (reduce #'(lambda (result training-instance)
	      (+ (destructuring-bind (i . o) training-instance
		   (expt (- (funcall function i) o) 2))
		 result))
	  training-set :initial-value 0))


;; select -> reproduction -> crossover -> mutation (two individuals) -> replace whorse

(defvar *program*
  (make-instance 'program
		 :generations-count 100
		 :terminals (list* 'x (loop for x from -5 to 5 collect x))
		 :functions '(+ - * /)
		 :fitness-function #'squared-error-fitness
		 :training-set (mapcar #'(lambda (x)
					   (cons x (/ (expt x 2) 2)))
				       '(0.0 0.1 0.2 0.3 0.4 0.5
					 0.6 0.7 0.8 0.9))
		 :individuals-count 600
		 :crossover-probability 90
		 :mutation-probability 5
		 :maximum-depth 10
		 :tournament-size 4))

;;(setf (maximum-depth-of *program*) 7)

;;(mapcar #<INDIVIDUAL {100320C17B}> '(0.0 0.1 0.2 0.3 0.4 0.5
;;					 0.6 0.7 0.8 0.9))