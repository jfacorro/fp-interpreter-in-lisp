(in-package :com.facorro.tree)
;------------------------------------
(defun make-node (element &rest children)
	"Returns a tree node with the specified chidlren nodes"
	(format t "make-node - [~a] - [~a]~%" element children)
	(cons element children))
;------------------------------------
(defun children (node) 
	"Gets the children from a tree node"
	(rest node))
;------------------------------------
(defun datum (node)
	"Gets the datum from a tree node"
	(first node))
;------------------------------------
(defun add-child (node child)
	(cond ((atom (rest node)) (setf (rest node) (list child)))
		  (t 
			(setf (rest node) (append (rest node) (list child))))))
;------------------------------------
#|
(defparameter *tree* nil)
(setf *tree* (make-node 'root))
(atom (rest *tree*))
(add-child *tree* (make-node 'child1))
*tree*
(add-child *tree* (make-node 'child2))
*tree*
;(make-node 'element (make-node 'bla) (make-node 'bla))
(children *tree* )
*tree*
(add-child (first (children *tree*)) (make-node 'child2-child1))
*tree*
|#