(in-package :com.facorro.tree)
;------------------------------------
(defun make-tree-node (element &rest children)
	"Returns a tree node with the specified chidlren nodes"
	(cond (
			(null children) (list element))
			(t
				(cons element (list children)))))
;------------------------------------
(defun get-children (node) 
	"Gets the children from a tree node"
	(rest node))
;------------------------------------
(defun get-datum (node)
	"Gets the datum from a tree node"
	(first node))
;------------------------------------
(defun add-child (node child)
	(cond ((atom (rest node)) (setf (rest node) (list child)))
		  (t 
			(setf (rest node) (append (list child) (rest node))))))
;------------------------------------
#|
(defparameter *tree* nil)
(setf *tree* (make-tree-node 'root))
(atom (rest *tree*))
(add-child *tree* (make-tree-node 'child1))
*tree*
(add-child *tree* (make-tree-node 'child2))
*tree*
;(make-tree-node 'element (make-tree-node 'bla) (make-tree-node 'bla))
(get-children *tree* )
*tree*
(add-child (first (get-children *tree*)) (make-tree-node 'child2-child1))
*tree*
|#