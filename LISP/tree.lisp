(in-package :com.facorro.tree)
;------------------------------------
(defun make-node (element &rest children)
	"Returns a tree node with the specified chidlren nodes"
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