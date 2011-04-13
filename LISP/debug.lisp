(in-package :com.facorro.debug)

(defparameter *debugging* nil)

(defun debugging? ()
	(null *debugging*))

(defun debug-on ()
	(setf *debugging* t))

(defun debug-off ()
	(setf *debugging* nil))	

(defun debug-msg (&rest args)
	(if *debugging*
		(apply #'format (cons t args))))