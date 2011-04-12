(in-package :com.facorro.debug)

(defparameter *debugging* nil)

(defun debugging? ()
	(null *debugging*))

(defun start-debug ()
	(setf *debugging* t))

(defun stop-debug ()
	(setf *debugging* nil))	