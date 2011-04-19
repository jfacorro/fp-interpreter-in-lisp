(in-package :com.facorro.debug)

(defparameter *debugging-package?* (make-hash-table))

(defun debugging? (name)
	(gethash name *debugging-package?*))

(defun debug-on (name)
	(setf (gethash name *debugging-package?*) t))

(defun debug-off (name)
	"If the package had been registered for debugging sets it off"
	(setf (gethash name *debugging-package?*) nil))

(defun debug-msg (name &rest args)
	"Show debug message for registered group with the value in name"
	(if (debugging? name)
		(apply #'format (cons t args))))