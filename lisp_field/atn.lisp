(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

;;(defun defnode (name conts &optional yes no)
;;  (setf (gethash name *nodes*)
;;	(make-node :contents conts
;;		   :yes yes
;;		   :no no)))

;;None of it meet the source in the ATN chapter. 
;;Forget about the implementation for a while. Really understand what is the requirement. 
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
	(if yes
	    #'(lambda ()
		(format t "~a~%>> " conts)
		(case (read)
		  (yes (funcall (gethash yes *nodes*)))
		  (t (funcall (gethash no *nodes*)))))
	    #'(lambda () conts))))
