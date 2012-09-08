;;Property have inheritance mechanism.
;;What is inheritance?
;;Travel the inheritance chain to get property or method
(defun rgetold (obj prop)
  (some2 #'(lambda (a) (gethash prop a))
	 (get-ancestors obj)))

;;Get all the ancesstors and remove duplication and sort by parent and grand-parents
;;The sort condition was interesting.
;;How to delete duplication?
;;LISP have no difficulty to support multiple inheritance.
(defun get-ancestors (obj)
  (labels ((getall (x)
	     (append (list x)
		     (mapcan #'getall
			     (gethash 'parents x)))))
    (stable-sort (delete-duplicates (getall obj))
		 #'(lambda (x y)
		     (member y (gethash 'parents x))))))

;;Found things from the list once find then quit
(defun some2 (fn lst)
  (if (atom lst)
      nil
  (multiple-value-bind (val win) (funcall fn (car lst))
    (if (or val win)
	(values val win)
	(some2 fn (cdr lst)))))
)


(defstruct meth around before primary after)

;;turn a method into call?
;;What's the purpose meth-p? make sure the object is a method?
;;Hold the question a little bit. Let's move ahead
(defmacro meth- (field obj)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (and (meth-p ,gobj)
	    (,(symb 'meth- field) ,gobj)))))

;;Where is the logic from most specific to the least specific call chain logic.
(defun run-methods (obj name args)
  (let ((pri (rget obj name :primary)))
    (if pri
	(let ((ar (rget obj name :around)))
	  (if ar
	      (apply ar obj args)
	      (run-core-methods obj name args pri)))
	(error "No primary ~A method for ~A." name obj))))

;;I guess, the before and after should have chance to intervene with the returned value, right?
;;It depends on what do you want from the before and after. 
;;If an aspect so strong to need to intervene with particular value of the method
;;I guess it no more a aspect, right?
(defun run-core-methods (obj name args &optional pri)
  (multiple-value-prog1
      (progn (run-befores obj name args)
	     (apply (or pri (rget obj name :primary))
		    obj args))
    (run-afters obj name args)))

;;What's the meaning of (skip 0)
;;Mean the default value for this argument is zero?
;;Interesting. Let's go ahead to write down the code.
(defun rget (obj prop &optional meth (skip 0))
  (some2 #'(lambda (a)
	     (multiple-value-bind (val win) (gethash prop a)
	       (if win
		   (case meth 
		     (:around (meth- around val))
		     (:primary (meth- primary val))
		     (t (values val win))))))
	 (nthcdr skip (ancestors obj))))

;;Simple and straightforward.
(defun run-befores (obj prop args)
  (dolist (a (ancestors obj))
    (let ((bm (meth- before (gethash prop a))))
      (if bm (apply bm obj args)))))


		   
 (defun my-run-afters (obj prop args)
   (dolist (a (reverse (ancestors obj)))
     (let ((bm (meth- after (gethash prop a))))
       (if bm (apply bm obj args)))))

;;Why PG's run after so complicated, did I miss some cases
;;Let's typing the code and check the details
;;All I find is try to save the reverse call.
(defun run-afters (obj prop args)
  (labels ((rec (lst)
	     (when lst
	       (rec (cdr lst))
	       (let ((am (meth- after
				(gethash prop (car lst)))))
		 (if am (apply am (car lst) args))))))
    (rec (ancestors obj))))


;;Define method marco show it's myth face to us now. 
(defmacro defmeth ((name &optional (type :primary))
		   obj parms &body body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       ;;what's the purpose of following call? why do we need it?
       ;;I guess this is for the name can be called as method. 
       (defprop ,name t)
       (unless (meth-p (gethash ',name ,gobj))
	 (setf (gethash ',name ,gobj) (make-meth)))
       (setf (,(symb 'meth- type) (gethash ',name ,gobj))
	     ,(build-meth name type gobj parms body)))))

;;Add all the method. by the end of today.
;;Enjoy eveyday.
;;Fine tune the system. Make your productive thinker and a productive doer.
;;Cook food now.
;;Better see a juicy example, then we can understand the usage better. 
(defun build-meth (name type gobj parms body)
  (let ((gargs (gensym)))
    `#'(lambda (&rest ,gargs)
	 (labels
	     ((call-next ()
		,(if (or (eq type :primary)
			 (eq type :around))
		     `(cnm ,gobj ',name (cdr ,gargs) ,type)
		     `(error "Illegal call-next.")))
	      (next-p ()
		,(case type
		       (:around
			`(or (rget ,gobj ',name :around 1)
			     (rget ,gobj ',name :primary)))
		       (:primary
			`(rget ,gobj ',name :primary 1))
		       (t nil))))
	   (apply #'(lambda ,parms ,@body) ,gargs)))))

(defun cnm (obj name args type)
  (case type
    (:around (let ((ar (rget obj name :around 1)))
	       (if ar 
		   (apply ar obj args)
		   (run-core-methods obj name args))))
    (:primary (let ((pri (rget obj name :primary 1)))
		(if pri
		    (apply pri obj args)
		    (error "No next method."))))))

(defmacro undefmeth ((name &optional (type :primary)) obj)
  `(setf (,(symb 'meth- type) (gethash ',name ,obj))
	 nil))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (gethash 'parents obj) parents)
    ;;I guest this method is to sort the ancestors according to the inheritance relationship
    (ancestors obj)
    obj))

;;Sort job happen within the ancestors
(defun ancestors (obj)
  (or (gethash 'ancestors obj)
      (setf (gethash 'ancestors obj) (get-ancestors obj))))


(defmacro defprop (name &optional meth?)
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
	    `(run-methods obj ',name args)
	    `(rget obj ',name)))
     (defsetf ,name (obj) (val)
       `(setf (gethash ',',name ,obj) ,val))))
