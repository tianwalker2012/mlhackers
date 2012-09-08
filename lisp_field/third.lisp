(defun lrec (rec &optional base)
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda ()
			      (self (cdr lst)))))))
    #'self))

(defun trev (rec &optional base)
  (labels ((self (tr)
	     (if (atom tr)
		   (if (functionp base)
		       (funcall base tr)
		       tr)
		   (funcall rec #'(lambda() (self (car tr))) #'(lambda() (self (cdr tr)))))))
    #'self))

(defun tr-if (tr fn)
  (funcall (trev #'(lambda (left right) (or (funcall left) (funcall right))) fn) tr
 )
)

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			     ,(rec (cdr fns)))
			  g)))
		(rec fns)))))


(defmacro fn (expr) 
  `#',(rbuild expr))


(defun rbuild (expr)
  (if (or (atom expr) (eql (car expr) 'lambda))
      expr
      (if (eql (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
      (,op ,@(mapcar #'(lambda (f)
			 `(,(rbuild f) ,g))
		     fns)))))

;;Why not turn this into 2 macro
;;Let's try to understand this macro first. 
;;How to use?
;;What the effect of calling it?
;;This is a macro generate side effect
;;The calling of it will generate another macro, 
;;this short macro are calling long within.
;;the first comma was to defense against the first anticomma. 
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))


(defun group (names n)
  (labels ((group-in (lst llst cg gcount lcount)
	     (let ((elm (car lst)) (remain (cdr lst)))
	       (if remain
		   (if (= lcount 1)
		       (group-in remain (cons (reverse (cons elm cg)) llst) nil gcount gcount)
		       (group-in remain llst (cons elm cg) gcount (- lcount 1)))
		   (reverse (cons (reverse (cons elm cg)) llst))))))
    (group-in names nil nil n n)))


(defmacro abbrevs (&rest names)
  `(progn ,@(mapcar #'(lambda (pair) `(abbrev ,(car pair) ,(cadr pair))) (group names 2))) 
)	

;;Cool, I love this game.
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

;;You can easily scale your single case into larger chunk of data. 
;;This is so easy in lisp. Why? It is so easy to do so in lisp?
;;Think about it.
(defmacro propmacros (&rest propnames)
  `(progn ,@(mapcar #'(lambda (name) `(propmacro ,name)) propnames)))

;;encapsulate a context into the exprs,
;;so by the end of each expr will have a it's return value captured by a varible
;;it.
(defmacro asingle (fn &rest exprs)
  `(let ((it nil))
     (,fn ,@(mapcar #'(lambda (expr) `(setf it ,expr)) exprs))) 
)

;;How to manipulate the quote string?
(defmacro ap-gen (afn fn)
  `(defmacro ,afn(&rest exprs)
     `(let ((it nil))
       (,',fn ,@(mapcar #'(lambda (expr) `(setf it ,expr)) exprs))))
)

(set-dispatch-macro-character #\# #\?
    #'(lambda (stream char1 char2)
	`#'(lambda (&rest ,(gensym))
	     ,(read stream t nil t))))


;;Let carefully examine the destrcut about what it was doing.
(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b) 
				     (if (consp (car b))
					 (car b)
					 b))
				 binds)
	 ,(wplac-ex (mapcan #'(lambda (b) 
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))


  