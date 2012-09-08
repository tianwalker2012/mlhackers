(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

;;Why we need to have macro?
;;
(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string 
				"=" (symbol-name name)))))
    `(progn 
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

;;Bind basically doing 2 things
;;Capture the name of *cont*, why?
;;So that the new bind definition will override the old one.
;;Then the expr will have the =values within.
;;This is the code we suppose to capture?
;;Why this is hard for me to understand?
;;I messup the usage of this function. 
;;Body will be the logic of the cont body.
;;It is hard to understood, because it put expr in the middle of parms and body.
;;Parms and body should be the 2 parts of the same thing. 
(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body)))
     ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

;;The continuation code based on home made continuation mechanism
;;Let's diggest it.
;;This is a non continuation implementation
;;Serve as a requirement discription
(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
	(cond ((null tree) (restart-c))
	      ((atom tree) (=values tree))
	      (t (push #'(lambda () (dft-node (cdr tree)))
		       *saved*)
		 (dft-node (car tree)))))
;;So simple?
;;Why?
(=defun restart-c()
	(if *saved*
	    (funcall (pop *saved*))
	    (=values 'done)))

(=defun dft2 (tree)
	(setq *saved* nil)
	;;from this code example, mean the lisp will look for free variable definition by walk alonge the stack trace?
	(=bind (node) (dft-node tree)
	       (cond ((eq node 'done) (=values nil))
		     (t (princ node)
			(restart-c)))))


(defvar *my-saved*)

(defun my-dft(tree)
  (cond ((null tree) (my-restart))
	((atom tree) tree)
	(t (push #'(lambda () (my-dft (cdr tree)))
		 *my-saved*)
	   (my-dft (car tree)))))

(defun my-restart()
  (if *my-saved*
      (funcall (pop *my-saved*))
      'done))


;;Practice for CPS style. 
;;Enjoy it.
;;Something not match.
;;1. Some mismatch here. 
;;
(defun rev (lst)
  (labels ((rev-in (lst fn)
	     (if (atom lst)
		 (funcall fn lst)
		 (rev-in (cdr lst) #'(lambda (w) 
				       ;;(format t "Value:~a" w)
				       (funcall fn (append w (list (car lst))))))
		 )))
    (rev-in lst #'identity)))


;;Following is the process simulation code. 
;;Enjoy the God like feeling. 
(defstruct proc pri state wait)

;;What's the usage of proclaim?
(proclaim '(special *procs* *proc*))

;;Why gensym to a global variable?
(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
			(format t "~%>> ")
			(princ (eval (read)))
			(pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc 
	    :state #'(lambda (,(gensym))
		       ,expr
		       (pick-process))
	    :pri ,pri)
	   *procs*)))

;;Seems program is a collection of processes.
;;What's the meaning of catch?
(defmacro program (name args &body body)
  `(=defun ,name ,args
	   (setq *procs* nil)
	   ,@body
	   (catch *halt* (loop (pick-process)))))

;;We need the definition of pick-process. 
;;So far, I didn't clear about the process logic.
;;based on what I saw now, it is a cooperative multiple process, right?
;;It means I give up control either by calling some block API or proactively.
;;Otherwise nobody could force me to give up control. 
(defun pick-process()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
	  *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun most-urgent-process()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
	(if (> pri max)
	    (let ((val (or (not (proc-wait p))
			   (funcall (proc-wait p)))))
	      (when val
		(setq proc1 p
		      max pri
		      val1 val))))))
    (values proc1 val1)))

;;what the meaning of this call
;;Kind of like yield.
;;I willing to give up the control,
;;But if I am the high priority one, I will get my control back 
;;after the execution.
(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
	(proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

;;The reason for wait, the restart point
(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
	       #'(lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))

;;The process simulation part not tested yet. 
;;It involve some detail complexity. 
;;But the indeterministic operation is really the fundamental complexity. 
;;I love it, it could unleash the brain power by building simple abstraction layer by layer.
;;We can even simulate the lisp programming during our daily thinking process
;;What's the difference between parameter and defvar?
(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn 
	 ,@(mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
		   ;;Why reverse it
		   (reverse (cdr choices)))
	 ,(car choices))
      `(fail)))

;;Seems familiar like the =bind
;;What's the relationship between the 2? 
(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices)
)

(defun cb (fn choices)
  (if choices
      (progn 
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))

(defun fail()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

;;(choose-bind x '(xie tian hou)
;;   (format nil "name:~a ~%" x))
;;name:xie
;;>>(fail)
;;name:tian

;;Let's implement the common lisp version of parlor-tricks.
;;This example illustrate how to combine the continuation and choose and fail
;;To build powerful abstraction which hlep you create app wiht a blink of eyes.
(=defun two-numbers ()
	(choose-bind n1 '(0 1 2 3 4 5 6)
		     (choose-bind n2 '(0 1 2 3 4 5 6)
				  (=values n1 n2))))
;;Could I construct the picture to run?
(=defun parlor-trick (sum)
	(=bind (n1 n2) (two-numbers)
	       ;;(progn (format t "n1: ~a, n2:~a ~%" n1 n2)
	       (if (= (+ n1 n2) sum)
		   `(the sum of ,n1 ,n2)
		   (fail))))

;;Following is the ATNs implementation
;;Where did the defination of "pos" and "regs" come from
(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

;;What's the purpose of this macro. 
;;Let's check several juicy example to find that out.
(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
	  (,next pos ,(compile-cmds cmds))))

;;What's the purpose of this functionality?
;;If *sent* is zero will backtrace. 
(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail)
       (let ((* (nth pos *sent*)))
	 (if (member ',cat (types *))
	     (,next (1+ pos) ,(compile-cmds cmds))
	     (fail)))))

(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

;;This is appearantly a function called by macro. 
;;Who do the compile jobs?
;;LISP not us. Wonderful
(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs))))

(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     ;;What's the meaning of this.
     (if (cdr result) result (car result))))

;;Strange. 
(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
	 (cdr ,regs)))

(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

(defmacro pushr (key val regs)
  `(set-register ',key 
		 (cons ,val (cdr (assoc ',key (car ,regs))))
		 ,regs))

;;Yesterday, The chapter about ATN just drive me right to sleep.
;;Today the topic of prolog just stir my spirit up. 
;;I love it. 
;;Will trace back unconditionally.
;;Is this the expected behavior?
(defmacro with-inference (query &body body)
  `(progn 
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
	    (let ,(mapcar #'(lambda (v)
			      `(,v (fullbind ',v binds)))
			  (vars-in query #'atom))
	      ,@body
	      (fail)))))

;;rep_ will create a tree.
(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))

;;Any example data to illustrate this
(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
			   (fullbind it b)
			   (gensym)))
	((atom x) x)
	(t (cons (fullbind (car x) b)
		 (fullbind (cdr x) b)))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(=defun prove-query (expr binds)
	(case (car expr)
	  (and (prove-and (cdr expr) binds))
	  (or (prove-or (cdr expr) binds))
	  (not (prove-not (cdr expr) binds))
	  (t (prove-simple expr binds))))

(=defun prove-and (clauses binds)
	(if (null clauses)
	    (=values binds)
	    (=bind (binds) (prove-query (car clauses) binds)
		   (prove-and (cdr clauses) binds))))
;;Brilliant, Can come up with the connection between or and trace back
(=defun prove-or (clauses binds)
	(choose-bind c clauses
		     (prove-query c binds)))

(=defun prove-not (expr binds)
	(let ((save-paths *paths*))
	  (setq *paths* nil)
	  (choose (=bind (b) (prove-query expr binds)
			 (setq *paths* save-paths)
			 (fail))
		  (progn 
		    (setq *paths* save-paths)
		    (=values binds)))))

(=defun prove-simple (query binds)
	;;(format t "query:~a ~%" query)
	;;(format t "query:~a ~%" binds)
	(choose-bind r *rlist*
		     (implies r query binds)))

;;Write it down, even if I have no idea what they are doing.
;;I am cultivate a new way of thinking. 
;;Which is another thinking model in my brain.
;;I want to master it. 
(defvar *rlist* nil)

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
		 (car ant)
		 `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con))))))

;;If will fail, fail mean it will come to me?
(=defun implies (r query binds)
	(let ((r2 (change-vars r)))
	  (aif2 (match query (cdr r2) binds)
		(prove-query (car r2) it)
		(fail))))

;;What's the purpose of this function.
;;Why do we need it?
(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
		      (cons v (symb '? (gensym))))
		  (vars-in r #'atom))
	  r))













	    

		 


