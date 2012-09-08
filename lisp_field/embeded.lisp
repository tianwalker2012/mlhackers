;;Facility code
;;Combine what you put in the parameter list into a string
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defmacro aif(test then &optional else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

;;Why do we need this?
;;What it can help us?
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))



;;Begin of Generalized variables
;;GenSym in bulck.
(defmacro with-gensyms(vals &body body)
  `(let (,@(mapcar #'(lambda (elm) `(,elm (gensym))) vals))
     ,@body)
)

;;Set all value to one value
(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval)) args)))))


(defmacro nilf (&rest args)
  `(allf nil ,@args))

(defmacro tf (&rest args) 
  `(allf t ,@args))


(defmacro toggle(&rest args)
  `(progn ,@(mapcar #'(lambda (elm) `(toggle2 ,elm)) args)))

(define-modify-macro toggle2() not)

;;I don't know what's the purpose of following function.
;;Should I visualize what they are doing?
;;How define-modify-macro work?
(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj) 
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

;;For simplify the set and get in macro
;;we will define a f which work in following way
;;(setf (obj-dx o) (* (obj-dx o) factor)
;;(_f * (obj-dx o) factor)
;;Following is the incorrect definition
(defmacro _f-error (op place &rest args)
  `(setf ,place (,op ,place ,@args)))

;;To implement a correct version of f, we need help from system level. 
;;get-setf-method
;;Have renamed to GET-SETF-EXPANSION
;;Correct version
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    ;;(format t "vars ~a, forms ~a, var ~a, set ~a, access ~a" vars forms var set access)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access) 
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	      ,set))))


(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
	      ,@(mapcar #'list vars forms)
	      (,glst ,access)
	      (,(car var) (nthcdr ,gn ,glst)))
	 ;;I don't know why do we need this?
	 ;;didn't the nthcdr already the job done?
	 ;;It is mean return the poped the value.
	 ;;Why have to call before?
	 ;;Because the glst have changed.
	 (prog1 (subseq ,glst 0 ,gn)
	   ,set)))))

;;Fully understand the _f 
;;Back to the embeded language. 

;;DB query language
(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
	  ',args)
)

;;First is the intepreter version of query.
(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v) 
			 `(,v (binding ',v ,binds)))
		     (vars-in query #'atom))
	 ,@body))))

;;You can't assume this is only for gethash usage.
(defmacro aif2 (test then &optional else)
  (let ((second (gensym)))
    `(multiple-value-bind (it ,second) ,test
       (if (or ,second it)
	   ,then
	   ,else))))

(defun interpret-query (expr &optional binds)
  (case (car expr)
    ;;I don't get it, why reverse the expr?
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lookup (car expr) (cdr expr) binds))))

(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b) (interpret-query (car clauses) b))
	      (interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
	      (interpret-query c binds)) 
	  clauses))

(defun interpret-not (clauses binds)
  (if (interpret-query clauses binds)
      nil
      (list binds)))

(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x) 
	      (aif2 (match x args binds) (list it)))
	  (db-query pred)))


;;What's my expectation to destruc?
;;It should be able to generate the pair of for used in let. 
(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))
	(if rest
	    ;;This part I don't quite get, why have an extra enclose sign?
	    `((,rest (subseq ,seq ,n)))
	    (let ((p (car pat))
		  (rec (destruc (cdr pat) seq atom? (1+ n))))
	      (if (funcall atom? p)
		  (cons `(,p (elt ,seq ,n))
			rec)
		  (let ((var (gensym)))
		    (cons (cons `(,var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))


(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b) 
			 (if (consp (car b))
			     (car b);;what about the second half?
			     b))
		     binds)
	 ,(dbind-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))
					   

;;Following is the example dbind support
;;1. (dbind (a b c) #(1 2 3) (list a b c))
;;2. (dbind (a (b c) d) '(1 #(2 3) 4) (list a b c d))
;;3. (dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)) (list a b c d)
;;The result of 3 is: 1 #\f "ribble" (2 3 4)
(defmacro dbind(pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body)))
)

;;Walk through the code, I got the purpose of this functionality.
;;Which is cool
(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1) (col -1)) 
		  (mapcan 
		   #'(lambda (pat)
		       (incf row)
		       (setq col -1)
		       (mapcar #'(lambda (p)
				   `(,p (aref ,gar 
					      ,row
					      ,(incf col)))) 
			       pat))
		   pats))
	 ,@body))))


;;This is the simplified version for the with-matrix.
;;Why not not ,@, not need, I just need the list to be present in the let init stage. 
;;We need this because it can be used to handle following cases
;;(with-array ((x 2 4) (w 3 5)) arr (format t "x:~a w:~a" x w))
;;ya, Let get it done. 
(defmacro with-array (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
      (let ,(mapcar 
		 #'(lambda (pat)
		     `(,(car pat) (aref ,gar ,@(cdr pat))))
		 pats)

      ,@body))))

;;Following destruct are dealing with structure. 
;;What do we mean by structure?
;;I really don't get the meaning of this.
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))

(defmacro with-struct-ps ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (symbol-macrolet ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))

;;It is great, I finally understood what wplac is all about.
(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      ;;why do we have symbol-macrolet here?
      ;;For what purpose?
      ;;Mean do not use the evaluated value, but use the 
      `(symbol-macrolet ,(mapcar #'(lambda (b)
				     (if (consp (car b))
					 (car b)
					 b))
				 binds)
      ,(wplac-ex (mapcan #'(lambda (b) 
			     (if (consp (car b))
				 (cdr b)))
			 binds)
		 body)))
)

;;How could we do to enable the destruct to support generic varibles?
;;Do the heavey lift for your brain, in the later times you can lift heavier intellectual weight than now. 
;;Go ahead. 
(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

;;After so many preparation we come to match. 
(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defmacro acond2 (&rest body)
  (let ((bsym (gensym)))
  `(block
      ,bsym
     ,@(mapcar #'(lambda (elm)
		   (let ((later (gensym)))
		   `(progn (multiple-value-bind (it ,later) ,(car elm)
		      (if (or ,later it)
			  (return-from ,bsym (progn ,@(cdr elm)))
		     )))))
	       body)))
)
;;Time to test the if-match functionality.
;;It takes so long a time to get here. Let's do it.
;;This is strange,
;;But one line of true come out mean you can traverse the code to 
(defmacro if-match-old (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
	 (let ,(mapcar #'(lambda (v)
			   `(,v (binding ',v it)))
		       ;;What's the purpose of this?
		       ;;Then just a bunch of expression
		       (vars-in then #'atom))
	   ,then)
	 ,else))

;;Let's test our aif2 method.
(defmacro aif2 (test then &optional else)
  (let ((result (gensym)))
  `(multiple-value-bind (it ,result) ,test
     (if (or ,result it)
	 ,then
	 ,else))))

;;The #\ just like '' in java
(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

;;assoc to find the proper on out.
;;why recurse into it?
;;I can use a case to illustrate this
;;(x y) (y z) (z g)
;;if you pass x, it will return g z.
(defun binding (x binds)
  (labels ((recbind (x binds)
	     (aif (assoc x binds)
		  ;;Following is very strange.
		  (or (recbind (cdr it) binds)
		      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
;;What's the puspose of this function?
;;Test it and found it out
;;Union mean combine together?
(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))



;;new match version, shift more work load to compile time.
;;Why? compile once and run many times. 
;;The purpose of this function is to move as much job that can be done at compile time to compile time.
(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in pat #'simple?))
     ;;Why we need to assign each variable in pattern to a gensym?
     ;;What's the orginal purpose of gensym?
     ;;It is to pervent name capture. seem in this circumstance no name capture need to worry.
     ;;Why?
     (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      ;;what's the purpose of this call? to add pat and seq into a list of pair
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
	;;Why do this?
	`(labels ((,gelse () ,else))
	   ;;What's the purpose of useing gseq, is not assign value yet.
	   ;;Why not treat then and else the same way?
	   ,(gen-match (cons (list gseq seq)
			     ;;gseq is no value yet,
			     ;;How can destruct get the right value?
			     ;;Only make sense that, this will not evaluate.
			     (destruc pat gseq #'simple?))
		       then
		       ;;By doing this way we could defer the execution.
		       ;;But why we need to defer the execution?
		       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

;;Don't get it.
;;Take some time to understand and better to visualize it.
;;Since it is called in the defmacro, so that it suppose to generate syntax instead of evaluate them.
(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
	(if (simple? (caar refs))
	    (match1 refs then else)
	    (gen-match (cdr refs) then else)))))

;;This will be called when the refs first element is simple type.
(defun match1 (refs then else)
  ;;dbind, old friend come out now. 
  (dbind ((pat expr) . rest) refs
	 (cond ((gensym? pat)
		`(let ((,pat ,expr))
		   (if (and (typep ,pat 'sequence)
			    ,(length-test pat rest))
		       ,then
		       ,else)))
	       ((eq pat '_) then)
	       ((var? pat)
		(let ((ge (gensym)))
		  `(let ((,ge ,expr))
		     (if (or (gensym? ,pat) (equal ,pat ,ge))
			 (let ((,pat ,ge)) ,then)
			 ,else))))
	       (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

;;Why do we need the length test?
;;What's purpose?
;;I need a juicy example. 
(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
	`(= (length ,pat) ,(length rest))
	`(> (length ,pat) ,(- (length rest) 2)))))


;;Following is the compiler version of with-answer.
(defmacro with-answer-c (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cdr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-and (clauses body)
  (if (null clauses)
      body
      (compile-query (car clauses)
		     (compile-and (cdr clauses) body))))
(defun compile-or (clauses body)
  (if (null clauses)
      nil
      (let ((gbod (gensym))
	    (vars (vars-in body #'simple?)))
	`(labels ((,gbod ,vars ,body))
	   ,@(mapcar #'(lambda (cl) (compile-query cl `(,gbod ,@vars)))
		     clauses)))))

(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
	   ,(compile-query q `(return-from ,tag nil))
	   t)
	 ,body)))



