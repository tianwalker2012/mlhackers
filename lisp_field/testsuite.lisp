(defun my-fun (x y)
  (+ x y)
  )
(my-fun 10 20)
(defun our-assoc (key list)
  (and (consp list)
       (let ((x (car list)))
	 (if (eql key (car x))
	     x
	     (our-assoc key (cdr list))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n) (cons n path))
	  (cdr (assoc node net))))

(defun our-union (lstx lsty)
  (defun our-union-in (result lstone lsttwo)
    (if (consp lstone)
	
    )
  (our-union-in nil lstx lsty)
)

(defun add-unique (lst elm)
  (defun add-unique-in (org, cur, elm)
    (if (consp cur)
	(if (equal (car cur) elm)
	    org
	    (add-unique-in org (cdr cur) elm)
	    )
	(if (equal cur elm)
	    org
	    (cons org elm)
	    )
    )
    )
  (add-unique-in (lst lst elm))
)

(defun mypush (elm lst) 
	(setf lst (cons elm lst)))


(defun testload (x)
  (+ 10 x)
)
