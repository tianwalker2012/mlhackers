;;Code for scheme.
;;Why PG use scheme as example to demo this?
;;Didn't CL have support for continuation?
;;Anyway, PG will reveal it anyway.
(define (dft tree)
    (cond ((null? tree) ())
	  ((not (pair? tree)) (write tree))
	  (else (dft (car tree))
		(dft (cdr tree)))))

(define *saved* ())

;;I wish I could run this. 
;;Let's download lisp and enjoy the continuation.
;;This is typical cases to combine closure with continuation.
(define (dft-node tree)
    (cond ((null? tree) (restart))
	  ((not (pair? tree)) tree)
	  (else (call-with-current-continuation
		 (lambda (cc)
		   (set! *saved*
			 (cons (lambda ()
				 (cc (dft-node (cdr tree))))
			       *saved*))
		   (dft-node (car tree)))))))

(define (restart)
    (if (null? *saved*)
	'done
	(let ((cont (car *saved*)))
	  (set! *saved* (cdr *saved*))
	  (cont))))

;;One issue I could think of it is that a global variable introduced to faciliate the continuation, which may cause problems. 
(define (dft2 tree)
    (set! *saved* ())
  (let ((node (dft-node tree)))
    (cond ((eq? node 'done) ())
	  (else (write node)
		(restart)))))
;;I could use the multiple return value advantage to let the dft-node return 2 values, for the cc stack. 
;;Scheme serve as a requirement document. 
;;This is an idea state we wish our CL continuation to have. 
;;Upon it we could build fancy staff 
;;Following is the scheme implementation for the choose.
;;PG called indeterministic programming.
(define *paths* ())
(define failsym '@)

(define (choose choices)
    (if (null? choices)
	(fail)
	(call-with-current-continuation
	 (lambda (cc)
	   (set! *path*
		 (cons (lambda ()
			 (cc (choose (cdr choices))))
		       *paths*))
	   (car choices)))))

(define fail)

;;Can I describe the logic in simple word
;;Check if the path have some to take, if not,
;;What the meaning of (cc failsym)?
;;It suppose to return the failure to the top level, right?
(call-with-current-continuation
     (lambda (cc)
       (set! fail 
	     (lambda ()
	       (if (null? *paths*)
		   (cc failsym)
		   (let ((p1 (car *paths*)))
		     (set! *paths* (cdr *paths*))
		     (p1)))))))


;;Following is the code to prevent the circulation reference in data to affect us to get the result.

(define *paths* ())
(define failsym '@)

;;It not distribute all the choice into the paths
(define (true-choose choices)
    (call-with-current-continuation
     (lambda (cc)
       (set! *paths* (append *paths*
			     (map (lambda (choice)
				    (lambda () (cc choice)))
				  choices)))
       (fail))))

(define fail)

;;Nothing special have done in the fail to deal with the circuler reference.
(call-with-curren-continuation
 (lambda (cc)
   (set! fail
	 (lambda ()
	   (if (null? *paths*)
	       (cc failsym)
	       (let ((p1 (car *paths*)))
		 (set! *paths* (cdr *paths*))
		 (p1)))))))






