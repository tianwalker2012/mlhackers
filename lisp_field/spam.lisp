;;What mean to say in this code?
;;inner product of garbage mail and inner product of none garbage mail
(let ((prod (apply #'* probs)))
  (/ prod (+ prod (apply #'* (mapcar #'(lambda (x) (- 1 x)) probs)))))

(let ((g (* 2 (or (gethash word good) 0)))
      (b (or (gethash word good) 0)))
  (unless (< (+ g b) 5)
    (max .01
	 (min .99 (float (/ (min 1 (/b nbad))
			    (+ (min 1 (/ g ngood))
			       (min 1 (/ b nbad)))))))))