;;;;
;;;; Translations of code from Statistical Rethinking, 2nd ed. from R into
;;;; Common Lisp, as well as implementations of necessary functions in CL
;;;; Isaac Stead, October 2021
;;;;

(defun factorial (n)
  (reduce #'* (loop for i from 1 to n collect i)))

(defun binomial (x n p)
  (* (/ (factorial n)
        (* (factorial x) (factorial (- n x))))
     (expt p x)
     (expt (- 1 p) (- n x))))

(defun stirling (n)
  (* (expt n n)
     (exp (* -1 n))
     (sqrt (* 2 pi n))))

;;; "While most parameters are continuous, capable of taking on an
;;; infinite number of values, it turns out that we can achieve an
;;; excellent approximation of the continuous posterior distribution
;;; by considering only a finite grid of parameter values. At any
;;; particular value of a parameter, p′, it’s a simple matter to
;;; compute the posterior probability: just multiply the prior
;;; probability of p′ by the likelihood at p′ . Repeating this
;;; procedure for each value in the grid generates an approximate
;;; picture of the exact posterior distribution. This procedure is
;;; called grid approximation."

(defun spaced-seq (range-start range-end length)
  (let ((step (/ (- range-end range-start)
                 (1- length))))
    (loop for i below length
          collect (float (* i step)))))

(defun grid-likelihood (x n-trials posterior-grid)
  (mapcar (alexandria:curry #'binomial x n-trials)
          posterior-grid))

(defun grid-posterior (likelihood prior)
  (let* ((unstd-posterior (mapcar (lambda (l p) (* l p))
                                  likelihood
                                  prior))
         (unstd-sum (reduce #'+ unstd-posterior)))
    ;; Standardise the posterior so it sums to 1
    (mapcar (lambda (p) (/ p unstd-sum))
            unstd-posterior)))

;;; Globe tossing example, translated from R. Produces identical result:
(defun globe-toss (n-points)
  (let ((posterior-grid (spaced-seq 0 1 n-points))
        (flat-prior     (loop for i below n-points collect 1)))
    (grid-posterior (grid-likelihood 6 9 posterior-grid)
                    flat-prior)))

