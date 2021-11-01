;;;;
;;;; Translations of code from Statistical Rethinking, 2nd ed. from R into
;;;; Common Lisp, as well as implementations of necessary functions in CL
;;;; Isaac Stead, October 2021
;;;;

(defpackage :rethinking
  (:use :cl))

(in-package :rethinking)

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

;;; [I]n this chapter we exploit [the frequency format, or natural
;;; frequencies] by taking the probability distributions from the
;;; previous chapter and sampling from them to produce counts. The
;;; posterior distribution is a probability distribution.  And like
;;; all probability distributions, we can imagine drawing samples from
;;; it. The sampled events in this case are parameter values. Most
;;; parameters have no exact empirical realization. The Bayesian
;;; formalism treats parameter distributions as relative plausibility,
;;; not as any physical random process. In any event, randomness is
;;; always a property of information, never of the real world. But
;;; inside the computer, parameters are just as empirical as the
;;; outcome of a coin flip or a die toss or an agricultural
;;; experiment. The posterior defines the expected frequency that
;;; different parameter values will appear, once we start plucking
;;; parameters out of it.

(defun sample (n seq)
  "Randomly select a value from SEQ"
  (loop for i below n
        collect (nth (random (length seq))
                     seq)))

(defun weighted-sample (seq weights)
  "Roulette wheel selection. Randomly select a value from SEQ,
with the chance of selection proportional to corresponding WEIGHTS
value. WEIGHTS should be floats which sum to 1, i.e. probabilities,
and be sorted from smallest to largest."
  (let ((r (random 1.0)))
    (loop for i below (length seq)
          for sum = (nth i weights) then (+ sum (nth i weights))
          when (<= r sum) return (nth i seq))))

(defun weighted-samples (seq weights n)
  "O(n²), look for a faster implementation.
I found one in TAOCP Vol. 2 but it's expressed in terms of registers
on an abstract CPU which is a bit beyond me lol"
  (loop for i below n collect (weighted-sample seq weights)))

(defun epanechnikov (u)
  "Parabolic kernel function.
I can see how the (>= 1 (abs u)) excludes points from beyond a certain
distance from U, but this seems to be inefficient because in the algo
below we still have to step through every point, even those which will
be 'ignored' by this function."
  (if (>= 1 (abs u))
      (* 0.75 (- 1 (* u u)))
      0))

(defun sum (seq) (reduce #'+ seq))

(defun kde (seq h)
  "Also O(n²), I'm sure there is a faster way, see above"
  (let ((n (length seq)))
    (mapcar (lambda (x) (list x ; So we know what point for what density
                         (/ (sum (mapcar (lambda (i) (epanechnikov (/ (- x i) h)))
                                         seq))
                            (* n h))))
            seq)))

(defun test-kde (bandwidth)
  (let* ((p-grid (spaced-seq 0 1 1000))
         (prior  (loop for i below 1000 collect i))
         (likeli (grid-likelihood 6 9 p-grid))
         (postr  (mapcar (lambda (x y) (* x y)) likeli prior))
         (psum   (sum postr))
         (posterior (mapcar (lambda (x) (/ x psum)) postr))
         (ksamples (kde (sort (weighted-samples p-grid posterior 10000)
                              #'<)
                        bandwidth)))
    (kai:line (loop for i in ksamples collect (first i))
              (loop for i in ksamples collect (second i)))
    (kai:show)))



;;; The "faithful" dataset from R
(defparameter faithful
  (sort '(79 54 74 62 85 55 88 85 51 85 54 84 78 47 83 52 62 84 52 79 51 47 78 69 74 83 55 76 78 79 73 77 66 80 74 52 48 80 59 90 80 58 84 58 73 83 64 53 82 59 75 90 54 80 54 83 71 64 77 81 59 84 48 82 60 92 78 78 65 73 82 56 79 71 62 76 60 78 76 83 75 82 70 65 73 88 76 80 48 86 60 90 50 78 63 72 84 75 51 82 62 88 49 83 81 47 84 52 86 81 75 59 89 79 59 81 50 85 59 87 53 69 77 56 88 81 45 82 55 90 45 83 56 89 46 82 51 86 53 79 81 60 82 77 76 59 80 49 96 53 77 77 65 81 71 70 81 93 53 89 45 86 58 78 66 76 63 88 52 93 49 57 77 68 81 81 73 50 85 74 55 77 83 83 51 78 84 46 83 55 81 57 76 84 77 81 87 77 51 78 60 82 91 53 78 46 77 84 49 83 71 80 49 75 64 76 53 94 55 76 50 82 54 75 78 79 78 78 70 79 70 54 86 50 90 54 54 77 79 64 75 47 86 63 85 82 57 82 67 74 54 83 73 73 88 80 71 83 56 79 78 84 58 83 43 60 75 81 46 90 46 74) #'<))
