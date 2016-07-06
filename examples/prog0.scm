(include "../online.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   A BAT with one fluent, two physical actions, and one sensing action

(define-fluent h (UNIFORM-RAND 2. 12.))

;; a Bayes network can be constructed using define-fluent, e.g., (define-fluent g (UNIFORM-RAND h 10))


;; for each physical action, an expression for the next value of h
(define-ss-exprs h
   (fwd z) `(max 0 (- h ,z))                                     ; exact move
   (nfwd z) `(max 0 (- h (GAUSSIAN-RAND ,z 4))) ; noisy move
)

;; for each sensing action, an expression for its likelihood (default = 1)
(define-l-exprs
   (sonar z) `(GAUSSIAN ,z h 1)     ; true value h + noise
)

(set-samples 100000)

(define (test1)
  (online-do 
     (let loop ()
	   (while (< (conf h 1) .6) (sonar))
	   (let ((diff (- (expected-val h) 5)))
		 (when (> (abs diff) 1)
			(nfwd (abs diff))
			(loop))))))

(define (test2)
  (expand-do 
     (let loop ()
	   (while (< (conf h 1) .6) (sonar))
	   (let ((diff (- (expected-val h) 5)))
		 (when (> (abs diff) .02)
			(nfwd diff)
			(loop))))))

(define (test3)
  (online-do
	(while (< (expected-val h) 15)
			(nfwd 5) (sonar) (printf "not yet"))
))

		
(define (test4)
  (tcp-online-do 
     (let loop ()
	   (while (< (conf h 1) .8) (sonar))
	   (let ((diff (- (expected-val h) 5)))
		 (when (> (abs diff) 1)
			(nfwd (abs diff))
			(loop))))))		
