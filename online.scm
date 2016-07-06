(require (lib "defmacro.ss") readline/readline)

;; utility macro
(define-macro (while test . body) 
  (let ((loop (gensym 'loop))) `(let ,loop () (when ,test ,@body (,loop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Setting up a BAT

(define-for-syntax bayes-vspecs '())        ; the Bayes net
(define-for-syntax l-exprs #f)              ; the likelihoods
(define-for-syntax ss-exprs (make-hasheq))  ; the ss-state axioms

(define-macro (define-fluent flu dist)
  (set! bayes-vspecs (cons (list flu dist) bayes-vspecs)))
(define-macro (define-l-exprs . pairs) 
  (set! l-exprs (apply hasheq (pp pairs))))
(define-macro (define-ss-exprs flu . pairs)
  (hash-set! ss-exprs flu (apply hasheq (pp pairs))))

;; convert list ... (act . args) expr ... to ... act (lambda args expr) ...
(define-for-syntax (pp pairs)
  (if (null? pairs) '()
	  (cons (caar pairs) 
			(cons (eval `(lambda ,(cdar pairs) ,(cadr pairs))) 
				  (pp (cddr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Some predefined prob distributions and their generators

;; uniform distribution of x with lo and high
(define (UNIFORM x low high)
  (if (and (> x low) (< x high)) (/ 1.0 (- high low)) 0.0))

;; normal distribution of x with mean mu and variance var
(define (GAUSSIAN x mu var)
  (/ (exp (- (/ (* (- x mu) (- x mu)) (* 2.0 var)))) (sqrt (* 2.0 var pi))))

;; discrete distribution with values vi from v1 p1 ... vn pn, where sum of pi=1
(define (DISCRETE x . args)
  (let loop ((args args))
    (if (null? args) 0.0
        (if (eq? (car args) x) (cadr args) (loop (cddr args))))))

;; binary distribution of x 
(define (BINARY x p) (DISCRETE x #t p #f (- 1 p)))

;; linear transform for generating uniform numbers
(define (UNIFORM-RAND low high) (+ low (* (- high low) (random))))

;; generating discrete values vi from v1 p1 ... vn pn, where sum of pi=1
(define (DISCRETE-RAND . args)
  (let ((r (random)))
    (let loop ((args args) (sum 0))
      (let ((next (+ sum (cadr args))))
        (if (<= r next) (car args) (loop (cddr args) next))))))

;; generating binary values with given prob for #t 
(define (BINARY-RAND p) (DISCRETE-RAND #t p #f (- 1 p)))

;; box muller transform for generating Gaussian numbers
(define cached-gaussian #f)
(define (GAUSSIAN-RAND mu sigma)
  (if cached-gaussian 
      (let ((x2 cached-gaussian)) 
        (set! cached-gaussian #f)
        (+ (* x2 sigma) mu))
      (let loop ()
        (let* ((z1 (- (* 2. (random)) 1.)) (z2 (- (* 2. (random)) 1.))
               (rsq (+ (* z1 z1) (* z2 z2))))
          (if (>= rsq 1.) (loop)
              (let* ((d (sqrt (/ (* -2. (log rsq)) rsq)))
                     (x1 (* z1 d)) (x2 (* z2 d)))
                (set! cached-gaussian x2)  ; save the x2 value
                (+ (* x1 sigma) mu)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Setting up multiple worlds
;;;    *worlds* : a vector for *num-worlds* worlds, where each world is 
;;;       a vector of K values, where K is the number of fluents + 1 (for lvar)

(define *worlds* #f)
(define *num-worlds* #f)
(define-for-syntax *fluents* '())
(define-for-syntax *num-fluents* 0)
(define-for-syntax *indices* (make-hasheq))
(define-for-syntax lvar (gensym 'like))

;; initialize *worlds* using bayes-vspecs to initialize each world
(define-macro (set-samples n)
  (set! *fluents* (map car (reverse bayes-vspecs)))
  (set! *num-fluents* (length *fluents*))
  (for ([i *num-fluents*]) (hash-set! *indices* (list-ref *fluents* i) i))
  `(begin 
	 (set! *worlds* (make-vector ,n))
	 (set! *num-worlds* ,n)
	 (for ([i *num-worlds*]) 
       (let* ,(reverse bayes-vspecs)  ; initial value of fluents
		 (vector-set! *worlds* i (vector ,@*fluents* 1.0))))))

;; evaluate fluent expression EXPR for world indexed by I
(define-macro (world-val expr i)
  (let ((w (gensym)))
	`(let ((,w (vector-ref *worlds* ,i))) ,(trans-expr expr w))))
			
;; change fluent FLU to fluent expression EXPR in world indexed by I
(define-macro (world-set! i flu expr)
  (let* ((w (gensym)) (tr (trans-expr expr w)))
	`(let ((,w (vector-ref *worlds* ,i))) 
	   (vector-set! ,w 
			,(if (eq? flu lvar) *num-fluents* (hash-ref *indices* flu))
			,tr))))
			
;; like world-set! but for multiple fluents changed in parallel
(define-macro (world-set!* i flus exprs)
  (let* ((w (gensym)) (vars (map (lambda (flu) (gensym 'new)) flus))
		 (trs (map (lambda (e) (trans-expr e w)) exprs)))
	`(let ((,w (vector-ref *worlds* ,i))) 
	   (let ,(map list vars trs)
		 ,@(map (lambda (flu var) 
				  `(vector-set! ,w ,(hash-ref *indices* flu) ,var))
				flus vars)))))

;; an expression that gives the value of EXPR in world W
(define-for-syntax (trans-expr expr w)
  (cond
   ((symbol? expr)
	(if (memq expr *fluents*) `(vector-ref ,w ,(hash-ref *indices* expr))
		(if (eq? expr lvar) `(vector-ref ,w ,*num-fluents*) expr)))
   ((pair? expr) (cons (trans-expr (car expr) w) (trans-expr (cdr expr) w)))
   (else expr)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Expressions that cross all worlds

;; the expected value of EXPR
(define-macro (expected-val expr)
  (let ((acc (gensym)) (norm (gensym)) (i (gensym)))
	`(let ((,acc 0.0) (,norm 0.0))
	   (for ([,i *num-worlds*]) 
		  (set! ,acc (+ ,acc (world-val (* ,lvar ,expr) ,i)))
		  (set! ,norm (+ ,norm (world-val ,lvar ,i))))
	   (/ ,acc ,norm))))

;; the degree of belief in EXPR
(define-macro (eval-bel expr) `(expected-val (if ,expr 1.0 0.0)))

;; the degree of belief that EXPR is within RANGE fraction of expected value
(define-macro (conf expr range)
  (let ((e (gensym 'exp)) (r (gensym 'range)))
	`(let ((,e (expected-val ,expr)) (,r ,range))
	   (if (= ,e 0) (eval-bel (< (abs ,expr) ,r))
		   (eval-bel (< (abs (- ,expr ,e)) ,r)))))) ;; changing hector's code to make it absolute 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Interaction with the outside world: internal or tcp

;; online interaction with the outside world via read and print
(define (receive-sensor-internal act) 
  (string->number (readline "Enter sensed value: ")))
(define (send-action-internal act) 
  (printf "Execute action: ~s\n" act))

(define receive-sensor receive-sensor-internal)
(define send-action send-action-internal)

(define-macro (internal-online-do prog)
  `(begin (set! send-action send-action-internal)
		  (set! receive-sensor receive-sensor-internal)
		  (online-do ,prog)))

(define-macro (tcp-online-do prog)
  (let ((iport (gensym)) (oport (gensym)))
	`(let-values ([(,iport ,oport) (tcp-connect "localhost" 8123)])
	   (displayln "Connecting to action server on TCP port 8123")
	   (file-stream-buffer-mode ,iport 'none)         
	   (file-stream-buffer-mode ,oport 'none)  
	   (set! send-action (lambda (act) (displayln act ,oport)))
	   (set! receive-sensor (lambda (act) (read ,iport)))
	   (online-do ,prog)
	   (displayln "Disconnecting from action server")
	   (close-input-port ,iport)
	   (close-output-port ,oport))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Program execution

;; the main routine for programs like 
;; (online-do (begin (sonar) (when (> (eval-bel (< h 7)) .9) (fwd 1))))
(define-macro (online-do prog) (compile-prog prog))

;; useful for debugging
(define-macro (expand-do prog)  
  `(syntax->datum (expand-once '(online-do ,prog))))

;; generate code for a program depending on its type: while, if etc. 
(define-for-syntax (compile-prog prog)
  (cond 
   ((eq? (car prog) 'begin) 
	(cons (car prog) (map compile-prog (cdr prog))))
   ((memq (car prog) '(if when unless while))
  	  (cons (car prog) (cons (cadr prog) (map compile-prog (cddr prog)))))
   ((memq (car prog) '(let let*)) 
	(if (symbol? (cadr prog))
		`(,(car prog) ,(cadr prog) ,(caddr prog)
		      ,@(map compile-prog (cdddr prog)))
		`(,(car prog) ,(cadr prog) ,@(map compile-prog (cddr prog)))))
   (else (let ((comp (compile-action prog)))
		   (if (eq? comp prog) comp
			   `(begin (send-action (list ',(car prog) ,@(cdr prog)))
					   ,comp))))))

;; generate code for one action: either change likelihood or change fluents 
(define-for-syntax (compile-action act)
  (let ((likefn (hash-ref l-exprs (car act) #f)))
	(if likefn (compile-sensing likefn act)
		(compile-physical act))))

;; the code for sensing:  update lvar
(define-for-syntax (compile-sensing likefn act)
  (let ((input (gensym 'sense)) (i (gensym 'i)))
	`(let ((,input (receive-sensor (list ',(car act) ,@(cdr act)))))
	   (for ([,i *num-worlds*])
		 (world-set! ,i ,lvar (* ,lvar ,(likefn input)))))))
	   
;; the code for a physical act: update one or more fluents
(define-for-syntax (compile-physical act)
  (define flus   ; fluents affected by act
	(filter (lambda (flu) (change-fn act flu)) *fluents*))
  (define vals   ; new value for affected fluents
	(map (lambda (flu) (apply (change-fn act flu) (cdr act))) flus))
  (if (null? flus) act
	(let ((i (gensym 'i)))
	  (if (null? (cdr flus))
		  `(for ([,i *num-worlds*]) (world-set! ,i ,(car flus) ,(car vals)))
		  `(for ([,i *num-worlds*]) (world-set!* ,i ,flus ,vals))))))

;; a function that modifies a fluent for an action (or #f if unchanged)
(define-for-syntax (change-fn act flu)
  (let ((ss (hash-ref ss-exprs flu #f))) (and ss (hash-ref ss (car act) #f))))

