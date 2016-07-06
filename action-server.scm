;;; This program is an action server that uses TCP port 8123
;;; Usage: racket -fm action-server.scm 

;;; The function (handle-action act oport) below is what decides what to do 
;;; with an action, where sensing results should be returned over oport 

(define (main . strs)
  (let ((listen (tcp-listen 8123 1 #t "localhost")))
	(displayln "Waiting for localhost TCP connection on port 8123")
	(let-values ([(iport oport) (tcp-accept listen)])
	  (file-stream-buffer-mode iport 'none)             
	  (file-stream-buffer-mode oport 'none)             
	  (displayln "Client has connected")
	  (let loop ((a (read iport)))
		(unless (eof-object? a)
           (printf "Action received: ~a\n" a)
		   (handle-action a oport)
		   (loop (read iport))))
	  (displayln "Client has disconnected"))))

(define (handle-action act oport) (simulated-action act oport))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  A simulated world.  One fluent h and three actions: fwd, nfwd, sonar 

(define true-h 7.0)

(define (simulated-action act oport)
  (sleep 1)             ; sleep for one second
  (case (car act)
	((sonar)            ; request for a sensing value
	 (let ((sense (GAUSSIAN-RAND true-h (max .01 (/ true-h 100)))))
	   (printf "Returning sensor value of ~a\n" sense)
	   (displayln sense oport)))
	((fwd nfwd)         ; request for change to h
	 (let ((arg (if (eq? (car act) 'fwd) (cadr act) 
					(GAUSSIAN-RAND (cadr act) (max .1 (/ (cadr act) 10))))))
	   (set! true-h (max 0 (- true-h arg)))
	   (printf "New value of h is ~a\n" true-h)))
	(else (error "Unrecognized act" act))))
	
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

