(define get-operator (lambda (op env)
   (cond
     ( (eq? op '+) +) 
     ( (eq? op '*) *) 
     ( (eq? op '/) /) 
     ( (eq? op '-) -) 
     ( else (get-value op env) )
   )))

(define get-value (lambda (var env)
  (cond
     ((null? env)         (error "s7-interpret: undefined variable -->" var))
     ((eq? (caar env) var)(cdar env))
     (else                (get-value var (cdr env)))
  )))

(define extend-env (lambda (var val old-env)
       (cons (cons var val) old-env)))

(define define-stmt? (lambda (e)
    (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)) )))
	
(define if-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'if) (= (length e) 4))))
  
(define let-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'let) (= (length e) 3))))

(define s7-interpret (lambda (e env)
      (cond
        ( (number? e) e)
		( (symbol? e)      (get-value e env))
        ( (not (list? e))  (error "s7-interpret: cannot evaluate -->" e))
		( (if-stmt? e) 	   (if (eq? (s7-interpret (car (cdr e)) env) 0) 
							( s7-interpret (car (cdr (cdr (cdr e)))) env) 
							( s7-interpret (car (cdr (cdr e))) env)))
		
		((let-stmt? e)
			(let ((names (map car  (car (cdr e))))
				  (inits (map cadr (car (cdr e)))))
				(let ((vals (map (lambda (init) (s7-interpret init env)) inits)))
					(let ((new-env (append (map cons names vals) env)))
						(s7-interpret (car (cdr (cdr e))) new-env)))))
		
        ( else             
           (let (
                 (operator (get-operator (car e)))
                 (operands (map s7-interpret (cdr e)))
                )
                (apply operator operands)
            )))))
			
			
			

(define repl (lambda (env)
    (let* (
            (dummy1 (display "cs305> "))
            (expr (read))
			(new-env (if (define-stmt? expr)
						 (extend-env (car (cdr expr)) (s7-interpret (car (cdr (cdr expr))) env) env) env))
            (val (if (define-stmt? expr)
                     (cadr expr)(s7-interpret expr env)))
            (dummy2 (display "cs305: "))
            (dummy3 (display val))
            (dummy4 (newline))
            (dummy5 (newline)))
          
          (repl new-env))))

(define cs305 (lambda () (repl '())))
