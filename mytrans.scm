;       Ruben Navarro
;       date: 04/16/2019
;	micro-pl to scheme converter

(define test
	(lambda (filename)
		(let ((s-expr (convert filename)))
			(display s-expr)
			(newline)
			(eval s-expr)
			(newline)
		)
	)
)

(define file2list
	(lambda (filename)
		(begin	(set-current-input-port! (open-input-file filename))
			(read-list))))


(define read-list
	(lambda ()
		(let ((token (read)))
			(cond	((eof-object? token) '())
				(else (cons token (read-list)))))))

(define convert
	(lambda (filename)
		(begin	(set-current-input-port! (open-input-file filename))
			(statement (read)))))

(define statements
	(lambda ()
		(let (	(token (read)))
		(cond	((eof-object? token) '())
			((eq? token 'end) '())
			(else	(let* (	(s (statement token))
					(r (statements)))
				(cons s r)))))))

(define statement
	(lambda (token)
		(cond	((eq? token 'declare) (declare_statement))
			((eq? token 'begin) (begin_statement))
			((eq? token 'for) (for_statement))
			((eq? token 'if) (if_statement))
			((eq? token 'set) (set_statement))
			(else (expression token)))))



;; student template for begin_statement
(define begin_statement                   ; begin scheme block
	(lambda ()
		(let* ((s (statements)) )
		       (cons 'begin s)))) 
		   		    

;; student template for declare_statement
(define declare_statement                 ; declare statment scheme block
	(lambda ()
		(let* ((d (declarations))
                        (s (statements)))
                       (cons 'let* (cons d s )))))

;; student template for for_statement
(define for_statement                     ; for statement scheme block
  (lambda ()
    (let* ( (token(read)) (t token) (e1 (statement (read-after '=))) (e2 (statement (read-after 'to)))  )
      (list 'do
            (list (list t e1 (list '+ t 1)))
            (list (list '> t e2 ) t )
            (statement (read))))))

;; student template for if_statement
(define if_statement                      ; if statement scheme block
    (lambda ()
        (let* ( (token(read) ) (e (expression token)) (s1 (statement(read-after 'then)) )
		(s2 (statement(read-after 'else))))
	  (list 'if e s1 s2 ))))

(define declarations
	(lambda ()
		(let (	(token (read)))
		(cond	((eof-object? token) '(eof))
			((eq? token 'begin) '())
			(else	(let* (	(d (declaration token))
					(r (declarations)))
				(cons d r)))))))

(define declaration
	(lambda (token)
		(list token (expression (read-after '=)))))

(define read-after
	(lambda (token)
		(if (eq? (read) token)
			(read)
			(display "Syntax error"))))

;; student template for expression
(define expression                        ; expression scheme block
        (lambda (token)
            (cond ((eq? token 'call) (call_expression))
		  ( else token ))))
		  
	       
;; additional template for expression2
(define call_expression                       ; expression 2 scheme block
  (lambda ()
    (let* ( ( token (read) ) (s (statement token)) )
      (cond 
       ((eq? token '$) '())
       (else (cons s (call_expression) ))))))
       
;; student template for expression
(define set_statement                     ; set statement scheme block 
	(lambda ()
            (let((token(read)))
	      (list 'set! token(expression(read-after'=))))))
