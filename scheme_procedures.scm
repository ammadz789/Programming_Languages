(define symbol-length
	(lambda (inSym)
		(if (symbol? inSym)
			(string-length (symbol->string inSym))
			0
		)
	)
)





(define sequence?
	(lambda (inSeq)
		(if (list? inSeq)
			(if (null? inSeq)
				#t
				(if (and (symbol? (car inSeq)) (eq? 1 (symbol-length (car inSeq)))) 				
					(sequence? (cdr inSeq))
					#f
				)
				
			)			
			#f
		)
	
	)
)


	

(define same-sequence?
	(lambda (inSeq1 inSeq2)
		(if (sequence? inSeq1)
			(if (sequence? inSeq2)
				(if (and (null? inSeq1) (null? inSeq2))
					(if (and (null? inSeq1) (null? inSeq2))					
						#t
						#f
					)				
					(if (eq? (car inSeq1) (car inSeq2))
						(same-sequence? (cdr inSeq1) (cdr inSeq2))
						#f
					)
				)
			
				(error "ERROR305: seq2 not a sequence")
			
			)
			
			(error "ERROR305: seq1 not a sequence")
		
		)
	)
)



;(define (reverse1 l)
;	(append (reverse1 (cdr l)) (list (car l)))  
;)


(define reverse-sequence
	(lambda (inSeq)
		(if (sequence? inSeq)
			(if (null? inSeq)
				()
				(append (reverse-sequence (cdr inSeq)) (list (car inSeq)))
			)
			(error "ERROR305: not a sequence")
		)
	)
)

		
(define palindrome?
	(lambda (inSeq)
		(if (sequence? inSeq)
			
			(if (same-sequence? (reverse-sequence inSeq) inSeq)
				#t
				#f
			)			
			(error "ERROR305: palindrome")						
		)
	)
)		

(define member?
	(lambda (inSym inSeq)
		(if (sequence? inSeq)
			(if (symbol? inSym)
				(if (null? inSeq)
					#f
					(if (eq? (car inSeq) inSym)
						#t
						(member? inSym (cdr inSeq))
					)
				)
				(error "ERROR305: not a symbol")			
			)
			(error "ERROR305: not a sequence")
		)
	)
)


(define remove-member
	(lambda (inSym inSeq)
		(if (and (sequence? inSeq) (symbol? inSym))
			(if (member? inSym inSeq)
				(cond 
					((equal? inSym (car inSeq)) (cdr inSeq))
					(else
						(cons (car inSeq) (remove-member inSym (cdr inSeq)))
					)
				)
					
				(error "ERROR305: remove-member")
			)
			(error "ERROR305: not symbol/sequence")
		)

	)
)

(define anagram?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (and (null? inSeq1) (null? inSeq2))
				#t
				(if (member? (car inSeq1) inSeq2)
					(anagram? (cdr inSeq1) (remove-member (car inSeq1) inSeq2))
					#f				
				)
			)
			(error "ERROR305: not a sequence")			
		)
	)
)


(define anapoli?
	(lambda (inSeq1 inseq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (and (palindrome? inSeq2) (anagram? inSeq1 inSeq2))
				#t
				#f
			)
			(error "ERROR305: not a sequence")
		)
	)
)




		

	