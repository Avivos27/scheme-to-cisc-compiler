
(load "pattern-matcher.scm")
(load "pc.scm") 

;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################
;                     ;SCANNER
;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################
;;####################################################################

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr2>))
       (*caten 2)
       done))


(define <infixexpression-comment>
 (new  (*parser (word "#;"))
       (*delayed (lambda () <infixexpression>))
       (*caten 2)

       done))

(define <comment>
  (disj <line-comment>
  		<infixexpression-comment>
		<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))


;##################### BOOLEAN ########################


(define <boolean>
  (new (*parser (word-ci "#t"))
       (*pack (lambda (_) #t))

       (*parser (word-ci "#f"))
       (*pack (lambda (_) #f))

       (*disj 2)
       done))



;##################### NUMBER ########################


(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <nat>
  (new (*parser <digit-0-9>) *plus
       (*pack
			(lambda (s)
			  (string->number
			   (list->string
			    `(,@s)))))
    done))



(define <int>
  (new (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
			(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
			(lambda (-- n) (- n)))

       (*parser <nat>)

       (*disj 3)

       done))



(define <fraction>
  (new (*parser <int>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
			(lambda (num div den)
	 			 (/ num den)))
       done))



(define <num>
	(new (*parser <fraction>) 
		 (*parser <int>)
		 
		 (*disj 2)
		 ; (*parser (range #\a #\z))
		 ; (*parser (range #\A #\Z))
		 ; (*disj 2)
		 ; *not-followed-by 

	done))



; ##################### STRING ########################

(define <hexchar>
	(new (*parser (range #\0 #\9))
		 (*parser (range #\a #\f))
		 (*parser (range #\A #\F))
		 (*disj 3)
	done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <string-meta-char>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*disj 7)
       done))


(define <stringhexchar>
	(new (*parser (word-ci "\\x"))
		 (*parser <hexchar>) *star
		 (*guard (lambda (n) (< (string->number (list->string n) 16) 1114112)))

		 (*parser (char #\;))

		 (*caten 3)
		 (*pack-with
		 	(lambda (x digits semicolon)
		 		(integer->char 
		 			(string->number
		 				(list->string digits)
		 				16))))
		 
		 done))

(define <string-char>
  (new (*parser <string-meta-char>)

       (*parser <stringhexchar>)
       (*parser <any-char>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)

       *diff
       (*disj 3)

    done))




(define <string>
  (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
			(lambda (open-delim chars close-delim)
	 			(list->string chars)))

       done))



;##################### CHAR ########################

(define <charprefix> 
	(new
	(*parser (word "#\\"))
	;(*pack (lambda (_) '(#\\)))
	done))


(define <namedchar>
	(new 
            (*parser (^<meta-char> "newline" #\newline))
	      	(*parser (^<meta-char> "return" #\return))
	       	(*parser (^<meta-char> "tab" #\tab))
	       	(*parser (^<meta-char> "page" #\page))
	       	(*parser (^<meta-char> "nul" #\nul))
	       	(*parser (^<meta-char> "space" #\space))
	       	(*parser (^<meta-char> "lambda" (integer->char 955)))
	       	(*disj 7)
	    	(*parser (range #\a #\z))
			(*parser (range #\A #\Z))
		 	(*disj 2)
	       	*not-followed-by
	done))



(define <simplevisiblechar>
	(new
	(*parser (range #\! #\~))

	(*parser (range #\a #\z))
	(*parser (range #\A #\Z))
	(*parser (range #\0 #\9))
	(*disj 3)
	*not-followed-by
	
	done))




(define <hexunicodechar>
	(new (*parser (char-ci #\x))
		 (*parser <hexchar>) *plus
		 (*guard (lambda (n) (< (string->number (list->string n) 16) 1114112)))

		 (*caten 2)

		 (*pack-with
		 	(lambda (x digits)
		 		(integer->char 
		 			(string->number
		 				(list->string digits)
		 				16))))
	done))


(define <char>
	(new (*parser <charprefix>)

		 (*parser <namedchar>)
		 (*parser <hexunicodechar>)
		 (*parser <simplevisiblechar>)
		 (*disj 3)

		 (*caten 2)
		 (*pack-with
			(lambda (a s) s))

	done))


; ##################### SYMBOLS ########################

(define <symbolchar>
	(new
		(*parser (range #\0 #\9))
		(*parser (range #\a #\z))
		(*parser (range #\A #\Z))
		; (*pack
  ; 			(lambda(ch)
  ; 				(integer->char (+ (char->integer ch) 32))))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\<))
		(*parser (char #\>))
		(*parser (char #\?))
		(*parser (char #\/))

		(*disj 15)
	done))



(define <symbol>
	(new
		(*parser <symbolchar>) *plus
		(*pack 
			(lambda (sym) (string->symbol (string-downcase (list->string sym)))))
	done))	





;############################# LISTS ###################

(define <properlist>
	(new

		(*parser (char #\( ))
		(*delayed (lambda () <sexpr2>)) *star 
		(*parser (char #\) ))	

		(*caten 3)
        (*pack-with
			(lambda (open-delim chars close-delim)
	  				 chars))

	done))

(define <improperlist>
	(new

		(*parser (char #\( ))
		(*delayed (lambda () <sexpr2>)) *plus 
		(*parser (char #\.))
		(*delayed (lambda () <sexpr2>))        
		(*parser (char #\) ))	

		(*caten 5)
        (*pack-with
                (lambda (open-delim _car dot _cdr close-delim)
                                ;(cons (car _car) _cdr)))
                                `(,@_car . ,_cdr)))
                                

	done))


;######################## VECTOR ################
(define <vector>
	(new
		(*parser (char #\#))
		(*parser (char #\())
		(*delayed (lambda () <sexpr2>)) *star 
		(*parser (char #\)))
		(*caten 4)
		(*pack-with 
			(lambda (hashtag opencaption exp closecaption)
                            ;(vector (car exp))))
                            `#(,@exp)))
		done))


;######################## ⟨quote⟩ ################

(define <quoted>
	(new
		(*parser (char #\'))
		(*delayed (lambda () <sexpr2>)) 
		(*caten 2)
		(*pack-with
			(lambda (q exp)
				(list 'quote exp)))

		done))


(define <quasiquoted>
	(new
		(*parser (char #\`))
		(*delayed (lambda () <sexpr2>)) 
		(*caten 2)
		(*pack-with
			(lambda (q exp)
				(list 'quasiquote exp)))

		done))


(define <unquoted> 
	(new
		(*parser (char #\,))
		(*delayed (lambda () <sexpr2>)) 
		(*caten 2)
		(*pack-with
			(lambda (q exp)
				(list 'unquote exp)))

		done))

(define <unquoteandspliced>
	(^<skipped*>
	(new
		(*parser (char #\,))
		(*parser (char #\@))
		(*delayed (lambda () <sexpr2>)) 
		(*caten 3)
		(*pack-with
			(lambda (q shtrudel exp)
				(list 'unquote-splicing exp)))

	done)))


; ##################### INFIX ########################
(define <infixprefixextentionprefix>
	(^<skipped*>
	(new
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
		done)))


(define <infixsexprescape>
	(^<skipped*>
	(new
		(*parser <infixprefixextentionprefix>)
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (prefix exp) exp))

	done)))


(define <infixsymbol>
	
	(new
		(*parser <symbolchar>)
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser (char #\*))
		(*parser (word "**"))
		(*parser (char #\^))
		(*parser (char #\/))
		(*disj 6)
		*diff
		*plus
		(*pack
    		(lambda (s)
     			(string->symbol
     				(string-downcase
      			(list->string
       			`(,@s))))))
	done))


(define print 
     (lambda(exp rest) 
      (fold-left 
        (lambda (acc next_exp)
        	 `( ,(string->symbol(string (car next_exp))) ,acc ,@(cdr next_exp))) 
        exp 
        rest)))


 			

(define <atomic>
	(^<skipped*>
	(new 
		
		(*parser <num>)
		(*parser <infixsymbol>)
		(*parser (range #\0 #\9))
        *diff
        *not-followed-by
        (*parser <infixsexprescape>)
        (*parser <infixsymbol>)

		(*disj 3)
	done)))

(define <infixparen>
	(^<skipped*>
		(new
			(*parser (char #\( ))
			(*delayed (lambda () <infixexpression>))
			(*parser (char #\) ))
			(*caten 3)
			(*pack-with
				(lambda (open exp close)
					exp
					))
			(*parser <atomic>)
			(*disj 2)

		done)))



(define printarglist
	(lambda (first rest)
		`(,first ,@rest)))

(define <infixarglist>
	(^<skipped*>
		(new
			(*delayed (lambda () <infixexpression>))
			
			(*parser (char #\,))
			(*delayed (lambda () <infixexpression>))
			(*caten 2) 
			(*pack-with
				(lambda (comma exp) exp))
			*star

			(*caten 2)
			(*pack-with printarglist)
			(*parser <epsilon>)
			(*disj 2)
		done)))


(define printfuncall
	(lambda (funcname args)
		(fold-left 
			(lambda (acc next_step)
				`(,acc ,@next_step))
				funcname
				args
				)))


(define <infixfuncall>
	(^<skipped*>
		(new
			(*parser <infixparen>)
			(*parser (char #\( ))
			(*parser <infixarglist>)
			(*parser (char #\) ))
			(*caten 3)
			(*pack-with (lambda (open exp close) exp))
			*star
			(*caten 2)
			(*pack-with printfuncall)

		done)))



(define printarrget
	(lambda (array rest)
		(fold-left 
          (lambda(acc next_step) 
          `(vector-ref ,acc ,next_step )) 
          array 
          rest)))

(define <infixarrayget>
	(^<skipped*>
		(new

			(*parser <infixfuncall>)
			(*parser (char #\[))
			(*delayed (lambda () <infixexpression>))
			(*parser (char #\]))
			(*caten 3)
			(*pack-with 
				(lambda (openbracet exp closebracet) exp))
			*star
			(*caten 2)
			(*pack-with printarrget)

		done)))



(define <infixneg>
  (^<skipped*> (new
    (*parser <infixarrayget>)  
    (*parser (^<skipped*> (char #\-)))
    (*parser <infixarrayget>)    
    (*caten 2)
    (*pack-with
      (lambda(minus num) `(- ,num)))
    (*disj 2)

   done)))



(define getlast
	(lambda(lst)
		(list-ref 
          	lst
          	(- (length lst) 1))
		))
(define lsttail
	(lambda (lst)
	(reverse (cdr (reverse lst)))
	))


(define printpow 
     (lambda(e1 lst_of_expr) 
      (fold-right (lambda(acc next) `(expt ,acc ,next)) 
      			(getlast (cons e1 lst_of_expr))
				(lsttail (cons e1 lst_of_expr))
    
              )))
    
   

(define <infixpow>
	(^<skipped*> (new
		(*parser <infixneg>)
		(*parser (char #\^))
		(*parser (word "**"))
		(*disj 2)
		(*parser <infixneg>)
		(*caten 2) 
    	(*pack-with
    	  (lambda(sym num)
        			num))
		*star
		(*caten 2)
		(*pack-with printpow)
	done)))


(define <infixmuldiv>
	(^<skipped*>
	(new
		(*parser <infixpow>)
		(*parser (char #\*))
		(*parser (char #\/))
		(*disj 2)

		(*parser <infixpow>)
		(*caten 2) *star
		(*caten 2)
		(*pack-with print)
	done)))



(define <infixaddsub>
	(^<skipped*>
	(new
		(*parser <infixmuldiv>)
		(*parser (^<skipped*> (char #\+)))
		(*parser (^<skipped*> (char #\-))) 
		(*disj 2)
		(*parser (^<skipped*> <infixmuldiv>))
		(*caten 2) *star
		(*caten 2)
		(*pack-with print)
	done)))



(define <infixexpression>
	(^<skipped*>
	(new

		(*parser <infixaddsub>)

	done)))





(define <infixextension> 
	(^<skipped*>
	(new

		(*parser <infixprefixextentionprefix>)
		(*delayed (lambda () <infixexpression>))
		(*caten 2)
		(*pack-with (lambda (prefix exp) exp))

	done)))






;######################### S-EXPRESSION ######################################3

(define <sexpr2>
	(^<skipped*>
	(new
   		 (*parser <boolean>)
   		 (*parser <char>)
   		 (*parser <string>)
   		 (*parser <num>)
		 (*parser <symbol>)
		 (*parser (range #\0 #\9))
  		 *diff
  		 *not-followed-by
  		 (*parser <symbol>)
		 (*parser <properlist>)
		 (*parser <improperlist>)
		 (*parser <vector>)
		 (*parser <quoted>)
		 (*parser <quasiquoted>)
		 (*parser <unquoted>)
		 (*parser <unquoteandspliced>)
		 (*parser <infixextension>)
		 (*disj 13)
		 done)))
		 
(define <sexpr> <sexpr2>)
(define <Sexpr> <sexpr2>)

(define add-list
  (lambda (s)
    (fold-right
     (lambda (a b) (+ a  b))
     0
     s)))



;#############################################################################
;#############################################################################
;#############################################################################
;#############################################################################
                                   ;HW2
;#############################################################################
;#############################################################################
;#############################################################################
;#############################################################################


;######################## HELPERS #######################
(define delayed-run
	(delay parse))

(define let-get-args
	(lambda (list)
		(map car list)
	)
)
(define let-get-args-exp
	(lambda (list)
		(map cadr list)
	)
)

(define (empty? lst)
	(eq? lst '()))

(define let-no-duplicate?
	(lambda (let-exp)
		(letrec ((iter (lambda (lst) 
						(cond 
						((empty? lst) #t)
						;((not (list? let-exp)) #f)
						((not (not (member (car lst) (cdr lst)))) #f)
						(else (iter (cdr lst)))))))
		(iter (let-get-args let-exp)))))

(define lambda-no-duplicate?
	(lambda (let-exp)
		(letrec ((iter (lambda (lst) 
						(cond 
						((empty? lst) #t)
						;((not (list? let-exp)) #f)
						((not (not (member (car lst) (cdr lst)))) #f)
						(else (iter (cdr lst)))))))
		(iter let-exp))))

(define make-set
		(lambda (assoc-lst)
			(map 
				(lambda (x) `(set! ,(car x) ,(cadr x)))
				assoc-lst)
			))

(define void
	(if #f `LR))

(define fail (lambda () 
	"ERROR-fail"))

(define void?
	(lambda (x)
		(eq? void x)))

(define *reserved-words*
	'(and begin cond define do else if lambda
	let let* letrec or quasiquote unquote
	unquote-splicing quote set!))	

(define var?
 (lambda (x)
	 (and (symbol? x)
 	 (not (member x *reserved-words*)))))

(define special-form?
 (lambda (x)
 	 (not (member x *reserved-words*))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))	

(define remove-begins 
	(lambda (lst)
		(fold-right
			(lambda (x filtered-lst) (if (and (list? x) (not (empty? x))) 
										 (if (eq? (car x) 'begin) 
										 	 (append (remove-begins (cdr x)) filtered-lst )
										 	 (append (list x) filtered-lst ))
										(append (list x) filtered-lst )))
			'()
			lst
		)))

; ################## CONSTANTS ######################
(define void-rule
	(pattern-rule 
		`,(? `void void?)
		(lambda (void) `(const ,void))))

(define nil-rule
	(pattern-rule 
		``()
		(lambda () `(const ()))))


(define vectors-rule
	(pattern-rule 
		`,(? `vector vector?)
		(lambda (vect) `(const ,vect))))

(define boolean-rule
	(pattern-rule 
		`,(? `boolean boolean?)
		(lambda (bool) `(const ,bool))))

(define characters-rule
	(pattern-rule 
		`,(? `char char?)
		(lambda (char) `(const ,char))))

(define numbers-rule
	(pattern-rule 
		`,(? `num number?)
		(lambda (num) `(const ,num))))

(define strings-rule
	(pattern-rule 
		`,(? `string string?)
		(lambda (str) `(const ,str))))

;###################### QUOTES #######################

(define quote-rule
	(pattern-rule 
		`(quote ,(? `quote))
		(lambda (sym) `(const ,sym))))

;###################### QUOTES #######################

(define variables-rule
	(pattern-rule 
		`,(? `var var?)
		(lambda (var) `(var ,var))))


; ##################### CONDITIONALS ###################

(define if3-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(if ,(? 'test) ,(? 'then) ,(? 'else))
 			(lambda (test then else)
 				(let ((run (force run)))
 				`(if3 ,(run test) ,(run then) ,(run else)))))))


(define if2-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(if ,(? 'test) ,(? 'then))
 			(lambda (test then)
 				(let ((run (force run)))
 				`(if3 ,(run test) ,(run then) ,(run void)))))))


; #################### DISJUNCTIONS ######################

(define empty-or-rule
	(pattern-rule
 		`(or )
 			(lambda ()
 				`(const #f))))

(define or-one-arg-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(or ,(? 'first))
 			(lambda (first)
 				(let* ((run (force run)))
 							(run first))))))

(define or-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(or ,(? 'first) . ,(? 'rest))
 			(lambda (first rest)
 				(let* ((run (force run))
 				 	   (exp-lst (cons first rest))
 					   (parsed-exp (map run exp-lst)))

 							`(or ,parsed-exp))))))

; #################### Lambda forms #########################


(define empty-lambda-simple-rule
	(pattern-rule
 		`(lambda () ,(? `body) . ,(? `body-rest))
 			(lambda (body body-rest)
 				(let* ((run (force delayed-run))
 					   (body-list (cons body body-rest))
 					    (body-seq (run `(begin ,@body-list))))
 					`(lambda-simple () ,body-seq)
 				))))



(define lambda-simple-rule
	(pattern-rule
 		`(lambda (,(? 'first) . ,(? 'rest list?)) ,(? `body) . ,(? `body-rest))
 			(lambda (first rest body body-rest)
 				(let* ((run (force delayed-run))
 					  (arg-lst (cons first rest))
 					  (body-list (cons body body-rest))
 					  (body-seq (run `(begin ,@body-list))))
 					(if (lambda-no-duplicate? arg-lst)
 					`(lambda-simple (,@arg-lst) ,body-seq)
 					(fail)
 					
 				)))))

(define lambda-opt-rule
	(pattern-rule
 		`(lambda (,(? 'first) . ,(? 'rest) ) ,(? `body) . ,(? `body-rest))
 			(lambda (first rest body body-rest)
 				(let* ((run (force delayed-run))
 					  (arg-lst (cons first (flatten rest)))
 					  (last-arg (car (reverse arg-lst)))
 					  (args (list-head arg-lst (- (length arg-lst) 1)))
 					  (body-list (cons body body-rest))
 					  (body-seq (run `(begin ,@body-list))))

 					  `(lambda-opt (,@args) ,last-arg ,body-seq)
 				))))

(define lambda-var-rule
	(pattern-rule
 		`(lambda ,(? 'args)  ,(? `body) . ,(? `body-rest))
 			(lambda (args body body-rest)
 				(let* ((run (force delayed-run))
 					  (body-list (cons body body-rest))
 					  (body-seq (run `(begin ,@body-list))))
 					`(lambda-var ,args ,body-seq)
 				))))



;###################### BEGIN ########################

(define begin-empty-rule
	(pattern-rule
		`(begin)
			(lambda ()
				`(const ,void)
				)))


(define begin-one-arg-rule
	(pattern-rule
		`(begin ,(? 'arg))
			(lambda (arg)
				(let ((run (force delayed-run)))
					(run arg)
				))))

 	
(define begin-rule
	(pattern-rule
		`(begin ,(? 'first) . ,(? 'rest))
			(lambda (first rest) 
				(let* ((run (force delayed-run))
					  (arg-lst (cons first rest))
					  (filtered-args (remove-begins arg-lst))
					(parsed-args (map run filtered-args)))

						`(seq ,parsed-args)
				))))		
;###################### DEFINE #########################

(define define-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(define ,(? 'var var?) ,(? 'exp) . ,(? 'rest))
 			(lambda (var exp rest)
 				(let* ((run (force run)) 
 				      (definition (cons exp rest))
 				  	  (begin-definition `(begin ,@definition)))
 				`(def ,(run var) ,(run begin-definition)))))))


(define define-mit-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(define (,(? 'var var?) . ,(? 'arglist)) ,(? 'body) . ,(? 'body-rest))
 			(lambda (var args body body-rest)
 				(let* ((run (force run)) 
 					 (body-list (cons body body-rest))
 					(lambdaexp (run `(lambda (,@args) ,@body-list))))
 				`(def ,(run var) ,lambdaexp))))))


;###################### APPLICATION ##########################



(define application-rule
	(let ((run (delay parse)))
	(pattern-rule
		`( ,(? 'func special-form?) . ,(? 'paramaters))
			(lambda (var paramaters)
				(let* ((run (force run))
					  (func (run var))
					  (parsed-par (map run paramaters)))
					  `(applic ,func ,parsed-par))))))	

;###################### AND ################################

(define and-empty-rule
	(pattern-rule
		`(and)
			(lambda ()
				`(const #t))))


(define and-one-arg-rule
	(pattern-rule
		`(and ,(? `arg))
			(lambda (arg)
				(let ((run (force delayed-run)))
					(run arg)
				))))

(define and-rule
	(pattern-rule
		`(and ,(? 'first) . ,(? 'rest))
		(lambda (first rest)
			(let ((run (force delayed-run))
				  (and-rest `(and ,@rest)))
			(run `(if ,first ,and-rest #f))))))


;####################### SET!  #################################

(define set-rule
	(let ((run (delay parse)))
	(pattern-rule
 		`(set! ,(? 'var var?) ,(? 'exp))
 			(lambda (var exp)
 				(let ((run (force run)))
 				`(set ,(run var) ,(run exp) ))))))


;####################### LET  #################################

(define let-empty-rule
	(pattern-rule
		`(let () ,(? 'body) . ,(? 'rest))
			(lambda (body rest)
				(let ((run (force delayed-run))
					  (body-list (cons body rest)))
				(run `((lambda () ,@body-list)))))))

(define let-rule
	(pattern-rule
		`(let ,(? 'args let-no-duplicate?) ,(? 'body) . ,(? 'rest))
			(lambda (assoc body rest)
				(let ((run (force delayed-run))
					  (arg-list (let-get-args assoc))
					  (bindings (let-get-args-exp assoc))
					  (body-list (cons body rest)))
						(run `((lambda ,arg-list ,@body-list) ,@bindings))))


				))

(define let*-empty-rule
	(pattern-rule
		`(let* () ,(? 'body) . ,(? 'rest))
			(lambda (body rest)
				(let ((run (force delayed-run))
					  (body-list (cons body rest)))
				(run `((lambda () ,@body-list)))))))

(define let*-rule
	(pattern-rule
			`(let* (,(? 'first-arg) . ,(? 'rest-args)) ,(? 'body) . ,(? 'rest))
				(lambda (first-arg rest-args body rest)
					(let* ((run (force delayed-run))
							(arg (car first-arg))
							(binding (cadr first-arg))
							(body-list (cons body rest))
							(rest-let `(let* (,@rest-args) ,@body-list)))
							(if (empty? rest-args)
								(run `((lambda (,arg) ,@body-list) ,binding))

							(run `((lambda (,arg) ,rest-let) ,binding)))))


					))


(define letrec-empty-rule
	(pattern-rule
		`(letrec () ,(? 'body) . ,(? 'rest))
			(lambda (body rest)
				(let ((run (force delayed-run))
					  (body-list (cons body rest)))
				(run `((lambda () ((lambda () ,@body-list)))))))))



(define letrec-rule
	(pattern-rule
		`(letrec ,(? 'args let-no-duplicate?) ,(? 'body) . ,(? 'rest))
			(lambda (assoc body rest)
				(let* ((run (force delayed-run))
					  (arg-list (let-get-args assoc))
					  (bindings (let-get-args-exp assoc))
					  (place-holders (map (lambda (x) #f) bindings))
					  (set (make-set assoc))
					  (body-list  `( ((lambda () ,@(cons body rest)))))
					  (body-and-set (append set body-list)))
					  ;(display set)(newline)(newline)
					  ;(display body-and-set)(newline)(newline)
					   (run `((lambda ,arg-list ,@body-and-set) ,@place-holders))))


				))

;############################## COND #########################################
(define else?
	(lambda (exp)
		(eq? (car exp) 'else)
			))


;(define empty-cond-rule
;	(pattern-rule
;		`(cond)
;		(lambda ()  `(const ,void))))

(define else-cond-rule
	(pattern-rule
		`(cond ,(? 'exp else?) . ,(? 'rest))
		(lambda (exp rest)
			(let ((run (force delayed-run))
				  (expr (cdr exp)))
		  		(run `(begin ,@expr))))))

(define cond-rule
	(pattern-rule 
		`(cond ,(? 'first-clause) . ,(? 'rest-clause))
		(lambda (first-c rest-c)
			(let* ((run (force delayed-run))
				   (cond-rest `(cond ,@rest-c))
					(rest (if (empty? rest-c) void cond-rest )))
				(run `(if ,(car first-c) (begin ,@(cdr first-c)) ,rest))))

		))



;####################################### QUASIQUOTE ####################

;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016


;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
    	
      (optimize-qq-expansion
        (expand-qq e)))))


(define qq-rule
	(pattern-rule
		`(quasiquote ,(? 'sexpr) . ,(? 'rest))
		(lambda (sexpr rest) 
			(let ((run (force delayed-run)))
		(run (expand-qq sexpr))))

		))


;############################### PARSE ###############################


(define parse
 (let ((run
 	(compose-patterns
 			let-empty-rule
 			let-rule
 			let*-empty-rule
 			let*-rule
 			letrec-empty-rule
 			letrec-rule
 			and-empty-rule
	 		and-one-arg-rule
	 		and-rule
	 		;empty-cond-rule
	 		else-cond-rule
	 		cond-rule
	 		void-rule
	 		nil-rule
	 		vectors-rule
	 		boolean-rule
	 		characters-rule
	 		numbers-rule
	 		strings-rule
	 		quote-rule
	 		set-rule
	 		variables-rule
	 		if3-rule
	 		if2-rule
	 		empty-or-rule
	 		or-one-arg-rule
	 		or-rule
	 		empty-lambda-simple-rule
	 		lambda-simple-rule
	 		lambda-opt-rule
	 		lambda-var-rule
	 		begin-empty-rule
	 		begin-one-arg-rule
	 		begin-rule
	 		define-rule
	 		define-mit-rule
	 		application-rule
	 		qq-rule

	 		)))

 	(lambda (sexpr)
 		(run sexpr fail))))


;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
									;HW3
;##############################################################################
;##############################################################################
;##############################################################################


(define (def? parsed-exp)
		(eq? (car parsed-exp) `def))

(define (const? parsed-exp)
		(eq? (car parsed-exp) `const))

(define (or? parsed-exp)
		(eq? (car parsed-exp) `or))

(define (if? parsed-exp)
		(eq? (car parsed-exp) `if3))

(define (applic? parsed-exp)
		(eq? (car parsed-exp) `applic))

(define (tc-applic? parsed-exp)
		(eq? (car parsed-exp) `tc-applic))

(define (var? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `var)))




(define (tc-var? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(or (eq? (car parsed-exp) `var)
			(eq? (car parsed-exp) `fvar)
			(eq? (car parsed-exp) `bvar)
			(eq? (car parsed-exp) `pvar))))


(define (seq? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `seq)))

(define (box-set-set? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
			(eq? (car parsed-exp) `box-set)))

(define (box-get? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
			(eq? (car parsed-exp) `box-get)))


(define (set? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `set)))



(define (get-lambda-params parsed-exp)
	(let ((type (car parsed-exp)))
		(cond 
			((equal? type 'lambda-simple) (cadr parsed-exp))
			((equal? type 'lambda-opt) (append (cadr parsed-exp) (list (caddr parsed-exp))))
			((equal? type 'lambda-var) (list (cadr parsed-exp))))
	))

(define (get-lambda-body parsed-exp)
	(let ((type (car parsed-exp)))
		(cond 
			((equal? type 'lambda-simple) (caddr parsed-exp))
			((equal? type 'lambda-opt) (cadddr parsed-exp))
			((equal? type 'lambda-var) (caddr parsed-exp)))
	))

(define (get-original-params parsed-exp)
	(let ((type (car parsed-exp)))
		(cond 
			((equal? type 'lambda-simple) (list (cadr parsed-exp)))
			((equal? type 'lambda-opt) (list (cadr parsed-exp) (caddr parsed-exp)))
			((equal? type 'lambda-var) (list (cadr parsed-exp))))
	))

(define (last lst)
 (car (reverse lst)))

(define (all-but-last lst)
 (reverse (cdr (reverse lst))))

(define (add-to-end lst plumbus)
	(append lst (list plumbus)))

(define (add-def-to-list def-list def)
	(let ((var (cadadr def))
		  (body (caddr def))
		  (var-lst (if (empty? def-list) '() (car def-list)))
		  (body-lst (if (empty? def-list) '() (cadr def-list))))

	(list (add-to-end var-lst var) (add-to-end body-lst body)
)))

(define (list-of-nested-defines parsed-exp nested-list)
	(cond 
		((empty? parsed-exp) nested-list)
		((seq? parsed-exp) (list-of-nested-defines (cadr parsed-exp) nested-list))
		((def? (car parsed-exp)) (let ((updated-nested-list (add-def-to-list nested-list (car parsed-exp)))
								 	   (rest (cdr parsed-exp)))
									
									(list-of-nested-defines rest updated-nested-list)))
		
		(else nested-list)))

(define (get-body-nested-defines parsed-exp)
		(if (empty? parsed-exp)
			 parsed-exp
			(if (seq? parsed-exp) 
				(get-body-nested-defines (cadr parsed-exp))
				(if (not (def? (car parsed-exp)))
					 parsed-exp
					(get-body-nested-defines (cdr parsed-exp))))))

(define (lambda? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(let ((type (car parsed-exp)))
			(if (or (eq? type 'lambda-simple) (eq? type 'lambda-opt) (eq? type 'lambda-var))
				#t
				#f))))

(define (lambda-nested-defines? parsed-exp)
	(if (lambda? parsed-exp)
			(let* ((body (get-lambda-body parsed-exp))
				   (type (car body)))
				(if (or (eq? type 'def) (and (eq? type 'seq) (eq? (caaadr body) 'def)))
					#t
					#f))
		#f))

(define (build-sets def-lst)
	(let ((var-lst (car def-lst))
		  (definitions (cadr def-lst))
		  (delayed-run eliminate-nested-defines))
	(map (lambda (var def) 
			;(display `DEF:)(display def)(newline)
			`(set (var ,var) ,(delayed-run def))
			)
		var-lst definitions)
	)
)

(define (get-place-holders var-lst)
	(map (lambda (x) `(const #f)) var-lst))


(define (build-letrec lambda-type lambda-par body-of-lambda def-lst body-lst)

		`(,lambda-type ,@lambda-par 
			(applic 
				(lambda-simple ,(car def-lst)
					(seq (,@(build-sets def-lst) 
					,@body-lst)))
					,(get-place-holders (car def-lst))))

					)


(define (generic pred func)
		(lambda (parsed-exp)
			(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
				 parsed-exp
				(if (pred parsed-exp)
					(func parsed-exp)
					;(cons (car parsed-exp) (map (generic pred func) (cdr parsed-exp)))))))
				(map (generic pred func) parsed-exp)))))

(define (nested-logic parsed-exp)
	(let* ((lambda-type (car parsed-exp))
		  (body-of-lambda (eliminate-nested-defines (get-lambda-body parsed-exp)))
		  (def-lst (list-of-nested-defines body-of-lambda '()))
		  (body-lst (eliminate-nested-defines (get-body-nested-defines body-of-lambda)))
		  (org-lambda-params (get-original-params parsed-exp)))
		
		  (build-letrec lambda-type org-lambda-params body-of-lambda def-lst body-lst)))


(define eliminate-nested-defines
	 (generic lambda-nested-defines? nested-logic))


;(define eliminate-nested-defines
;	(lambda (parsed-exp)
;		(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
;			 parsed-exp
;			(if (and (lambda? parsed-exp) (lambda-nested-defines? (caddr parsed-exp)))
;				(let* ((lambda-type (car parsed-exp))
;					  (lambda-par (cadr parsed-exp))
;					  (body-of-lambda (caddr parsed-exp))
;					  (def-lst (list-of-nested-defines body-of-lambda '()))
;					  (body-lst (eliminate-nested-defines (get-body-nested-defines body-of-lambda))))
;						;(begin (display lambda-type) (newline) (display lambda-par) (newline) (display body-of-lambda) (newline) (display def-lst) (newline) (display body-lst) (newline) )
;					  (build-letrec lambda-type lambda-par body-of-lambda def-lst body-lst))

						
;				(cons (car parsed-exp) (map eliminate-nested-defines (cdr parsed-exp)))))))

(define (lambda-empty? parsed-exp)
	(and (eq? (car parsed-exp) 'applic)
	     (empty? (caddr parsed-exp))
	     (lambda? (cadr parsed-exp))
	     (empty? (get-lambda-params (cadr parsed-exp)))))

(define (lambda-nil-logic parsed-exp)
	(let ((body (get-lambda-body (cadr parsed-exp))))
			(remove-applic-lambda-nil body)))

(define remove-applic-lambda-nil 
	(generic lambda-empty? lambda-nil-logic))



(define (bound? parsed-exp par in-lambda?)
	(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(if (and in-lambda? (lambda? parsed-exp) (member par (get-lambda-params parsed-exp))) #f
			(if (and in-lambda? (equal? parsed-exp `(var ,par))) 
				#t
				(ormap (lambda (sub-exp)
						(if (lambda? sub-exp)

					 		(let ((params (get-lambda-params sub-exp))
					 			  (body-of-lambda (get-lambda-body sub-exp)))
					 			  (and (not (member par params))
					 				   (or  (equal? body-of-lambda `(var ,par)) 
					 				 		(ormap (lambda (sub-body) (bound? sub-body par #t)) body-of-lambda))))

			 				(bound? sub-exp par in-lambda?)))
						parsed-exp)))))

(define (has-set? parsed-exp par)
	(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(ormap (lambda (sub-exp)
				(if (and (lambda? sub-exp) (member par (get-lambda-params sub-exp)))
					#f
					(if (set? sub-exp)
				 		(let ((set-var (cadr sub-exp))
				 			   (set-body (caddr sub-exp)))
				 				(or (equal? `(var ,par) set-var) (has-set? set-body par))) 

	 					(has-set? sub-exp par))))
				parsed-exp)))

(define (has-get? lambda-body par)
	(if  (or (not (list? lambda-body)) (empty? lambda-body))
		#f
		(ormap (lambda (sub-body) (if (and (lambda? sub-body) (member par (get-lambda-params sub-body)))
									#f
									(if (set? sub-body)
										(has-get? (cddr sub-body) par)
										(or (equal? sub-body `(var ,par)) (has-get? sub-body par)))))
				lambda-body)))	

(define (box-it-par? parsed-exp par)
	(and (bound? parsed-exp par #f)
		 (has-set? (list (get-lambda-body parsed-exp)) par)
		 (has-get? (list (get-lambda-body parsed-exp)) par)))

(define (box-it? parsed-exp)
	(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(if (lambda? parsed-exp)
			(let ((params (get-lambda-params parsed-exp)))
				(ormap (lambda (par)
							(box-it-par? parsed-exp par))
						params))
			#f ))) 

(define (box-it-set parsed-exp param)
	(if (box-it-par? parsed-exp param)
		`(set (var ,param) (box (var ,param)))
	`())
)

;(define (box-it-set-list parsed-exp params)
;	(fold-left (lambda (x y) (if (empty? x) 
;								(box-it-set parsed-exp y)
;								(append x (list (box-it-set parsed-exp y)))))
;				'() params))



(define (remove-seq-from-body body)
	(if (seq? body)
		(cadr body)
		(list body)))

(define (box-it-set-list parsed-exp params)
	(map (lambda (x) (box-it-set parsed-exp x)) (filter (lambda (x) (box-it-par? parsed-exp x)) params)))


(define (box-it-logic-one-par parsed-exp)
	   (let* ((lambda-type (car parsed-exp))
			  (lambda-par (get-lambda-params parsed-exp))
			  (org-lambda-par (get-original-params parsed-exp))
			  (body-of-lambda (get-lambda-body parsed-exp))
	   		  (updated-body (fold-left box-it-update-body body-of-lambda (filter (lambda (x) (box-it-par? parsed-exp x)) lambda-par)))
	   		  (set-list (box-it-set-list parsed-exp lambda-par))
	   		  (body (map box-set (remove-seq-from-body updated-body))))

	   		  `(,lambda-type ,@org-lambda-par
					(seq (,@set-list 
						,@body)))

					))



;(define (box-it-logic-one-par parsed-exp par)
;	(let* ((lambda-type (car parsed-exp))
;				  (lambda-par (cadr parsed-exp))
;				  (body-of-lambda (caddr parsed-exp))
;				  (updated-body (box-it-update-body body-of-lambda par))

;				;(begin (display lambda-type) (newline) (display lambda-par) (newline) (display body-of-lambda) (newline) (display def-lst) (newline) (display body-lst) (newline) )
;				  (build-letrec lambda-type lambda-par body-of-lambda def-lst body-lst))))

(define (box-it-update-body parsed-exp par)
		(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
			parsed-exp
			(if (and (set? parsed-exp) (equal? (cadr parsed-exp) `(var ,par)))  
				`(box-set (var ,par) ,(box-it-update-body (caddr parsed-exp) par))
				(if (and (lambda? parsed-exp) (member par (get-lambda-params parsed-exp)))
					parsed-exp
					(if (equal? `(var ,par) parsed-exp)
						`(box-get (var ,par))
						(map (lambda (x) (box-it-update-body x par)) parsed-exp))))))
			 	  
(define box-set
	(generic box-it? box-it-logic-one-par))		 


(define (update-env env params)
	(let ((prev-params (car env))
		  (bounds (cdr env)))
	(cons params (append bounds (list prev-params)))))



(define (list-index e lst length)
	(if (empty? lst)
		(- (+ 1 length))
		(if (or (eq? (car lst) e) (equal? (car lst) e))
			0
			(+ 1 (list-index e (cdr lst) length)))))

;(define list-index
;        (lambda (e lst)
;                (if (or (empty? lst) (null? lst))
;                        -1
;                        (if (or (equal? (car lst) e) (eq? (car lst) e))
;                                0
;                                (if (= (list-index e (cdr lst)) -1) 
;                                        -1
;                                        (+ 1 (list-index e (cdr lst))))))))
(define (var-bound? var bounds)
	(fold-right (lambda (y x) 
					(if (and (empty? x) (member var y))
						y
						x))
				`()
				bounds))

(define (tag-var var env)
	(let* ((params (car env))
		  (bounds (cdr env))
		  (bound-ref (var-bound? var bounds)))


	(if (member var params)
		`(pvar ,var ,(list-index var params (length params)))
		(if (empty? bound-ref)
			`(fvar ,var)
			(let 
				  ((minor (list-index var bound-ref (length bound-ref)))
				   (major (list-index bound-ref (reverse bounds) (length (reverse bounds)))))
			`(bvar ,var ,major ,minor))))))




	

; env = pair(params . bounds)
(define (exp-lex-env parsed-exp env)

		(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
			parsed-exp
			(if (lambda? parsed-exp)
				(let* ((lambda-type (car parsed-exp))
				  		 (lambda-par (get-lambda-params parsed-exp))
				  		 (org-lambda-par (get-original-params parsed-exp))
				  		 (updated-env (update-env env lambda-par))
				 		 (body-of-lambda (get-lambda-body parsed-exp)))
				  	`(,lambda-type ,@org-lambda-par ,(exp-lex-env body-of-lambda updated-env)))

				 (if (var? parsed-exp)
				 	 (tag-var (cadr parsed-exp) env)
				 	(map (lambda (x) (exp-lex-env x env)) parsed-exp)))))



(define (pe->lex-pe parsed-exp)
	(exp-lex-env parsed-exp (cons `() `())))

(define (annotate parsed-exp tp?)
	(if  (or (not (list? parsed-exp)) (empty? parsed-exp))
			parsed-exp
			(if (or (tc-var? parsed-exp) (const? parsed-exp)) 
				parsed-exp
				(if (applic? parsed-exp)
					(if tp? 
						`(tc-applic ,@(map (lambda (x) (annotate x #f)) (cdr parsed-exp)))
						`(applic ,@(map (lambda (x) (annotate x #f)) (cdr parsed-exp))))


					(let ((app parsed-exp))
						(cond 
						   ((or? app)
						   		`(or ,(append (map (lambda (x) (annotate x #f)) (all-but-last (cadr app))) 
						   					(list (annotate (last (cadr app)) tp?)))))
						   ((seq? app)
						   		`(seq ,(append (map (lambda (x) (annotate x #f)) (all-but-last (cadr app))) 
						   					(list (annotate (last (cadr app)) tp?)))))
						   ((if? app)
						   	 (let ((test (cadr app))
						   	 	   (dit (caddr app))
						   	 	   (dif (cadddr app)))
						   	 	`(if3 ,(annotate test #f) ,(annotate dit tp?) ,(annotate dif tp?))))
						   ((def? app)
						   		`(def ,(cadr app) ,(annotate (caddr app) #f)))
						   ((set? app)
						   		`(set ,(cadr app) ,(annotate (caddr app) #f)))
						   	((box-set-set? app)
						   		`(box-set ,(cadr app) ,(annotate (caddr app) #f)))
						   ((lambda? app)
						   		(let* ((lambda-type (car app))
					  		 			(org-lambda-par (get-original-params app))
					  		 			(lambda-body (get-lambda-body app)))
						   			`(,lambda-type ,@org-lambda-par ,(annotate lambda-body #t))))
						   (else (map (lambda (x) (annotate x tp?)) parsed-exp))


						   	))

				))))

(define (annotate-tc parsed-exp)
	(annotate parsed-exp #f))


(define *example*
'(lambda (z) (define a 5) (define b 123) (lambda (y) (define x (list 1 2 123)) (define x1 (lambda (abc) (define a 56) (define x1 10) (+ 1 2))) (f 32 45 (quote aviv))) (a 5)))

(define (pipe exp)
	 (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse exp)))))))




;##############################################################################################
;##############################################################################################
;##############################################################################################
; 									CONSTANT TABLE
;##############################################################################################
;##############################################################################################
;##############################################################################################
(define dictionary `NOT_IMPLEMENTED_YET)

(define fvar-dictionary `NOT_IMPLEMENTED_YET)


(define file->sexprs
	(lambda (filename)
		(let ((input (open-input-file filename)))
			(letrec ((run
				(lambda ()
					(let ((e (read input)))
						(if (eof-object? e)
							(begin (close-input-port input) '())
							(cons e (run)))))))
		(run)))))



(define label-counter 0)
(define (c-file-header p)
		(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '<stdio.h> p)
		(newline p)
		(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '<stdlib.h> p)
		(newline p)(newline p)
		(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'DO_SHOW p)(write-char #\x20 p)(write-char #\x31 p)
		(newline p)(newline p)
		(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"cisc.h" p)
		(newline p)(newline p)
		(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"macros.h" p)
		(newline p)(newline p)
		(write 'int p)
		(write-char #\x20 p)(write 'main p)(write-char #\x28 p)(write-char #\x29 p)
		(newline p)
		(write-char #\x7B p)
		(newline p)
		(write-char #\x09 p)(write 'START_MACHINE p)(write-char #\x3B p)
		(newline p)(newline p) 
		
		(newline p)(newline p)
		(write-char #\x09 p)(write 'JUMP p)	
		(write-char #\x28 p)(write 'CONTINUE p)(write-char #\x29 p)
		(write-char #\x3B p)
		(newline p)(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'SOB_TRUE p)
		(write-char #\x20 p)(write (look-up-not-const #t)  p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'SOB_FALSE p)
		(write-char #\x20 p)(write (look-up-not-const #f)  p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'SOB_NIL p)
		(write-char #\x20 p)(write (look-up-not-const `())  p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'SOB_VOID p)
		(write-char #\x20 p)(write (look-up-not-const void)  p)
		(newline p)
		
		(write-char #\x20 p)(write-char #\x23 p)(write 'define p)
		(write-char #\x20 p)(write 'SOB_SYMTAB p)
		(write-char #\x20 p)(write 1 p)
		(newline p)(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"char.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"scheme.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"math.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"string.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"io.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"system.lib" p)
		(newline p)
		(write-char #\x20 p)(write-char #\x23 p)(write 'include p)
		(write-char #\x20 p)(write '"functions.lib" p)
		(newline p)
		(newline p)(newline p)

		(write-char #\x20 p)(write-char #\x20 p)(write 'CONTINUE p)
		(write-char #\x3A p)(newline p)



)

(define (c-file-bottom p)
		(newline p) (newline p)
		(write-label p `(label END))
		(newline p)
		;(write-inst-unary p `(PUSH(R0)))
		;(newline p)
		;(write-inst-binary p `(CMP ((INDD((R0)(0))) (INDD ((SOB_VOID) (0))))))
		;(newline p)
		;(write-inst-unary p `(JUMP_EQ (VOID_PRINT)))
		;(newline p)
		;(write-inst-unary p `(CALL (WRITE_SOB)))
		;(newline p)
		;(write-inst-unary p `(CALL (NEWLINE)))
		(newline p)
		(write-label p `(label L_error_cannot_apply_non_clos))
		(newline p)
		(write-label p `(label L_lambda_wrong_arg_count))
		(newline p)
		(newline p)
		(write-label p `(label L_error_not_pair))
		(newline p)
		(newline p)
		(write-label p `(label L_error_arg_count))
		(newline p)

		;(write-label p `(label VOID_PRINT))
		;(newline p)




		(write-char #\x09 p)(write 'STOP_MACHINE p)(write-char #\x3B p)
		(newline p)
		(write-char #\x09 p)(write 'return p)
		(write-char #\x20 p)(write-char #\x30 p)(write-char #\x3B p)
		(newline p)
		(write-char #\x7D p)
		(newline p)		

)

(define (unary-inst? inst)
		(if (= (length (cadr inst)) 1)
			#t
			#f)
	)



(define (inst? c)
	(if  (or (not (list? c)) (empty? c))
		#f
		(or (eq? `LABEL (car c)) (eq? `IND (car c)) (eq? `INDD (car c)) (eq? `IMM (car c)))))

(define (make-write-arg p arg)
		(cond ((inst? arg)
			(if (eq? (car arg) `INDD)
					(lambda ()
						;(display 'INDD)(newline)
						(write (car arg) p) (write-char #\x28 p)
						(write (caadr arg) p) (write-char #\x2C p)
						(write (cadadr arg) p) (write-char #\x29 p))
					(if (eq? (car arg) `IMM)
						(lambda () 
							;(display 'IMM)(newline)
							(write (car arg) p) (write-char #\x28 p)
							(if (not (char? (caadr arg)))
							 	(write (caadr arg) p)
							 	(begin (write-char #\x27 p) (write-char (caadr arg) p) (write-char #\x27 p))) 
								(write-char #\x29 p))
						(lambda () 
							;(display arg)(newline)
							(write (car arg) p)
							(write (cadr arg) p)))))
			((label-num? (car arg))
				;(display `label-num)(newline)
				;(display `label-num)(newline)
				(lambda ()
					(let* 	((label (car arg))
						   	(name (cadr label))
							(num (caddr label)))

						(write name p)(write num p))))
			((fp-arg? (car arg))
				;(display `im_FPARG)	
				(lambda ()
					(let* 	((fparg (car arg))
						   	(index (cadr arg)))
						(write fparg p)(write index p))))
			((LABEL-inst? (car arg))
				;(display (car arg))(newline)
				;(display (cdar arg))(newline)	
				(lambda ()
					(let* 	((header (car arg))
						   	(label (cdar arg)))
						(write `LABEL p)(write-char #\x28 p)((make-write-arg p label))(write-char #\x29 p) )))
			
			((list? (car arg))
				(make-write-arg p (car arg)))
			(else 
				;(display `im_else)
				(lambda () 
					(write (car arg) p))))
)

(define (write-inst-unary p inst)
	(let ((name (car inst))
		  (source (make-write-arg p (cadr inst))))
		;(display (cadr inst))(newline)
		(write-char #\x09 p)(write name p) (write-char #\x28 p)
		(source)(write-char #\x29 p) (write-char #\x3B p)

	))

(define (write-inst-binary p inst)
	(let ((name (car inst))
		  (source (make-write-arg p (caadr inst)))
		  (dest (make-write-arg p (cadadr inst))))
		;(display (cadadr inst))
		(write-char #\x09 p)(write name p) (write-char #\x28 p)
		(source) (write-char #\x2C p)
		(dest) (write-char #\x29 p) (write-char #\x3B p)

	))

(define (define? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `define (car c))))

(define (write-define p inst)
	(let ((name (car inst))
		  (def (cadr inst))
		  (value (caddr inst)))
		(write-char #\x23 p)(write name p) (write-char #\x20 p)
		(write def p) (write-char #\x20 p)
		(write value p)

	))

(define (label? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `label (car c))))

(define (LABEL-inst? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `LABEL-inst (car c))))

(define (write-label p inst)
	(let ((name (car inst))
		  (label (cadr inst)))
		(write label p) (write-char #\x3A p)

	))


(define (comment? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `comment (car c))))

(define (macro? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `macro (car c))))

(define (write-comment p inst)
	(let ((name (car inst))
		  (comment (cadr inst)))
		(write-char #\x2F p)(write-char #\x2F p)(write comment p)))

(define (write-macro p inst)
	(let ((name (car inst))
		  (macro (cadr inst)))
			(write-char #\x9 p)(write macro p)(write-char #\x3B p)))

(define (label-num? c)
		(if  (or (not (list? c)) (empty? c))
		#f
		(eq? `label-num (car c))))

(define (fp-arg? c)
		;(newline)(display c)(newline)	

		(or (eq? `FPARG c) (eq? `STARG c)))

(define (write-label-num p inst)
	(let ((name (car inst))
		  (label (cadr inst))
		  (label-num (caddr inst)))
		(write label p)(write label-num p)(write-char #\x3A p)

	))

(define (out-to-file filename list-to-be-printed) 
(let ((p (open-output-file filename)))
	(c-file-header p)(newline p)
  (let f ((ls list-to-be-printed))
    (if (not (null? ls))
        (begin
          (cond ((comment? (car ls))
          			(write-comment p (car ls)))
          	    ((define? (car ls))
          			(write-define p (car ls)))
          		((label? (car ls))
          			(write-label p (car ls)))
          		((macro? (car ls))
          			(write-macro p (car ls)))
          		((label-num? (car ls))
          			(write-label-num p (car ls)))
          		((unary-inst? (car ls))	
			          (write-inst-unary p (car ls)))
          		(else 
			          (write-inst-binary p (car ls))))	
          (newline p)
          (f (cdr ls)))))
  	(c-file-bottom p)
  (close-output-port p)))





(define default-const-list 
	(let ((void (if #f #f)))
	  `((const ,void)
	 	(const ())
	 	(const #t)
	 	(const #f)
	 	(const 0)
	 	(const 1)
	 	(const 2)
	 	)
	))

(define default-fvar-list 
	  `((fvar car)
	 	(fvar cdr)
	 	(fvar cons)
	 	(fvar denominator)
	 	(fvar numerator)
	 	(fvar char->integer)
	 	(fvar integer->char)
	 	(fvar char?)
	 	(fvar boolean?)
	 	(fvar integer?)
	 	(fvar pair?)
	 	(fvar string?)
	 	(fvar procedure?)
	 	(fvar null?)
	 	(fvar rational?)
	 	(fvar symbol?)
	 	(fvar number?)
	 	(fvar zero?)
	 	(fvar vector?)
	 	(fvar not)
	 	(fvar string-length)
	 	(fvar string-ref)
	 	(fvar vector-length)
	 	(fvar vector-ref)
	 	(fvar make-string)
	 	(fvar make-vector)
	 	(fvar map)
	 	(fvar remainder)
	 	(fvar andmap)
	 	(fvar =)
	 	(fvar eq?)
	 	(fvar +)
	 	(fvar *)
	 	(fvar -)
	 	(fvar list)
	 	(fvar lib_append_2_arg)
	 	(fvar asm-append)
	 	(fvar append)
	 	(fvar reverse)
	 	(fvar lib_foldl)
	 	(fvar >)
	 	(fvar <)
	 	(fvar /)
	 	(fvar asm-apply)
	 	(fvar apply)
	 	(fvar set-car!)
	 	(fvar set-cdr!)
	 	(fvar string-set!)
	 	(fvar vector-set!)
	 	(fvar vector)
	 	(fvar symbol->string)
	 	(fvar string->symbol)
	 )

	)




(define (create-lib-functions)
		`(  
			(comment CREATE_LIBRARY_FUNCTIONS)
			(CALL (CAR))
			(MOV ((IND(,(look-up-fvar `car))) (R0)))
			(CALL (CDR))
			(MOV ((IND(,(look-up-fvar `cdr))) (R0)))
			(CALL (CONS))
			(MOV ((IND(,(look-up-fvar `cons))) (R0)))
			(CALL (DENOMINATOR))
			(MOV ((IND(,(look-up-fvar `denominator))) (R0)))
			(CALL (NUMERATOR))
			(MOV ((IND(,(look-up-fvar `numerator))) (R0)))
			(CALL (CHARTOINTEGER))
			(MOV ((IND(,(look-up-fvar `char->integer))) (R0)))
			(CALL (INTEGERTOCHAR))
			(MOV ((IND(,(look-up-fvar `integer->char))) (R0)))
			(CALL (isCHAR))
			(MOV ((IND(,(look-up-fvar `char?))) (R0)))
			(CALL (isBOOLEAN))
			(MOV ((IND(,(look-up-fvar `boolean?))) (R0)))
			(CALL (isINTEGER))
			(MOV ((IND(,(look-up-fvar `integer?))) (R0)))
			(CALL (isPAIR))
			(MOV ((IND(,(look-up-fvar `pair?))) (R0)))
			(CALL (isSTRING))
			(MOV ((IND(,(look-up-fvar `string?))) (R0)))
			(CALL (isCLOSURE))
			(MOV ((IND(,(look-up-fvar `procedure?))) (R0)))
			(CALL (isNIL))
			(MOV ((IND(,(look-up-fvar `null?))) (R0)))
			(CALL (isRATIONAL))
			(MOV ((IND(,(look-up-fvar `rational?))) (R0)))
			(CALL (isSYMBOL))
			(MOV ((IND(,(look-up-fvar `symbol?))) (R0)))
			(CALL (isNUMBER))
			(MOV ((IND(,(look-up-fvar `number?))) (R0)))
			(CALL (isZERO))
			(MOV ((IND(,(look-up-fvar `zero?))) (R0)))
			(CALL (isVECTOR))
			(MOV ((IND(,(look-up-fvar `vector?))) (R0)))
			(CALL (NOT))
			(MOV ((IND(,(look-up-fvar `not))) (R0)))
			(CALL (STRING_LENGTH))
			(MOV ((IND(,(look-up-fvar `string-length))) (R0)))
			(CALL (STRING_REF))
			(MOV ((IND(,(look-up-fvar `string-ref))) (R0)))
			(CALL (VECTOR_LENGTH))
			(MOV ((IND(,(look-up-fvar `vector-length))) (R0)))
			(CALL (VECTOR_REF))
			(MOV ((IND(,(look-up-fvar `vector-ref))) (R0)))
			(CALL (MAKE_STRING))
			(MOV ((IND(,(look-up-fvar `make-string))) (R0)))
			(CALL (MAKE_VECTOR))
			(MOV ((IND(,(look-up-fvar `make-vector))) (R0)))
			,@(code-gen (pipe lib_map))
			(MOV ((IND(,(look-up-fvar `map))) (R0)))
			(CALL (REMAINDER))
			(MOV ((IND(,(look-up-fvar `remainder))) (R0)))
			,@(code-gen (pipe lib_andmap))
			(MOV ((IND(,(look-up-fvar `andmap))) (R0)))
			,@(code-gen (pipe lib_mathequals))
			(MOV ((IND(,(look-up-fvar `=))) (R0)))
			(CALL (isEQ))
			(MOV ((IND(,(look-up-fvar `eq?))) (R0)))
			(CALL (PLUS))
			(MOV ((IND(,(look-up-fvar `+))) (R0)))
			(CALL (MULTIPLY))
			(MOV ((IND(,(look-up-fvar `*))) (R0)))
			(CALL (MINUS))
			(MOV ((IND(,(look-up-fvar `-))) (R0)))
			(CALL (LIST))
			(MOV ((IND(,(look-up-fvar `list))) (R0)))
			(CALL (ASM_APPEND))
			(MOV ((IND(,(look-up-fvar `asm-append))) (R0)))
			,@(code-gen (pipe lib_append_2_arg))
			(MOV ((IND(,(look-up-fvar `lib_append_2_arg))) (R0)))
			,@(code-gen (pipe lib_append))
			(MOV ((IND(,(look-up-fvar `append))) (R0)))
			,@(code-gen (pipe lib_reverse))
			(MOV ((IND(,(look-up-fvar `reverse))) (R0)))
			,@(code-gen (pipe lib_foldl))
			(MOV ((IND(,(look-up-fvar `lib_foldl))) (R0)))
			(CALL (GREATERTHAN))			
			(MOV ((IND(,(look-up-fvar `>))) (R0)))
			(CALL (LESSTHAN))
			(MOV ((IND(,(look-up-fvar `<))) (R0)))
			(CALL (DIVIDE))
			(MOV ((IND(,(look-up-fvar `/))) (R0)))
			(CALL (ASM_APPLY))
			(MOV ((IND(,(look-up-fvar `asm-apply))) (R0)))
			,@(code-gen (pipe lib_apply))
			(MOV ((IND(,(look-up-fvar `apply))) (R0)))
			(CALL (SETCAR))
			(MOV ((IND(,(look-up-fvar `set-car!))) (R0)))
			(CALL (SETCDR))
			(MOV ((IND(,(look-up-fvar `set-cdr!))) (R0)))
			(CALL (STRINGSET))
			(MOV ((IND(,(look-up-fvar `string-set!))) (R0)))
			(CALL (VECTORSET))
			(MOV ((IND(,(look-up-fvar `vector-set!))) (R0)))
			(CALL (VECTOR))
			(MOV ((IND(,(look-up-fvar `vector))) (R0)))
			(CALL (SYMBOLTOSTRING))
			(MOV ((IND(,(look-up-fvar `symbol->string))) (R0)))
			(CALL (STRINGTOSYMBOL))
			(MOV ((IND(,(look-up-fvar `string->symbol))) (R0)))

			(comment END_LIBRARY_FUNCTIONS)

		)	
)

(define lib_map 
 `(lambda (func lst)
  	(if (null? lst) lst
  		(cons (func (car lst)) (map func (cdr lst))))

  )
)


(define lib_accmap
	`(lambda (lst func default) 
		(if (null? lst)
			default
			(let ((first (car lst))
				  (second (accmap (cdr lst) func default)))
			(func (numerator first) (denominator first) 
					(numerator second) (denominator second))))))

(define lib_foldl 
	`(lambda (lst func default) 
		(if (null? lst)
			default
			(let ((first (car (reverse lst)))
				  (second (lib_foldl (reverse (cdr (reverse lst))) func default)))
			(func (numerator first) (denominator first) 
					(numerator second) (denominator second))))))

	;`(lambda (lst) 
	;	(if (null? lst)
	;		0
	;		(plus2args (car lst) (my-accmap (cdr lst))))))

(define lib_andmap
	`(lambda (lst) 
		(if (null? lst)
			#t
			(and (car lst) (andmap (cdr lst))))))


(define lib_mathequals
 `(lambda lst
  	(let ((anc (car lst)))
  		(if (null? (cdr lst))
  			#t
  			(andmap (map (lambda (x) (eq? anc x)) lst))))))

(define lib_plus
`(lambda lst
	(if (null? lst) 0
  	(let ((anc (car lst)))
  		(if (null? (cdr lst))
  			anc
  			(accmap lst plus2args 0))))))

(define lib_mul
`(lambda lst
	(if (null? lst) 1
  	(let ((anc (car lst)))
  		(if (null? (cdr lst))
  			anc
  			(accmap lst mul2args 1))))))

(define lib_reverse
`(lambda (l)
  (if (null? l)
     l
     (append (reverse (cdr l)) (list (car l)))
 		 )
			))


(define lib_apply
	`(lambda (f lst)
			  (asm-apply f (reverse lst))))


(define lib_append
	`(lambda lst
		(cond ((null? lst) `())
			  (else (asm-append lst lib_append_2_arg)))))




(define lib_append_2_arg 
	`(lambda (first second)
		(cond ((null? first) second)
			  (else 
				(cons (car first) 
					   (lib_append_2_arg (cdr first) second))))))


(define foo 
	(lambda (e) 
	(cond ((empty? e) '())
		  ((or (number? e) (char? e) (string? e) (void? e) (null? e) (boolean? e)) `((const ,e)))
		  ((pair? e) `((const ,e) ,@(foo (car e)) ,@(foo (cdr e))))
		  ((vector? e) `((const ,e) ,@(apply append (map foo (vector->list e))))) 
		  ((symbol? e) `((const ,e) ,@(foo (string-downcase (symbol->string e)))))
		)))


(define (const? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `const)))

(define (const-fraction? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(and (eq? (car parsed-exp) `const) (number? (cadr parsed-exp)) (not (integer? (cadr parsed-exp))))
))

(define (fraction? parsed-exp)
	(and (number? parsed-exp) (not (integer? parsed-exp)))
)

(define (fvar? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `fvar)))

(define (box? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `box)))

(define (bvar? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `bvar)))

(define (pvar? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `pvar)))


(define (lambda-simple? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `lambda-simple)))

(define (lambda-var? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `lambda-var)))

(define (lambda-opt? parsed-exp)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		#f
		(eq? (car parsed-exp) `lambda-opt)))

(define (get-constants-list parsed-exp constants pred)
	(if (or (not (list? parsed-exp)) (empty? parsed-exp))
		constants
		(if (and (pred parsed-exp) (not (member parsed-exp constants))) (append constants (list parsed-exp))
		(fold-right (lambda (exp const-list)
						(if (and (pred exp) (not (member exp const-list)))
							(append const-list (list exp))
							(get-constants-list exp const-list pred)))

				constants parsed-exp))))

(define (get-fvars parsed-exp)
	(get-constants-list parsed-exp default-fvar-list fvar?))

(define (get-constants parsed-exp)
	(get-constants-list parsed-exp default-const-list const?))

(define (fix-order lst)
	(let* ((head (list-head lst 4))
		  (tail (list-tail lst 4)))

			(append tail head)))

(define (expand-list constants)
		(fix-order (fold-left (lambda (acc next-const)
												(append acc 
												(if (and (list? (cadr next-const)) (empty? (cadr next-const)))
													(list next-const)
										 			(foo (cadr next-const))))) `() constants)))


(define (my-map func lst)
	(if (empty? lst)
		'()
		(cons (func (car lst)) (my-map func (cdr lst)))))


(define (my-flatten lst)
	(let ((next-arg (list (car lst) (cadr lst))))
		(if (empty? (cddr lst))
			(cons next-arg `())
			(cons next-arg (my-flatten (cddr lst)))

	)))
(define memory-pointer 2)
(define (set-up-dictionary const-list)
		(my-map
			(lambda (const-exp)
					;(display const-exp)(newline)
					(let* ((data (cadr const-exp))
						  (size (cond ((eq? `fvar (car const-exp)) 1)
						  			  ((void? data) 1)
						  			  ((null? data) 1)
						  			  ((boolean? data) 2)
						  			  ((fraction? data) 3)
						  			  ((number? data) 2)
						  			  ((char? data) 2)
						  			  ((symbol? data) 5)
						  			  ((string? data) (+ 2 (string-length data)))
						  			  ((vector? data) (+ 2 (vector-length data)))
						  			  ((or (list? data) (pair? data)) 3)
						  			  (else 0)))
						(memory-loc memory-pointer))
						(begin 
							(set! memory-pointer (+ memory-pointer size))
						  	(cons const-exp memory-loc))))
				
					 const-list))

(define (de-dup lst)
	(if (null? lst)
		`()
		(cons (car lst) (de-dup (filter (lambda (x) (not (equal? x (car lst)))) 
										(cdr lst))
						))
	))


(define (make-dictionary parsed-exp)
	(let ((fvar-dict (set-up-dictionary (get-fvars parsed-exp)))


		(const-dict (let ((const-list (get-constants parsed-exp)))
						(if (empty? const-list)
							const-list
							(set-up-dictionary (de-dup (reverse (expand-list const-list))))))))
		  

		(begin (set! memory-pointer 2) (set! dictionary const-dict) (set! fvar-dictionary fvar-dict) dictionary)))

(define (look-up dictionary key)
	(let*  ((pair (car dictionary))
		    (data (car pair))
			(mem-ptr (cdr pair)))
	(if (null? dictionary) (begin (display `(couldnt_find: ,key))(newline))
	(if (equal? key data)
		mem-ptr
		(look-up (cdr dictionary) key)))))

(define (cg-constants parsed-exp)
	(cond ((void? (cadr parsed-exp))
				`((MOV ((R0) (IMM (SOB_VOID))))))
		  ((null? (cadr parsed-exp))
				`((MOV ((R0) (IMM (SOB_NIL))))))
		  (else 
				`((MOV ((R0) (IMM (,(look-up dictionary parsed-exp)))))))))

(define (cg-if parsed-exp)
	(let* ((test (cadr parsed-exp))
		   	 	   (dit (caddr parsed-exp))
		   	 	   (dif (cadddr parsed-exp))
		   	 	   (num (begin (set! label-counter (+ 1 label-counter)) label-counter))
		   	 	   (label_else `(label-num L_if3_else_ ,num))
		   	 	   (label_exit `(label-num L_if3_exit_ ,num))	
		   	 	)

			`(
				(comment IF)
				,@(code-gen test)
				(CMP ((R0)(IMM(SOB_FALSE))))
				(JUMP_EQ (,label_else))
				,@(code-gen dit)
				(JUMP (,label_exit))
				,label_else
				,@(code-gen dif)
				,label_exit
				(comment END_OF_IF)

			)))

(define (cg-seq parsed-exp)
	(let* ((exprs  (cadr parsed-exp)))
	(append 
		`((comment SEQUENCE))
		(fold-left append `() (map code-gen exprs))
		`((comment END_OF_SEQUENCE))
	)))

(define (cg-or parsed-exp)
	(let* ((exprs  (cadr parsed-exp))
		   (head-exp (list-head exprs (- (length exprs) 1)))
		   (num (begin (set! label-counter (+ 1 label-counter)) label-counter))
		   (label_exit `(label-num L_or_exit_ ,num))
		   (last-exp (car (reverse exprs)))
		   (or-instructions (fold-left append `() 
										(map (lambda (e)
											`(
												,@(code-gen e)
												(CMP ((R0)(IMM(SOB_FALSE))))
												(JUMP_NE (,label_exit))
											))  
										head-exp))))
	(append 
		`((comment OR))
		`(,@or-instructions)
		`(,@(code-gen last-exp))
		`(,label_exit)
		`((comment END_OF_OR))
	)))

(define (cg-applic parsed-exp)
	(let* ((params (reverse (caddr parsed-exp)))
		   (p-length (length params))
		   (proc (cadr parsed-exp))
		   (pushifempty (if (= 0 p-length) `((PUSH ((IMM(SOB_NIL))))) `((comment WAYNESSSSS10ROONEYSSS)) ))
		   (param-instructions (fold-left append `() 
									(map (lambda (e)
										`(
											,@(code-gen e)
											(PUSH (R0))
										))
									params))))
	;(newline)(display (if (= 0 p-length) `im_0 `()))(newline)(newline)
	(append 
		`((comment APPLICATION))
		`((comment ,parsed-exp))
		;pushifempty
		`(,@param-instructions)
		`((PUSH ((IMM(,p-length)))))
		`(,@(code-gen proc))
		`((CMP ((INDD(R0 0)) (IMM(T_CLOSURE)))))
		;`((macro INFO))
		`((JUMP_NE (L_error_cannot_apply_non_clos)))
		`((PUSH ((INDD(R0 1)))))
		`((CALLA ((INDD(R0 2)))))
		`((DROP(1)))
		`((POP(R1)))
		`((DROP(R1)))
		`((comment END_OF_APPLICATION))
	)))


(define (cg-tc-applic parsed-exp)
	(let* ((params (reverse (caddr parsed-exp)))
		   (p-length (length params))
		   (proc (cadr parsed-exp))
		   (pushifempty (if (= 0 p-length) `((PUSH ((IMM(SOB_NIL))))) `((comment CHUKA)) ))
		   (param-instructions (fold-left append `() 
									(map (lambda (e)
										`(
											,@(code-gen e)
											(PUSH (R0))
										))
									params))))
	;(newline)(display (if (= 0 p-length) `im_0 `()))(newline)(newline)
	(append 
		`((comment TAIL_CALL_APPLICATION))
		;pushifempty
		`(,@param-instructions)
		`((PUSH ((IMM(,p-length)))))
		`(,@(code-gen proc))
		`((CMP ((INDD(R0 0)) (IMM(T_CLOSURE)))))
		`((JUMP_NE (L_error_cannot_apply_non_clos)))
		`((PUSH ((INDD(R0 1))))) ;push env
		`((MOV ((R5) (FPARG(-1)))))
		`((PUSH (R5))) ;old ret add
		`((MOV ((R1) (FPARG(-2))))) ;R1 <= old_fp
		`((TCAPPLIC ((FPARG(1)) (STARG(1)))))
		`((JUMPA ((INDD(R0 2)))))
		`((comment END_OF_TAIL_CALL_APPLICATION))
	)))



(define lambda-major-counter 0)

(define (cg-labmda-simple parsed-exp)
		(set! lambda-major-counter (+ 1 lambda-major-counter))
		(let* ((params (cadr parsed-exp))
			  (body (caddr parsed-exp))
			  (num (begin (set! label-counter (+ 1 label-counter)) label-counter))
		      (lambda-body-label `(label-num L_body_clos_ ,num))
		      (lambda-exit-label `(label-num L_exit_clos_ ,num))
			  (major lambda-major-counter)
			  (code-gen-body (code-gen body)))
			  (set! lambda-major-counter (- lambda-major-counter 1)) 
				`(
					(comment LAMBDA-SIMPLE-START)
					(MOV ((R1) (FPARG(0))))
					(PUSH ((IMM (,major))))
					(CALL (MALLOC))
					(DROP (1))
					(MOV ((R2) (R0)))
					(comment R1=OLD_ENV)
					(comment R2=VECTOR_ADD)	
					(ENVCOPIER (,major))	
					(PUSH ((LABEL-inst(,@lambda-body-label))))
					(PUSH (R2))
					(CALL(MAKE_SOB_CLOSURE))
					(DROP(2))
					(JUMP (,lambda-exit-label))
					(,@lambda-body-label)
					(PUSH (FP))
					(MOV ((FP) (SP)))
					(CMP ((FPARG(1)) (IMM (,(length params)))))
					(JUMP_NE (L_lambda_wrong_arg_count))
					,@code-gen-body
					(POP (FP))
					(macro RETURN)
					(,@lambda-exit-label)
					(comment LAMBDA-SIMPLE-END)
		)
			))


(define (cg-labmda-opt parsed-exp)
		(set! lambda-major-counter (+ 1 lambda-major-counter))
		(let* ((params (cadr parsed-exp))
			  (body (cadddr parsed-exp))
			  (num (begin (set! label-counter (+ 1 label-counter)) label-counter))
		      (lambda-body-label `(label-num L_body_clos_ ,num))
		      (lambda-exit-label `(label-num L_exit_clos_ ,num))
			  (major lambda-major-counter)
			  (code-gen-body (code-gen body)))
			  (set! lambda-major-counter (- lambda-major-counter 1)) 
				`(
					(comment LAMBDA-OPTIONAL-START)
					(MOV ((R1) (FPARG(0))))
					(PUSH ((IMM (,major))))
					(CALL (MALLOC))
					(DROP (1))
					(MOV ((R2) (R0)))
					(comment R1=OLD_ENV)
					(comment R2=VECTOR_ADD)	
					(ENVCOPIER (,major))	
					(PUSH ((LABEL-inst(,@lambda-body-label))))
					(PUSH (R2))
					(CALL(MAKE_SOB_CLOSURE))
					(DROP(2))
					(JUMP (,lambda-exit-label))
					(,@lambda-body-label)
					(PUSH (FP))
					(MOV ((FP) (SP)))
					(MOV ((R4) (FPARG(1))))
					(OPTIONAL((R4) (,(length params))))
					(CMP ((FPARG(1)) (IMM (,(+ 1 (length params))))))
					(JUMP_NE (L_lambda_wrong_arg_count))
					,@code-gen-body
					(POP (FP))
					(macro RETURN)
					(,@lambda-exit-label)
		)
			))


(define (cg-labmda-var parsed-exp)
		(set! lambda-major-counter (+ 1 lambda-major-counter))
		(let* ((params (cdr parsed-exp))
			  (body (caddr parsed-exp))
			  (num (begin (set! label-counter (+ 1 label-counter)) label-counter))
		      (lambda-body-label `(label-num L_body_clos_ ,num))
		      (lambda-exit-label `(label-num L_exit_clos_ ,num))
			  (major lambda-major-counter)
			  (code-gen-body (code-gen body)))
			  (set! lambda-major-counter (- lambda-major-counter 1)) 
			  ;(display (length params)) (newline)

				`(
					(comment LAMBDA-VARIADIC-START)
					(MOV ((R1) (FPARG(0))))
					(PUSH ((IMM (,major))))
					(CALL (MALLOC))
					(DROP (1))
					(MOV ((R2) (R0)))
					(comment R1=OLD_ENV)
					(comment R2=VECTOR_ADD)	
					(ENVCOPIER (,major))	
					(PUSH ((LABEL-inst(,@lambda-body-label))))
					(PUSH (R2))
					(CALL(MAKE_SOB_CLOSURE))
					(DROP(2))
					(JUMP (,lambda-exit-label))
					(,@lambda-body-label)
					(PUSH (FP))
					(MOV ((FP) (SP)))
					(MOV ((R4) (FPARG(1))))
					(VARIADIC((R4) (,(length params))))
					(CMP ((FPARG(1)) (IMM (1))))
					(JUMP_NE (L_lambda_wrong_arg_count))
					,@code-gen-body
					(POP (FP))
					(macro RETURN)
					(,@lambda-exit-label)
		)
			))


(define (cg-pvar parsed-exp)
	(let ((minor (caddr parsed-exp)))
	`(
		(MOV ((R0) (FPARG(2 + ,minor))))
	 )
))

(define (cg-pvar-set parsed-exp)
	(let ((minor (caddr parsed-exp)))
	`(
		(MOV ((FPARG(2 + ,minor)) (R0)))
	 )
))

(define (cg-pvar-box-set parsed-exp)
	(let ((minor (caddr parsed-exp)))
	`(
		(PUSH (R1))
		(MOV ( (R1) (FPARG(2 + ,minor)) ))
		(MOV ((INDD ((R1) (0))) (R0)))
		(POP (R1))
	 )
))



(define (cg-bvar parsed-exp)
	(let ((minor (cadddr parsed-exp))
		  (major (caddr parsed-exp)))
	
	`(
		(MOV ((R0) (FPARG(0))))
		(MOV ((R0) ((INDD((R0) (,major))))))
		(MOV ((R0) ((INDD((R0) (,minor))))))
	 )
))

(define (cg-bvar-set parsed-exp)
	(let ((minor (cadddr parsed-exp))
		  (major (caddr parsed-exp)))
	
	`(
		(PUSH (R1))
		(MOV ((R1) (FPARG(0))))
		(MOV ((R1) ((INDD((R1) (,major))))))
		(MOV ((INDD ((R1) (,minor))) (R0)))
		(POP (R1))
	 )
))

(define (cg-bvar-box-set parsed-exp)
	(let ((minor (cadddr parsed-exp))
		  (major (caddr parsed-exp)))
	
	`(
		(PUSH (R1))
		(MOV ((R1) (FPARG(0))))
		(MOV ((R1) ((INDD((R1) (,major))))))
		(MOV ((R1) ((INDD((R1) (,minor))))))
		(MOV ((INDD ((R1) (0))) (R0)))
		(POP (R1))
	 )
))

(define (cg-fvar parsed-exp)
	(let ((addr (look-up fvar-dictionary parsed-exp)))
	`(
		(MOV ((R0) (IND(,addr))))
	 )
))

(define (cg-fvar-set parsed-exp)
	(let ((addr (look-up fvar-dictionary parsed-exp)))
	`(
		(MOV ((IND(,addr)) (R0)))
	 )
))


(define (get-var-add exp)
		(cond 
			((pvar? exp) (cg-pvar-set exp))
			((bvar? exp) (cg-bvar-set exp))
			((fvar? exp) (cg-fvar-set exp))		

		)
	)

(define (cg-set parsed-exp)
	(let ((dest (get-var-add (cadr parsed-exp)))
		  (value (code-gen (caddr parsed-exp))))
	`(
		,@value
		,@dest
		(MOV ((R0) (IMM (SOB_VOID))))
	 )
))

(define (cg-def parsed-exp)
	;(display parsed-exp)(newline)(newline)
	(let ((dest (get-var-add (cadr parsed-exp)))
		  (value (code-gen (caddr parsed-exp))))
	;(display dest)(newline)(display value)(newline)
	`(
		,@value
		,@dest
		(MOV ((R0) (IMM (SOB_VOID))))
	 )
))

(define (cg-box-get parsed-exp)
	(let ((box (code-gen (cadr parsed-exp))))
	`(
		,@box
		(MOV ((R0) (IND(R0))))
	 )
))

(define (get-box-var-add exp)
		(cond 
			((pvar? exp) (cg-pvar-box-set exp))
			((bvar? exp) (cg-bvar-box-set exp))		

		)
	)


(define (cg-box-set parsed-exp)
	(let ((dest (get-box-var-add (cadr parsed-exp)))
		  (value (code-gen (caddr parsed-exp))))
	`(
		,@value
		,@dest
		(MOV ((R0) (IMM (SOB_VOID))))
	 )
))

(define (cg-box parsed-exp)
	(let ((arg (caddr (cadr parsed-exp))))
	`(
		(PUSH((IMM(1))))
		(CALL(MALLOC))
		(DROP(1))
		(MOV ((INDD ((R0) (0))) (FPARG(2 + ,arg))))
	 )
))


(define (code-gen parsed-exp)
	(cond ((const? parsed-exp) (cg-constants parsed-exp))
		  ((if? parsed-exp) (cg-if parsed-exp))
		  ((seq? parsed-exp) (cg-seq parsed-exp))
		  ((or? parsed-exp) (cg-or parsed-exp))
		  ((applic? parsed-exp) (cg-applic parsed-exp))
		  ((tc-applic? parsed-exp) (cg-tc-applic parsed-exp))
		  ((lambda-simple? parsed-exp) (cg-labmda-simple parsed-exp))
		  ((lambda-opt? parsed-exp) (cg-labmda-opt parsed-exp))
		  ((lambda-var? parsed-exp) (cg-labmda-var parsed-exp))
		  ((pvar? parsed-exp) (cg-pvar parsed-exp))
		  ((bvar? parsed-exp) (cg-bvar parsed-exp))
		  ((fvar? parsed-exp) (cg-fvar parsed-exp))
		  ((set? parsed-exp)  (cg-set parsed-exp))
		  ((box? parsed-exp)  (cg-box parsed-exp))
		  ((box-set-set? parsed-exp)  (cg-box-set parsed-exp))
		  ((box-get? parsed-exp)  (cg-box-get parsed-exp))
		  ((def? parsed-exp)  (cg-def parsed-exp))

		  (else (begin (display `GOT_ERROR_FOR)(newline)(display parsed-exp)(newline)(newline) `((label ERROR)))))


		)

;(define (create-dummy-frame)
;		`(  
;			(comment CREATE_DUMMY_FRAME)
;			(PUSH (SOB_NIL))
;			(PUSH ((IMM (0))))
;			(PUSH ((IMM (0))))
;			(CALL (MAKE_SOB_VECTOR))
;			(DROP (1))
;			(PUSH (R0))
;			(PUSH ((LABEL(END))))
;			(PUSH ((IMM(0))))
;			(MOV ((FP) (SP)))
;			(comment END_DUMMY_FRAME)

;		)	
;)


(define (cg parsed-exp)
	(let* ((dict (make-dictionary parsed-exp))
		   (prologe (fold-left append '() (make-prologe)))

		   (fvar-malloc `((PUSH ((IMM (,(length fvar-dictionary)))))
							  (CALL (MALLOC))
							  (DROP (1))))
		   	(symtab `((PUSH ((IMM (1))))
							  (CALL (MALLOC))
							  (DROP (1))
							  (MOV ((IND(R0))  (IMM(SOB_NIL))))))
			(lib-functions (create-lib-functions))
			(print-sexpr `((PUSH (R0))
							  (CALL (WRITE_SOB2))
							  (CALL (NEWLINE2))
							  (DROP (1))
							)))


		(append 
				symtab
				prologe 
				fvar-malloc
				
				lib-functions
				(fold-left append '() (map (lambda(exp) (append (code-gen exp) print-sexpr)) parsed-exp)))))



(define (boolean->number bool)
		(or (and bool 1) 0))

(define (make-boolean-label bool mem-ptr)
	(if bool
		`(define SOB_TRUE ,mem-ptr)
		`(define SOB_FALSE ,mem-ptr)
	))


(define (make-prologe)
	(begin (set! label-counter 0)
	(my-map (lambda (pair)
				(let ((data (cadr (car pair)))
					  (mem-ptr (cdr pair)))
					(cond ((void? data) 
							`((CALL (MAKE_SOB_VOID))))
							  ;(define SOB_VOID ,mem-ptr)))
						  ((null? data) 
							`((CALL (MAKE_SOB_NIL))))
							  ;(define SOB_NIL ,mem-ptr)))	
						  ((boolean? data) 
						  	 `((PUSH ((IMM (,(boolean->number data)))))
							  (CALL (MAKE_SOB_BOOL))
							  (DROP (1))
						  	  ;,(make-boolean-label data mem-ptr)

						  	))
						  ((fraction? data) 
						  	 `((PUSH ((IMM (,(numerator data)))))
						  	   (PUSH ((IMM (,(denominator data)))))	
							  (CALL (MAKE_SOB_FRACTION))
							  (DROP (2))

						  	))
  						  ((number? data) 
						  	 `((PUSH ((IMM (,data))))
							  (CALL (MAKE_SOB_INTEGER))
							  (DROP (1))

						  	))
  						  ((symbol? data) 
						  	 `((PUSH ((IMM (,(look-up-not-const (string-downcase (symbol->string data)))))))
							  (CALL (MAKE_SOB_SYMBOL))
							  (DROP (1))

						  	))
						  ((char? data) 
						  	 `((PUSH ((IMM (,(char->integer data)))))
							  (CALL (MAKE_SOB_CHAR))
							  (DROP (1))

						  	))
  						  ((string? data) 
  						  	  (let* ((chars (string->list data))
								     (len (length chars)))
  							
						  	 `(,@(make-push-constructor chars char->integer)
						  	  (PUSH ((IMM (,len))))
							  (CALL (MAKE_SOB_STRING))
							  (DROP (,(+ 1 len)))

						  	)))
						  ((vector? data) 
  						  	  (let* ((vars (vector->list data))
								     (len (length vars)))

						  	 `(,@(make-push-constructor vars look-up-not-const)
						  	  (PUSH ((IMM (,len))))
							  (CALL (MAKE_SOB_VECTOR))
							  (DROP (,(+ 1 len)))

						  	)))
  						  ((pair? data) 
  						  	  (let ((pairs-car `(const ,(car data)))
								     (pairs-cdr `(const ,(cdr data))))
  						  	  ;(display (look-up dictionary pairs-car))
  							
						  	 `(
						  	  (PUSH ((IMM (,(look-up dictionary pairs-cdr)))))
						  	  (PUSH ((IMM (,(look-up dictionary pairs-car)))))

							  (CALL (MAKE_SOB_PAIR))
							  (DROP (2))
						  	  )

						  ))
						)))
	

			dictionary)))

(define (make-push-constructor chars func)
		      (fold-left 
        (lambda (acc next_char)
        	 `( ,@acc (PUSH ((IMM(,(func next_char))))))) 
        `() 
        chars))

		  
(define (look-up-not-const x)
	(look-up dictionary `(const ,x)))

(define (look-up-fvar x)
	(look-up fvar-dictionary `(fvar ,x)))


(define (create-c filename exp)
	(let ((exp1  (map pipe exp)))
(out-to-file filename (cg exp1))))

(define (create-c-splice filename exp)
	(let ((exp1 (map pipe exp)))
(out-to-file filename (cg exp1))))



(define (compile-scheme-file schemefile filename)
	(create-c-splice filename 
		(let ((p (open-input-file schemefile)))
  		(let f ((x (read p)))
  			    (if (eof-object? x)
        (begin
          (close-input-port p)
          '())
        (cons x (f (read p))))
  			))))



(define testtest
`(

(define (func . numbers) 
    (if (null? numbers)
        0
        (+ (car numbers) (apply func (cdr numbers)))))
(func 9 8 7 6 5 4)


))

(define testtest2
`(

(append)


))




