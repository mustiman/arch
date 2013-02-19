;;; compiler.scm
;;;
;;; Programmer: ???

;;; general support routines

(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))

(define with (lambda (s f) (apply f s)))

(define member?
  (lambda (a s)
    (ormap
     (lambda (b) (eq? a b))
     s)))

(define file->list
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (letrec ((loop
		(lambda ()
		  (let ((ch (read-char port)))
		    (if (eof-object? ch) '()
			(cons ch (loop)))))))
	(let ((s (loop)))
	  (close-input-port port)
	  s)))))

(define make-char-between?
  (lambda (char<=?)
    (lambda (char-from char-to)
      (lambda (char)
	(and (char<=? char-from char)
	     (char<=? char char-to))))))

;;; The scanner recognizes parenthesis and single quote.
;;; It knows to ignore comments up to the end of the current input line,
;;; as well as whitespaces.

(define list->tokens
  (letrec ((st-init
	    (lambda (s)
	      (cond
	       ((null? s) '())
	       ((char=? (car s) #\;) (st-comment s))
	       ((char=? (car s) #\.) `((dot) ,@(st-init (cdr s))))
	       ((char=? (car s) #\') `((single-quote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\`) `((quasiquote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\,) (st-unquote (cdr s)))
	       ((char=? (car s) #\() `((lparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\)) `((rparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\#) (st-hash (cdr s)))
	       ((char=? (car s) #\") (st-string (cdr s) '()))
	       ((char-whitespace? (car s)) (st-init (cdr s)))
	       ((char-symbol? (car s))
		(st-symbol/number (cdr s) (list (car s))))
	       (else (scanner-error "What's this" s)))))
	   (st-unquote
	    (lambda (s)
	      (cond ((null? s) `((,'unquote) ,@(st-init '())))
		    ((char=? (car s) #\@)
		     `((,'unquote-splicing) ,@(st-init (cdr s))))
		    (else `((,'unquote) ,@(st-init s))))))
	   (st-symbol/number
	    (lambda (s chars)
	      (cond ((null? s)
		     `(,(make-symbol/number-token chars) ,@(st-init '())))
		    ((char-symbol? (car s))
		     (st-symbol/number (cdr s) (cons (car s) chars)))
		    ((char-delimiter? (car s))
		     `(,(make-symbol/number-token chars) ,@(st-init s)))
		    (else (scanner-error "At the end of a symbol: " s)))))
	   (st-string
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error "Expecting a \" char to close the string"))
		    ((char=? (car s) #\")
		     `((string ,(list->string (reverse chars)))
		       ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-meta-char (cdr s) chars))
		    (else (st-string (cdr s) (cons (car s) chars))))))
	   (st-meta-char
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting a string meta-char; reached EOF"))
		    ((char=? (car s) #\\) (st-string (cdr s) (cons #\\ chars)))
		    ((char=? (car s) #\") (st-string (cdr s) (cons #\" chars)))
		    ((char-ci=? (car s) #\n)
		     (st-string (cdr s) (cons #\newline chars)))
		    ((char-ci=? (car s) #\r)
		     (st-string (cdr s) (cons #\return chars)))
		    ((char-ci=? (car s) #\t)
		     (st-string (cdr s) (cons #\tab chars)))
		    ((char-ci=? (car s) #\f)
		     (st-string (cdr s) (cons #\page chars)))
		    (else (scanner-error "What kind of a meta-char is " s)))))
	   (st-hash
	    (lambda (s)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting something after #, but reached end"))
		    ((char=? (car s) #\() `((vector) ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-char-1 (cdr s)))
		    ((char-ci=? (car s) #\f)
		     `((boolean #f) ,@(st-init (cdr s))))
		    ((char-ci=? (car s) #\t)
		     `((boolean #t) ,@(st-init (cdr s))))
		    ((char=? (car s) #\;) `((comment) ,@(st-init (cdr s))))
		    (else (scanner-error
			   "Expecting t, f, \\, ( after #, but found" s)))))
	   (st-char-1
	    (lambda (s)
	      (cond ((null? s) (error 'scanner "Must be one char after #\\"))
		    (else (st-char (cdr s) (list (car s)))))))
	   (st-char
	    (lambda (s chars)
	      (cond ((null? s) `((char ,(make-char chars)) ,@(st-init '())))
		    ((char-delimiter? (car s))
		     `((char ,(make-char chars)) ,@(st-init s)))
		    (else (st-char (cdr s) (cons (car s) chars))))))
	   (st-comment
	    (lambda (s)
	      (cond ((null? s) (st-init '()))
		    ((char=? (car s) #\newline) (st-init (cdr s)))
		    (else (st-comment (cdr s)))))))
    (lambda (s)
      (st-init s))))

(define make-symbol/number-token
  (lambda (chars)
    (let* ((string (list->string (reverse chars)))
	   (maybe-number (string->number string)))
      (if (number? maybe-number)
	  `(number ,maybe-number)
	  `(symbol ,(string->symbol (string-downcase string)))))))

(define make-char
  (lambda (chars)
    (cond ((null? chars) (scanner-error "Found #\\ without any char"))
	  ((null? (cdr chars)) (car chars))
	  (else (let* ((string (list->string (reverse chars)))
		       (maybe-number (string->number string 8)))
		  (if (number? maybe-number)
		      (integer->char maybe-number)
		      (cond ((string-ci=? string "return") #\return)
			    ((string-ci=? string "newline") #\newline)
			    ((string-ci=? string "space") #\space)
			    ((string-ci=? string "tab") #\tab)
			    ((string-ci=? string "page") #\page)
			    (else (scanner-error
				   "Can't recognize the following character: "
				   (format "#\\~s" string))))))))))

(define char-alphabetic? ((make-char-between? char-ci<=?) #\a #\z))
(define char-decimal? ((make-char-between? char<=?) #\0 #\9))

(define char-symbol?
  (let ((punc-chars (string->list "!@$%^*-_=+<>./?:")))
    (lambda (char)
      (or (char-alphabetic? char)
	  (char-decimal? char)
	  (ormap 
	   (lambda (punc-char) (char=? punc-char char))
	   punc-chars)))))

(define char-whitespace?
  (lambda (char)
    (char<=? char #\space)))

(define char-delimiter?
  (lambda (char)
    (or (char-whitespace? char)
	(not (char-symbol? char)))))

(define scanner-error
  (lambda (message s)
    (if (null? s)
	(error 'list-tokens message)
	(error 'list-tokens
	       (format "~a: [~s]~a"
		       message
		       (car s)
		       (list->string (cdr s)))))))

(define file->tokens
  (lambda (filename)
    (list->tokens
     (file->list filename))))

(define string->tokens
  (lambda (string)
    (list->tokens
     (string->list string))))
	 
	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
(define get-sexprs
  (lambda (toks ret-exps)
    (get-sexpr toks (lambda (sexpr toks) 
							(get-sexprs toks 
												(lambda (sexprs toks) (ret-exps (cons sexpr sexprs) toks))))
                    (lambda () (ret-exps '() toks)))))



(define get-sexpr
  (lambda (token ret-sexpr+toks ret-fail)
     (cond ((or (null? token) (eq? (caar token) 'rparen) (eq? (caar token) 'dot)) (ret-fail))
           ((or (eq? (caar token) 'boolean) (eq? (caar token) 'symbol) (eq? (caar token) 'string) (eq? (caar token) 'char) (eq? (caar token) 'number))
               (ret-sexpr+toks (cadar token) (cdr token)))
            ((eq? (caar token) 'vector)
               (get-sexprs (cdr token) (lambda (sexprs toks)
                                         (if (and (pair? toks)
													(eq? (caar toks) 'rparen))
													(ret-sexpr+toks (list->vector sexprs) (cdr toks))
										  toks))))
		    ((eq? (caar token) 'single-quote)
			   (get-sexpr (cdr token) (lambda (sexpr toks)
													(ret-sexpr+toks `',sexpr toks))
													(lambda () 'error)))
													
		    ((eq? (caar token) 'quasiquote)
			   (get-sexpr (cdr token) (lambda (sexpr toks)
													(ret-sexpr+toks (list 'quasiquote sexpr) toks))
													(lambda () 'error)))
													
			((eq? (caar token) 'unquote)
			   (get-sexpr (cdr token) (lambda (sexpr toks)
													(ret-sexpr+toks (list 'unquote sexpr) toks))
													(lambda () 'error)))
													
			((eq? (caar token) 'unquote-splicing)
			   (get-sexpr (cdr token) (lambda (sexpr toks)
													(ret-sexpr+toks (list 'unquote-splicing sexpr) toks))
													(lambda () 'error)))
													
		    ((eq? (caar token) 'lparen)
			   (get-sexprs (cdr token) (lambda (sexprs toks)
                                         (cond ((null? toks) 'error)
											   ((eq? (caar toks) 'rparen) (ret-sexpr+toks sexprs (cdr toks)))
											   ((eq? (caar toks) 'dot) (get-sexpr (cdr toks) 
																				  (lambda (sexpr toks)
																						(if (and (pair? toks) (eq? (caar toks) 'rparen))
																										(ret-sexpr+toks `(,@sexprs . ,sexpr) (cdr toks))
																						'error)) 
																				  (lambda () 'error)))
											  (else 'error)))))
			(else 'error))))
										  
	 
(define tokens->sexprs
	(lambda (tokenList)
		(get-sexprs tokenList (lambda (a b) a))))
	
(define lambda-tag
	(lambda (e ret-pro ret-imp ret-sym)
		(cond ((pair? e) 
				(lambda-tag (cdr e) ret-pro (lambda (s a) (ret-imp (cons (car e)s) a))
											(lambda () (ret-imp (list (car e))(cdr e)))))
			  ((null? e)(ret-pro))
			  ((symbol? e)(ret-sym))
			  (else ('error)))))
	
(define beginify
		(lambda (es)
				(cond ((null? es) `(const ,(void)))
					  ((null? (cdr es)) (car es))
					  (else `(begin ,@es)))))
					  
(define expand-cond
		(lambda (conds)
			(if (null? (cdr conds)) (cadar conds)
			`(if ,(caar conds) ,(beginify (cdar conds)) ,(expand-cond (cdr conds))))))
			
(define expand-and
		(lambda (conds)
			(cond ((null? conds) '#t)
				  ((if (null? (cdr conds)) (car conds)
					`(if ,(car conds) ,(expand-and (cdr conds)) #f))))))

(define expand-let*
		(lambda (ribs body)
			(if (null? ribs) (beginify body)
			`(let (,(car ribs)) ,(expand-let* (cdr ribs) body)))))
			
(define expand-let
		(lambda (ribs body)
			(if (null? ribs) body			
			`((lambda ,(map (lambda (a) (car a)) ribs) ,@body) ,@(map (lambda (a) (cadr a)) ribs)))))

;---------mayer's letrec
(define with (lambda (s f) (apply f s)))

(define Yn
  (lambda fs
    (let ((ms (map
		  (lambda (fi)
		    (lambda ms
		      (apply fi
			     (map (lambda (mi)
				    (lambda args
				      (apply (apply mi ms) args)))
			       ms))))
		fs)))
      (apply (car ms) ms))))

(define expand-letrec
  (lambda (e)
    (with e
      (lambda (_letrec ribs . exprs)
	(let* ((names `(,(gensym) ,@(map car ribs)))
	       (fs `((lambda ,names ,@exprs)
		     ,@(map (lambda (rib) `(lambda ,names ,(cadr rib)))
			 ribs))))
	  `(Yn ,@fs))))))
	  
;-----------------------

(define parse
		(lambda (e)
			(cond ((or (number? e) (boolean? e) (char? e) (vector? e) (string? e))
						(list 'const e))
				  ((symbol? e) (list 'var e))						
				  ((pair? e)
				  (cond
					((eq? (car e) 'quote) (list 'const (cadr e)))
					
					((eq? (car e) 'quasiquote) ((parse (expand-qq (cadr e)))))				  				  
					
					((eq? (car e) 'lambda)
						(let ((argl (cadr e))
							  (body (parse (beginify (cddr e)))))
						  (lambda-tag argl
							(lambda () `(lambda-simple ,argl ,body))
							(lambda (s a) `(lambda-opt ,s ,a ,body))
							(lambda () `(lambda-variadic ,argl ,body)))))
							
					((eq? (car e) 'if)
						(let ((t (cadr e))
							  (dit (caddr e)))
							(cond 
								((= (length e) 4) `(if-3 ,(parse t) ,(parse dit) ,(parse (cadddr e))))
								((= (length e) 3) `(if-3 ,(parse(cadr e)) ,(parse (caddr e)) (const ,(void))))
								 (else 'error))))
					
					((eq? (car e) 'define)
						(if (OR (pair? (cadr e)) (list? (cadr e)))
							(let ((var (caadr e))
								  (params (cdadr e))
								  (body (caddr e)))
						(list 'define (parse var) (parse (list 'lambda params body))))
						`(define ,(parse (cadr e)) ,(parse (caddr e)))))
						
					((eq? (car e) 'begin)
						(if (eq? (cdr e) '()) `(const ,(void))
						 `(seq ,(map parse (cdr e)))))
						 
					((eq? (car e) 'or)
						(cond ((null? (cdr e)) (parse '#f)) 
							  ((null? (cddr e)) (parse (cadr e)))
							   (else `(or ,(map parse (cdr e))))))
					
					((eq? (car e) 'let)
						(parse (expand-let (cadr e) (cddr e))))
						
					((eq? (car e) 'let*)
						(parse (expand-let* (cadr e) (cddr e))))

					((eq? (car e) 'letrec)
						(parse (expand-letrec e)))

					((eq? (car e) 'and)
						(parse (expand-and (cdr e))))
						
					((eq? (car e) 'cond)
						(parse (expand-cond (cdr e))))
					
					(else  ;applic
						(let ((app (car e))
							  (params (cdr e)))
							`(applic ,(parse app) ,(map parse params))))))
				(else
					'error))))
						

							 				
	
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e) (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define search-in-rib
	(lambda (a s ret-min ret-nf)
		(cond   ((null? s) (ret-nf))
				((eq? (car s) a) (ret-min 0))
				(else (search-in-rib a (cdr s)
						(lambda(mini) (ret-min (+ 1 mini))) ret-nf)))))

(define search-in-ribs
	(lambda (a env ret-maj+min ret-nf)
		(if (null? env)		
			(ret-nf)
			(search-in-rib a (car env)
				(lambda (mini) (ret-maj+min 0 mini))
				(lambda() (search-in-ribs a (cdr env)
					(lambda(maj mini)
						(ret-maj+min (+ 1 maj) mini))
					ret-nf))))))
					
(define pe->lex-pe
	(lambda(pe)
		(run pe '() '())))


(define run
	(lambda (pe params env)
			 (cond
				((eq? (car pe) 'const) pe)
				((eq? (car pe) 'var)
					(with pe
						(lambda (_ v)
							(search-in-rib v params 
								(lambda(mini) `(pvar ,v ,mini))
								(lambda() (search-in-ribs v env
									(lambda (maj mini) `(bvar ,v ,maj ,mini))
									(lambda() `(fvar ,v))))))))
									
				((eq? (car pe) 'lambda-simple)
					(with pe
						(lambda (_ argl body)
							`(lambda-simple ,argl ,(run body argl (cons params env))))))
				
				((eq? (car pe) 'lambda-opt)
					(with pe
						(lambda (_ argl opt body)
							(let ((new-rib `(,@argl ,opt)))
								`(lambda-opt ,argl ,opt ,(run body new-rib (cons params env)))))))

				((eq? (car pe) 'lambda-variadic)
					(with pe
						(lambda (_ argl body)
							`(lambda-variadic ,argl ,(run body (list argl) (cons params env))))))
				
				((eq? (car pe) 'define)
					(with pe
						(lambda (_ argl body)
							`(define ,(run argl params env) ,(run body params env)))))
							
				((eq? (car pe) 'if-3)
					(with pe
						(lambda (_ con doit doif)
							`(if-3 ,(run con params env) ,(run doit params env) ,(run doif params env)))))
							
				((eq? (car pe) 'applic)
							`(applic ,(run (cadr pe) params env) 
								,(map (lambda (x) (run x params env)) (caddr pe))))
								
				((eq? (car pe) 'or)
					`(or ,(map (lambda (x) (run x params env)) (cadr pe))))
					
				((eq? (car pe) 'seq)
					`(seq ,(map (lambda (x) (run x params env)) (cadr pe))))
		)))

(define run2
	(lambda (pe tp?)
			 (cond
				((eq? (car pe) 'const) pe)
				((or (eq? (car pe) 'pvar) (eq? (car pe) 'bvar) (eq? (car pe) 'fvar) (eq? (car pe) 'var)) pe)
				
				((eq? (car pe) 'if-3)
					(with pe
						(lambda (_ test dit dif)
							`(if-3 ,(run2 test #f)
								   ,(run2 dit tp?)
								   ,(run2 dif tp?)))))
								   
				((eq? (car pe) 'lambda-simple)
					(with pe
						(lambda (_ argl body)
							`(lambda-simple ,argl ,(run2 body #t)))))
				
				((eq? (car pe) 'lambda-opt)
					(with pe
						(lambda (_ argl opt body)
								`(lambda-opt ,argl ,opt ,(run2 body #t)))))

				((eq? (car pe) 'lambda-variadic)
					(with pe
						(lambda (_ argl body)
							`(lambda-variadic ,argl ,(run2 body #t)))))
								   
				((eq? (car pe) 'or)
					(let ((counter (length (cadr pe)))
						 (count (- (length (cadr pe)) 1)))
					`(or ,(map (lambda (x) (set! counter (- counter 1))
							(if (< counter count) (run2 x #f) (run2 x tp?))) (cadr pe)))))
							
				((eq? (car pe) 'seq)
					(let ((counter (length (cadr pe)))
						 (count (- (length (cadr pe)) 1)))
					`(seq ,(map (lambda (x) (set! counter (- counter 1))
							(if (< counter count) (run2 x #f) (run2 x tp?))) (cadr pe)))))
					
				((eq? (car pe) 'define)
					(with pe
						(lambda (_ argl body)
							`(define ,argl ,(run2 body #f)))))
					
				((eq? (car pe) 'applic)
							(if tp? 
								`(tc-applic ,(run2 (cadr pe) #f) ,(map (lambda (x) (run2 x #f)) (caddr pe))) 
								`(applic ,(run2 (cadr pe) #f) ,(map (lambda (x) (run2 x #f)) (caddr pe)))))
				)))

(define annotate-tc
	(lambda (pe) (run2 pe #t)))

;the initialized number for the labels	
(define generators-num 0)
(define lable-gen
	(lambda (lable) (set! generators-num (+ generators-num 1)) 
						(string-append " " lable "_" (number->string generators-num))))

(define env-size 1)

(define inc-env
		(lambda () (begin (set! env-size (+ env-size 1)) "~~~~~~~~~" (number->string env-size))))
		
(define dec-env
		(lambda () (begin(set! env-size (- env-size 1)) "**********")))

(define compile-scheme-file1
	(lambda (input-file output-file)
		(let ((debug (close-output-port (open-output-file output-file))))
		'ok)))

			
(define compile-scheme-file
	(lambda (input-file output-file)
		(let* ((input-scheme (pe->lex-pe(parse (car (tokens->sexprs (list->tokens (file->list input-file)))))))
			  (out (open-output-file output-file))
			  (start-result "/* cisc.c
 * Mock-assembly programming for a CISC-like architecture
 * 
 * Programmer: Mayer Goldberg, 2010
 */

#include <stdio.h>
#include <stdlib.h>

#include \"cisc.h\"

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

/* for debugging only, use SHOW(\"<some message>, <arg> */
#if DO_SHOW
#define SHOW(msg, x) { printf(\"%s %s = %ld\\n\", (msg), (#x), (x)); }
#else
#define SHOW(msg, x) {}
#endif



int main()
{
  START_MACHINE;

  void print_heap(){
	int i;
	printf(\"printing heap\\n\");
	for (i=ADDR(0); i>=0; i--){
		printf(\"\t element %d: \", i);
		SHOW(\" \",ADDR(i));
		}
	}
	
  void print_stack(char* comment){
	int i;
	printf(\"printing stack, FP: %d SP: %d %s\\n\", (int)(FP), (int)(SP), comment);
	for(i=SP+5; i>=0; --i){
		if(SP == i){
			printf(\"SP \");
		}
		if(FP == i){
			printf(\"FP\");
		}
		printf(\"\\telement %d: \", i);
			SHOW(\" \", STACK(i));
		}
}


  JUMP(CONTINUE);

#include \"scheme.lib\"
#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"

 CONTINUE:
  /* initial the stack */
  PUSH(IMM(0));
  PUSH(IMM(T_NIL));
  PUSH(&&END);
  PUSH(FP);
  MOV(FP,SP);
	
 /* initial the heap */
  PUSH(IMM(9));
  CALL(MALLOC);
  CALL(MAKE_SOB_VOID);
  CALL(MAKE_SOB_NIL);
  PUSH(IMM(0));
  CALL(MAKE_SOB_BOOL);
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(3);\n\n int i,j;") ;;;;;;;;;; delete i j
			  (middle-result (code-gen input-scheme 0 '()))			  
			  (end-result " END:
	print_heap();
	print_stack(\"no comment\");
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	
  STOP_MACHINE;

  return 0;
}
"))
			(display (string-append start-result middle-result end-result) out)
			(close-output-port out))))
			
(define code-gen
	(lambda (pe env params)
		(cond
			((eq? (car pe) 'const)
				(cond
					((boolean? (cadr pe)) (cg-bool (cadr pe) env))))
			((eq? (car pe) 'pvar) (cg-pvar pe env))
			((eq? (car pe) 'bvar) (cg-bvar pe env))
			((eq? (car pe) 'seq) (cg-seq pe env))
			((eq? (car pe) 'or) (cg-or pe env))
			((eq? (car pe) 'lambda-simple) (cg-lambda-simple pe (+ 1 env)))
			((eq? (car pe) 'lambda-opt) (cg-lambda-opt pe (+ 1 env)))
			((eq? (car pe) 'lambda-variadic) (cg-lambda-variadic pe (+ 1 env)))
			((eq? (car pe) 'if-3) (cg-if-3 pe env))
			((eq? (car pe) 'applic) (cg-applic pe env))
			(else ""))))

(define cg-applic
	(lambda (pe env)
		(let ((error-lable (lable-gen "L_APPLIC_ERROR_NOT_A_CLOS")))
			(with pe
				(lambda (_ proc params)
					(string-append "\t\t/* start applic*/\n"
						(accumulate (lambda (x y)
							(string-append
								(code-gen x env '())
								"\tPUSH(R0);\n"
								y)) "" (reverse params))
						"\tPUSH(" (number->string (length params)) ");\n\n"
						(code-gen proc env '())
						"\tCMP(IND(R0), T_CLOSURE);\n"
						"\tJUMP_NE (" error-lable ") ;\n"
						"\tPUSH(INDD(R0,1));\n"
						"\tCALLA(INDD(R0,2));\n"
						"\tMOV(R3,STARG(0));\n"
						"\tADD(R3,IMM(2));\n"
						"\tDROP(R3);\n"
						" " error-lable ": \n"))))))

(define cg-lambda-opt
	(lambda (pe env)
		(let ((code-lable (lable-gen "L_CLOS_OPT_CODE"))
			  (exit-lable (lable-gen "L_CLOS_OPT_EXIT"))
			  (start-env-loop (lable-gen "L_ENV_LOOP"))
			  (end-env-loop (lable-gen "L_ENV_LOOP_END"))
			  (start-params-loop (lable-gen "L_PARAMS_LOOP"))
			  (end-params-loop (lable-gen "L_PARAMS_LOOP_END"))
			  (start-pair-loop (lable-gen "L_PAIR_LOOP_START"))
			  (end-pair-loop (lable-gen "L_PAIR_LOOP_END"))
			  (start-true-loop (lable-gen "L_TRUE_LOOP_START"))
			  (end-fix-loop (lable-gen "L_FIX_LOOP_END"))
			  (start-false (lable-gen "L_FALSE_START"))
			  (start-false-loop (lable-gen "L_FALSE_LOOP_START")))
			(with pe
				(lambda (_ args opt body)
				   (let ((args-size (length args)))
					(string-append "\t\t/* start lambda-opt*/\n"
						"\tMOV(R3,IMM(" (number->string env) "));\n"
						"\tPUSH(IMM(3));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R1,R0);\n"
						"\tMOV(IND(R1),IMM(T_CLOSURE));\n"
						"\tPUSH(R3);\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(INDD(R1,1),R0);\n"

							;extend the env
						"\t\t/* starts extend env loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						"\tMOV(R5,IMM(1));\n" ;R5=j
						"\tSUB(R3,IMM(1));\n"
						
						" " start-env-loop ": \n"
						"\tCMP(R4,R3);\n"
						"\tJUMP_EQ( " end-env-loop " );\n"
						"\tMOV(INDD(INDD(R1,1),R5),INDD(FPARG(0),R4));\n"
						"\tADD(R4,IMM(1));\n"
						"\tADD(R5,IMM(1));\n"
						"\tJUMP( " start-env-loop " );\n"
						"\t\t/* ends extend env loop */\n"
						" " end-env-loop ": \n"		

						"\tPUSH(FPARG(1));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R2,INDD(R1,1));\n"
						"\tMOV(INDD(R2,0),R0);\n"

							;extend the env with params
						"\t\t/* starts extend env[0] with params loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						
						" " start-params-loop ": \n"
						"\tCMP(R4,FPARG(1));\n"
						"\tJUMP_EQ( " end-params-loop " );\n"
						"\tMOV(R5,R4);\n"
						"\tADD(R5,IMM(2));\n" ;R5 =i+2
						"\tMOV(INDD(INDD(R2,IMM(0)),R4),FPARG(R5));\n"
						"\tADD(R4,IMM(1));\n" ;i++
						"\tJUMP( " start-params-loop " );\n"
						"\t\t/* ends extend env[0] with param loop */\n"
						" " end-params-loop ": \n"							

						"\tMOV(INDD(R1,2),&&" code-lable " );\n"
						"\tMOV(R0,R1);\n"
						"\tJUMP(" exit-lable " );\n"
						  
						" " code-lable ": \n"
						"\tPUSH(FP);\n"
						"\tMOV(FP,SP);\n"
									;code generation (part B)
										;stack fixing
						"printf(\" 0- %d \\n 1- %d \\n 2- %d \\n\",FPARG(0),FPARG(1),FPARG(2));\n"
						
						"\tMOV(R6,FPARG(1));\n" ;number of all arguments
						"\tMOV(FPARG(1), IMM("(number->string (+ 1 args-size)) "));\n"
						"\tMOV(R0,IMM(11));\n"
						
								;create pairs	
						"\t\t/* starts create pairs loop */\n"
						"\tMOV(R9,R6);\n" ;i=R9
						"\tMOV(R10,IMM(" (number->string args-size) "));\n"
						
						" " start-pair-loop ": \n"
						"\tCMP(R9,R10);\n"
						"\tJUMP_EQ( " end-pair-loop " );\n"
						"\tPUSH(R0);\n"
						"\tMOV(R7,R9);\n"
						"\tADD(R7,IMM(1));\n"
						"\tPUSH(FPARG(R7));\n"
						"\tCALL(MAKE_SOB_PAIR);\n"
						"\tDROP(2);\n"
						"\tSUB(R9,IMM(1));\n"
						"\tJUMP( " start-pair-loop " );\n"	
						"\t\t/* ends create pairs loop */\n"
						" " end-pair-loop ": \n"		
						
									;create pairs					
							;	"for(i=R6; i > " (number->string args-size) "; i--){
							;		PUSH(R0);
							;		MOV(R7,i);
							;		ADD(R7,IMM(1));
							;		PUSH(FPARG(R7));
							;		CALL(MAKE_SOB_PAIR);
							;		DROP(2);
						;	} \n"
							
						"\tMOV(R7,R0);\n"
						
						"\tMOV(R8,SP);\n" ;old sp
						"\tMOV(R11,R6);\n";
						"\tSUB(R6, IMM(" (number->string args-size) "));\n" ;optional size
						"\tSUB(R6,IMM(1));\n" ;opt-1
						"\tSUB(R8,R6);\n" ;sp - (opt -1)
						
								;fixing stack	
						"\t\t/* starts fixing the stack */\n"
						
						"\tCMP(R6,IMM(-1));\n" ;check if num of args is greater than 0
						"\tJUMP_EQ( " start-false " );\n"
						
								;more than 0 args loop	
						"\t\t/* starts copying down loop */\n"
						"\tMOV(R12,IMM(" (number->string args-size) "));\n" 
						"\tADD(R12,IMM(2));\n" ;R12 = i = num of args + 2
						"\tMOV(R13,R11);\n" 
						"\tADD(R13,IMM(1));\n" ;R13 = j
												
						" " start-true-loop ": \n"
						"\tCMP(R12,IMM(-3));\n" ;check if end of loop
						"\tJUMP_EQ( " end-fix-loop " );\n"
						"\tMOV(FPARG(R13),FPARG(R12));\n"						
						"\tSUB(R12,IMM(1));\n" ;i--
						"\tSUB(R13,IMM(1));\n" ;j--
						"\tJUMP( " start-true-loop " );\n"
						
						" " start-false ": \n"
									; 0 args loop	
						"\t\t/* starts copying up loop */\n"
						"\tMOV(R12,IMM(-2));\n" ;R12 = i = -2
						"\tMOV(R13,R11);\n" 
						"\tADD(R13,IMM(2));\n" ;R13 = R11+2
												
						" " start-false-loop ": \n"
						"\tCMP(R12,R13);\n" ;check if end of loop	
						"\tJUMP_EQ( " end-fix-loop " );\n"
						"\tMOV(R14,R12);\n" ;
						"\tSUB(R14,IMM(1));\n" ;R14 = i-1
						"\tMOV(FPARG(R14),FPARG(R12));\n"
						"\tADD(R12,IMM(1));\n" ;i++
						"\tJUMP( " start-false-loop " );\n"
						
						;"if(R6 > -1){"
						;	"for(j=R11+1,i= 2+ "(number->string args-size)" ; i > -3 ; i--,j--){
						;	printf(\"from: %d -> to: %d\\n\", FPARG(i),FPARG(j));
						;	"
						;	"\tMOV(FPARG(j),FPARG(i));\n"
						;	"
						;	} }
						
						
						;	else{\n"
						;	"printf(\"other\");\n"
						;	"for(i=-2 ; i < R11+2 ; i++){
						;		printf(\"from: %d -> to: %d\\n\", FPARG(i),FPARG(i-1));
						;	"
						;	"\tMOV(FPARG(i-1),FPARG(i));\n"
						;	"
						;	} }
						;	\n"
						
						"\t\t/* finish fixing the stack */\n"
						" " end-fix-loop ": \n"
						
						"\tADD(R11,IMM(1));\n"
						"\tMOV(FPARG(R11),R7);\n" ;put opt list
						"\tMOV(SP,R8);\n"
						"\tMOV(FP,SP);\n"						
						
						"\t\t /* start code-gen body (in lambda opt) */ \n"
								
						  (code-gen body env '())
						"\t\t /* finish code-gen body and finishing lambda-opt */ \n"
						"\tPOP(FP);\n"
						"\tRETURN;\n"
						  
						" " exit-lable ": \n")))))))

(define cg-lambda-variadic
	(lambda (pe env)
		(let ((code-lable (lable-gen "L_CLOS_VAR_CODE"))
			  (exit-lable (lable-gen "L_CLOS_VAR_EXIT"))
			  (start-env-loop (lable-gen "L_ENV_LOOP"))
			  (end-env-loop (lable-gen "L_ENV_LOOP_END"))
			  (start-params-loop (lable-gen "L_PARAMS_LOOP"))
			  (end-params-loop (lable-gen "L_PARAMS_LOOP_END"))
			  (start-pair-loop (lable-gen "L_PAIR_LOOP_START"))
			  (end-pair-loop (lable-gen "L_PAIR_LOOP_END"))
			  (start-true-loop (lable-gen "L_TRUE_LOOP_START"))
			  (end-fix-loop (lable-gen "L_FIX_LOOP_END"))
			  (start-false (lable-gen "L_FALSE_START"))
			  (start-false-loop (lable-gen "L_FALSE_LOOP_START")))
			(with pe
				(lambda (_ args body)
					(string-append "\t\t/* start lambda-var*/\n"
						"\tMOV(R3,IMM(" (number->string env) "));\n"
						"\tPUSH(IMM(3));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R1,R0);\n"
						"\tMOV(IND(R1),IMM(T_CLOSURE));\n"
						"\tPUSH(R3);\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(INDD(R1,1),R0);\n"
						
							;extend the env
						"\t\t/* starts extend env loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						"\tMOV(R5,IMM(1));\n" ;R5=j
						"\tSUB(R3,IMM(1));\n"
						
						" " start-env-loop ": \n"
						"\tCMP(R4,R3);\n"
						"\tJUMP_EQ( " end-env-loop " );\n"
						"\tMOV(INDD(INDD(R1,1),R5),INDD(FPARG(0),R4));\n"
						"\tADD(R4,IMM(1));\n"
						"\tADD(R5,IMM(1));\n"
						"\tJUMP( " start-env-loop " );\n"
						"\t\t/* ends extend env loop */\n"
						" " end-env-loop ": \n"		
						
						;	"	
						;		for(i=0, j=1; i < R3-1;j++,i++){
						;			MOV(INDD(INDD(R1,1),j),INDD(FPARG(0),i));
						;	} \n"
							

						"\tPUSH(FPARG(1));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R2,INDD(R1,1));\n"
						"\tMOV(INDD(R2,0),R0);\n"
						
							;extend the env with params
						"\t\t/* starts extend env[0] with params loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						
						" " start-params-loop ": \n"
						"\tCMP(R4,FPARG(1));\n"
						"\tJUMP_EQ( " end-params-loop " );\n"
						"\tMOV(R5,R4);\n"
						"\tADD(R5,IMM(2));\n" ;R5 =i+2
						"\tMOV(INDD(INDD(R2,IMM(0)),R4),FPARG(R5));\n"
						"\tADD(R4,IMM(1));\n" ;i++
						"\tJUMP( " start-params-loop " );\n"
						"\t\t/* ends extend env[0] with param loop */\n"
						" " end-params-loop ": \n"							
						

						;	"	for(i=0; i < FPARG(1);i++){
						;			MOV(INDD(INDD(R2,0),i),FPARG(i+2));
						;		} \n"

						"\tMOV(INDD(R1,IMM(2)),&&" code-lable " );\n"
						"\tMOV(R0,R1);\n"
						"\tJUMP(" exit-lable " );\n"
						  
						" " code-lable ": \n"
						"\tPUSH(FP);\n"
						"\tMOV(FP,SP);\n"
									;code generation (part B)
										;stack fixing
						"printf(\" 0- %d \\n 1- %d \\n 2- %d \\n\",FPARG(0),FPARG(1),FPARG(2));\n"
						
						"\tMOV(R6,FPARG(1));\n" ;number of all arguments
						"\tMOV(FPARG(1), IMM(1));\n"
						"\tMOV(R0,IMM(11));\n"

								;create pairs	
						"\t\t/* starts create pairs loop */\n"
						"\tMOV(R9,R6);\n" ;i=R9
						;"\tMOV(R10,IMM(" (number->string args-size) "));\n"
						
						" " start-pair-loop ": \n"
						"\tCMP(R9,IMM(0));\n"
						"\tJUMP_EQ( " end-pair-loop " );\n"
						"\tPUSH(R0);\n"
						"\tMOV(R7,R9);\n"
						"\tADD(R7,IMM(1));\n"
						"\tPUSH(FPARG(R7));\n"
						"\tCALL(MAKE_SOB_PAIR);\n"
						"\tDROP(2);\n"
						"\tSUB(R9,IMM(1));\n"
						"\tJUMP( " start-pair-loop " );\n"	
						"\t\t/* ends create pairs loop */\n"
						" " end-pair-loop ": \n"							

							;create pairs					
						;		"for(i=R6; i > 0; i--){
						;			PUSH(R0);
						;			MOV(R7,i);
						;			ADD(R7,IMM(1));
						;			PUSH(FPARG(R7));
						;			CALL(MAKE_SOB_PAIR);
						;			DROP(2);
						;	} \n"

						"\tMOV(R7,R0);\n"
						
						"\tMOV(R8,SP);\n" ;old sp
						"\tMOV(R11,R6);\n";
						"\tSUB(R6,IMM(1));\n" ;args-1
						"\tSUB(R8,R6);\n" ;sp - (args -1)
						
								;fixing stack	
						"\t\t/* starts fixing the stack */\n"
						
						"\tCMP(R6,IMM(-1));\n" ;check if num of args is greater than 0
						"\tJUMP_EQ( " start-false " );\n"
						
								;more than 0 args loop	
						"\t\t/* starts copying down loop */\n"
						"\tMOV(R12,IMM(2));\n" ;R12 = i = 2
						"\tMOV(R13,R11);\n" 
						"\tADD(R13,IMM(1));\n" ;R13 = j
												
						" " start-true-loop ": \n"
						"\tCMP(R12,IMM(-3));\n" ;check if end of loop
						"\tJUMP_EQ( " end-fix-loop " );\n"
						"\tMOV(FPARG(R13),FPARG(R12));\n"						
						"\tSUB(R12,IMM(1));\n" ;i--
						"\tSUB(R13,IMM(1));\n" ;j--
						"\tJUMP( " start-true-loop " );\n"
						
						" " start-false ": \n"
									; 0 args loop	
						"\t\t/* starts copying up loop */\n"
						"\tMOV(R12,IMM(-2));\n" ;R12 = i = -2
						"\tMOV(R13,IMM(2));\n" ;R13 = 2
												
						" " start-false-loop ": \n"
						"\tCMP(R12,R13);\n" ;check if end of loop	
						"\tJUMP_EQ( " end-fix-loop " );\n"
						"\tMOV(R14,R12);\n" ;
						"\tSUB(R14,IMM(1));\n" ;R14 = i-1
						"\tMOV(FPARG(R14),FPARG(R12));\n"
						"\tADD(R12,IMM(1));\n" ;i++
						"\tJUMP( " start-false-loop " );\n"

					;	"if(R6 > -1){"
					;		"for(j=R11+1,i= 2 ; i > -3 ; i--,j--){
					;		printf(\"from: %d -> to: %d\\n\", FPARG(i),FPARG(j));
					;		"
					;		"\tMOV(FPARG(j),FPARG(i));\n"
					;		"
					;		} }
					;		else{\n"
					;		"printf(\"other\");\n"
					;		"for(i=-2 ; i < 2 ; i++){
					;			printf(\"from: %d -> to: %d\\n\", FPARG(i),FPARG(i-1));
					;		"
					;		"\tMOV(FPARG(i-1),FPARG(i));\n"
					;		"
					;		} }
					;		\n"
					
						"\t\t/* finish fixing the stack */\n"
						" " end-fix-loop ": \n"
						
						"\tMOV(SP,R8);\n"
						"\tMOV(FP,SP);\n"
						"\tMOV(FPARG(IMM(2)),R7);\n" ;put variadic list
						
						"\t\t /* start code-gen body (in lambda var) */ \n"								
						  (code-gen body env '())
						"\t\t /* finish code-gen body and finishing lambda-var */ \n"
						"\tPOP(FP);\n"
						"\tRETURN;\n"
						  
						" " exit-lable ": \n"))))))
						
(define cg-lambda-simple
	(lambda (pe env)
		(let ((code-lable (lable-gen "L_CLOS_CODE"))
			  (exit-lable (lable-gen "L_CLOS_EXIT"))
			  (start-env-loop (lable-gen "L_ENV_LOOP"))
			  (end-env-loop (lable-gen "L_ENV_LOOP_END"))
			  (start-params-loop (lable-gen "L_PARAMS_LOOP"))
			  (end-params-loop (lable-gen "L_PARAMS_LOOP_END")))
			(with pe
				(lambda (_ args body)
					(string-append "\t\t/* start lambda-simple*/\n"
						"\tMOV(R3,IMM(" (number->string env) "));\n"
						"\tPUSH(IMM(3));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R1,R0);\n"
						"\tMOV(IND(R1),IMM(T_CLOSURE));\n"
						"\tPUSH(R3);\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(INDD(R1,1),R0);\n"

						;extend the env
						"\t\t/* starts extend env loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						"\tMOV(R5,IMM(1));\n" ;R5=j
						"\tSUB(R3,IMM(1));\n"
						
						" " start-env-loop ": \n"
						"\tCMP(R4,R3);\n"
						"\tJUMP_EQ( " end-env-loop " );\n"
						"\tMOV(INDD(INDD(R1,1),R5),INDD(FPARG(0),R4));\n"
						"\tADD(R4,IMM(1));\n"
						"\tADD(R5,IMM(1));\n"
						"\tJUMP( " start-env-loop " );\n"
						"\t\t/* ends extend env loop */\n"
						" " end-env-loop ": \n"							

						"\tPUSH(FPARG(1));\n"
						"\tCALL(MALLOC);\n"
						"\tDROP(1);\n"
						"\tMOV(R2,INDD(R1,1));\n"
						"\tMOV(INDD(R2,0),R0);\n"

						;extend the env with params
						"\t\t/* starts extend env[0] with params loop */\n"
						"\tMOV(R4,IMM(0));\n" ;R4 =i
						
						" " start-params-loop ": \n"
						"\tCMP(R4,FPARG(1));\n"
						"\tJUMP_EQ( " end-params-loop " );\n"
						"\tMOV(R5,R4);\n"
						"\tADD(R5,IMM(2));\n" ;R5 =i+2
						"\tMOV(INDD(INDD(R2,IMM(0)),R4),FPARG(R5));\n"
						"\tADD(R4,IMM(1));\n" ;i++
						"\tJUMP( " start-params-loop " );\n"
						"\t\t/* ends extend env[0] with param loop */\n"
						" " end-params-loop ": \n"							

						"\tMOV(INDD(R1,IMM(2)),&&" code-lable " );\n"
						"\tMOV(R0,R1);\n"
						"\tJUMP(" exit-lable " );\n"
						  
						" " code-lable ": \n"
						"\tPUSH(FP);\n"
						"\tMOV(FP,SP);\n"
						"\t\t /* start code-gen body */ \n"
						  (code-gen body env '())
						"\t\t /* finish code-gen body and finishing lambda */ \n"
						"\tPOP(FP);\n"
						"\tRETURN;\n"
						  
						" " exit-lable ": \n"))))))
				
				
(define cg-if-3
	(lambda (pe env)
		(let ((dif-lable (lable-gen "L_DIF"))
			  (exit-lable (lable-gen "L_IF_EXIT")))
			(with pe
				(lambda (_ test dit dif)
					(string-append "\t\t/* start if*/\n"
								(code-gen test env '())
								"\tCMP(INDD(R0,1), INDD(12,1));\n"
								"\tJUMP_EQ(" dif-lable " );\n"
								(code-gen dit env '())
								"\tJUMP(" exit-lable " );\n"
								"\n" dif-lable ": \n"
								(code-gen dif env '())
								"\n" exit-lable ": \n" ))))))
(define cg-or
	(lambda (pe env)
		(let ((exit-lable (lable-gen "L_OR_EXIT")))
			(with pe
				(lambda (_ bodies)
					(string-append "\t\t/* start or*/\n"
						(accumulate (lambda (x y) 
							(string-append (code-gen x env '()) 
											"\tCMP(INDD(R0,1), INDD(12,1));\n"
											"\tJUMP_NE(" exit-lable " );\n"
											y)) "" bodies)
							"\n" exit-lable ": \n"))))))
								
(define cg-bool
	(lambda (bool env)
		(if (eq? bool #t) "\tMOV(R0,14);\n" "\tMOV(R0,12);\n")))
			
(define cg-seq
	(lambda (pe env)
		(with pe
			(lambda (_ bodies)
				(string-append "\t\t/* start seq*/\n"
					(accumulate (lambda (x y) (string-append (code-gen x env '()) y)) "" bodies))))))
				
(define cg-pvar
	(lambda (pe env)
		(with pe
			(lambda (_ var minor)				
				(string-append "\t\t/* start pvar*/\n"
					"\tMOV(R0,FPARG(" (number->string (+ 2 minor)) "));\n")))))

(define cg-bvar					
	(lambda (pe env)
		(with pe
			(lambda (_ var maj minor)				
				(string-append "\t\t/* start bvar*/\n"
					"\tMOV(R0, FPARG(0));\n"
					"\tMOV(R0, INDD(R0,"(number->string maj )"));\n"
					"\tMOV(R0, INDD(R0,"(number->string minor)"));\n"
					)))))
				
				
				
				
				
				
				
				
				
				
				