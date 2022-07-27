(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")


  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.





;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        (new-array-exp (exp1 exp2)
                      (let ((val1 (expval->num (value-of exp1 env)))
                            (val2 (value-of exp2 env)))
                        
                         (array-val    (new-array-helper val1 val2))))

        (update-array-exp (exp1 exp2 exp3)
                         (let ((arr (expval->array (value-of exp1 env)))
                               (index (expval->num (value-of exp2 env)))
                               (val (value-of exp3 env)))
                             (setref! (list-ref arr index) val)))

        (read-array-exp (exp1 exp2)
                       (let ((arr (expval->array (value-of exp1 env)))
                             (index (expval->num(value-of exp2 env))))
                         (deref (list-ref arr index))))

        (swap-array-exp (exp1 index1 index2)
              (let ((val1 (find-location(expval->array (value-of exp1 env)) index1))
                    (arr (expval->array (value-of exp1 env)))
                    )
                (begin
                  (change-location arr index1 (find-location arr index2))

                  (change-location arr index2 val1)

                 )
                        ))

        (length-array-exp (exp)
                (num-val (length-helper (expval->array (value-of exp env))))
                          )

       (copy-array-exp (exp)
       
           (copy-helper (expval->array (value-of exp env)) (new-array-helper (length-helper (expval->array (value-of exp env))) 0) 0)
           )

        (new-queue-exp ()
                       
                       (array-val (new-array-helper 200 0))
                      )
        
        (enqueue-exp (exp1 exp2)
                     (begin
                     (let ((arr (expval->array (value-of exp1 env)))
                            (element (expval->num (value-of exp2 env))))
                            (change-location arr (+ 2 (find-location arr 0)) element)

                       (change-location arr 0 (+ 1 (find-location arr 0)))
                       )
                 ))
        
        (queue-size-exp (exp1)
                       
                        (num-val (- (find-location (expval->array (value-of exp1 env)) 0) (find-location (expval->array (value-of exp1 env)) 1)))
                   
                        )
        
        (dequeue-exp (exp1)
                    

                     (let ((arr (expval->array (value-of exp1 env))))
                     (begin  
                     (change-location arr (+ 2 (find-location arr 1)) 0)
                     (change-location arr 1 (+ 1 (find-location arr 1)))
                     )
                     ))
                     
                     
        

        (peek-exp (exp1)
                 
                  (num-val (find-location (expval->array (value-of exp1 env)) (+ 2 (find-location (expval->array (value-of exp1 env)) 1)))))
                  
                          
        (empty-queue?-exp (exp1)

                   (let ((arr (expval->array (value-of exp1 env)) ))
                     
                    (if (eq? (find-location arr 0) (find-location arr 1) ) (bool-val #t) (bool-val #f))
                          
                                    
                            )
                     
        )
        (print-queue-exp (exp1)

                       (print-helper (expval->array (value-of exp1 env)) (find-location (expval->array (value-of exp1 env)) 0) (find-location (expval->array (value-of exp1 env)) 1))
                         )

        
        
        (else (+ 3 5))
             
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
(define new-array-helper
  (lambda (length val )
    (if (= length 0 )
        '()
        (cons (newref val ) (new-array-helper (- length 1) val)))))

 (define change-location
    (lambda (arr index val)
      (setref! (list-ref arr index) val)))

  (define find-location
    (lambda (array index)
      (deref (list-ref array index))))

 (define length-helper
    (lambda (array)
   (cond ((null? array)
          0)
         (else
          (+ 1 (length-helper (cdr array)))))))

(define copy-helper
  (lambda (array1 array2 length)
    (if (eq? (length-helper array1) length) (array-val (cons (car array2)(cdr array2)))
    (begin

      (change-location array2 length (find-location array1 length))
      (copy-helper array1 array2 (+ length 1))
     
     )
        )
    ))
      
 (define print-helper
   (lambda (array end start)
     (begin
     (if (eq? (- (find-location array 0) (find-location array 1)) start) (display "") (display (find-location array (+ (+ 2 start) (find-location array 1))))) 
     (if (eq? (- (find-location array 0) (find-location array 1)) start) (display "") (print-helper array end (+ start 1)))
     )
     ))
    
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;;
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
  )
  


  
