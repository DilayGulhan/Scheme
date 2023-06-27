#lang scheme


;PLEASE OPEN THE COMMENT SECTION THAT I MENTION BELOW TO CHOOSE WHICH PART YOU WANT TO CONTROL


;1.PART 
(define lambda1 (lambda(n)( * n n) ))
(define lambda2 (lambda ()(displayln "I'm the simple lambda function which only prints to screen :) ")))
(define pair5(cons "Adil" "Burak"))

(define pair3(cons pair5 (list "Hüseyin"  "Samet"  null  "Dilek"  "Altuğ")  ))
(define pair7(cons "Serhat" (cons(cons(cons 1923 "Güliz") "Nuri") (cons "Eren" (cons null lambda2)))))
(define pair6(list "Didem"  lambda1  (cons "Büşra" pair7)))
(define pair4(list "Cansu"  "Ege"  pair6 pair7 ) )

(define pair8(cons 1992  (cons pair3 pair4)))
(define pair9( cons  (cons pair5 (cons  "Ersin" 7 )) "Didem"   ))

(define pair2(cons pair8 (cons pair9 lambda1)  ))

(define pair10(cons 3 pair8 ))
(define pair1 (cons pair10 pair10 ) )
(define pair(cons pair1 pair2))



;2.PART

(define findAll(list))

(define (myproc v1 v2 v3)
  (cond
    ((member v1 findAll) (void))
    ((pair? v1)
     (begin
       (set! findAll (cons v1 findAll))
       (myproc (cdr v1) v2 v3)
       (myproc (car v1) v2 v3)))
    ((list? v1)
     (begin
       (set! findAll (cons v1 findAll))
       (let loop ((lst (reverse v1)))
         (if (null? lst)
             '()
             (begin
               (myproc (car lst) v2 v3)
               (loop (cdr lst)))))))
    (else
     (begin
       (set! findAll (cons v1 findAll))
       (cond
        ((null? v1) (void))
        ((or (string? v1) (number? v1))
         (if (v2 v1) (v3 v1) (void)))
        ((procedure? v1) (v3 v1)))))))

(define v2 (lambda(x)(cond ((or (string? x) (number? x)) #t)
      (else #f))))

;(myproc pair v2 displayln) ; 1.OUTPUT to observe Part 3's first output open that comment part

;PART3




(define (isPrime? n)
  (cond
    ((string? n )#f)
    ((<= n 1) #f)    
    ((= n 2) #t)     
    (else
      (define (controlNum count)
        (cond
          ((> count (sqrt n)) #t) 
          ((= (modulo n count) 0) #f)     
          (else (controlNum (+ count 1))) 
        )
      )
      (controlNum 3) 
    )
  )
)

(define (whetherNumber n)
  (if ( number? n)
      (displayln n)
      (void)))

;(myproc  pair isPrime? whetherNumber) ; 2.OUTPUT to find prime numbers which is part 3 's 2. output open that comment part


(define (stringNumber x)
  (if (and (string? x) (< 5 (string-length x) ))
      #t
      #f))
 



(define (whetherString n)
  (if (string? n)
      (displayln n)
      (void)))


;(myproc pair stringNumber whetherString) ; 3.OUTPUT to find string which has longer char than 5 please open this comment section

  
;lambda1  
(define (getsSquare x) (if (and (= (procedure-arity x) 1) (procedure? x) ) (displayln (x 17))(void)))


;(myproc pair procedure? getsSquare) ; ; 4. OUTPUT to find lambda1's output and check this function only gets one input open this comment section 



;lambda2
(define (simpleLambdaFunc x)(if (= 0 (procedure-arity x))(x)(void)))

;(myproc pair procedure? simpleLambdaFunc)  ; 5. OUTPUT to find lambda2's output and check this function only gets one input open this comment section 


