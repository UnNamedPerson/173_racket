#lang racket

;;; LIST FUNCTIONS 

; list functions (function 1)

;(define List1  list "hello" "hi" "hola" "what is up"  ) 
;(define List2  list (1 2 3 4 5) )

(define (my-append-helper List1 List2)
  (if (null? List2)
      List1 
      (my-append-helper (cons (first List2) List1) (rest List2) )
   )
  )
(define (my-append list1 list2)
  (my-append-helper list2 (reverse list1)))

;(displayln "my-append test" )
;(my-append (list 1 2 3 4) (list 4 5 6 7 8) )

;(define y 1)



(define (My-Remove-All OldList NewList Letter )
  (if (null? OldList) ; if the oldlist is empty
     (reverse NewList) ; return the new list, we use (reverse)( since the result list would be in reversed order
    (if(equal? Letter (first OldList)) ; else: check if the letter is equal to the first element on the list
      (My-Remove-All (rest OldList) NewList Letter) ;if it is equal, then recall the function without adding the first letter to the result list 
      (My-Remove-All (rest OldList) (cons (first OldList) NewList) Letter) ; else: recall the function, but this time, add the letter to the result list (notice we add the letter to the begining of the list)
   )
   )
)

;(displayln "My-Remove-All Test")
;(My-Remove-All (list "a" "c" "b" "c" "c" "b" "a" "a")(list )  "a" )


(define (My-Check-For-Duplicates Letter List)
  (if (null? List)
      #f
      (if (equal? Letter (first List))
          #t
          (My-Check-For-Duplicates Letter (rest List))
      )
  )

)

(define (My-Remove-Duplicates List1 List2)
 (if (null? List1)
    (if (My-Check-For-Duplicates (first List1) (rest List1))
      (My-Remove-Duplicates (rest List1) List2)
      (My-Remove-Duplicates (rest List1) (cons (first List1) List2) )
     )
    List2
  )
)

(displayln "My-Remove-Duplicates Test")
(My-Remove-Duplicates (list 1 2 3)(list))

;;; MATH FUNCTIONS


(define (My-Factorial x)
  (if (= x 1)
      1
     (* x (My-Factorial (- x 1)))
    )
  )


;(displayln "My-Factorial Test")
;(My-Factorial 4)

(define (My-Absolute-Value x)
   (if(< x 0) ;if x < 0
      (* x -1) ; return this
      x ; return that
   )
)

;(displayln "Absolute value")
;(My-Absolute-Value -6 )
;(My-Absolute-Value -6 )


(define (My-Fibonacci x)
  (cond
    [(= x 2)
     1 ]
    [(= x 1)
     1 ]
    [(= x 0)
     0]
    [else (+ (My-Fibonacci (- x 1)) (My-Fibonacci (- x 2) ))]
  )
    
)

;(displayln "My-Fibonacci Function")
;(My-Fibonacci 6)

(define (My-GCD x y)
  (if (= x y)
      x
      (cond
        [(> x y) (My-GCD (- x y) y) ]
        [(> y x) (My-GCD x (- y x)) ]
      )
   )

)

;(displayln "My-GCD Function")
;(My-GCD 20 8)


;;;SET FUNCTIONS
(define (My-Check-Membership Letter Set)
  (if (null? Set)
      #f
      (if (equal? Letter (first Set))
          #t
          (My-Check-Membership Letter (rest Set))
      )
  )

)

;(define (fib-helper a b n)
 ; (if (= n 0)
  ;  a
   ;(fib-helper b (+ a b) (- 1 n))))

;(define (fib n) (fib-helper 1 1 n))

;(displayln "My-Check-Membership Function")
;(if (My-Check-Membership "a" (list "a" "b" "c"))
;  (displayln "a member")
;  (displayln "NOT a member")
;)
;(if (My-Check-Membership "a" (list "b" "b" "c"))
  ;(displayln "a member")
 ; (displayln "NOT a member")
;)


; Insert an element in the set
(define (My-Insert-Element Letter Set)
    (if (My-Check-Membership Letter Set)
        Set
        (My-Insert-Element Letter (cons Letter Set) )
    )
)

;(displayln "My-Insert-Element Function")
;(My-Insert-Element 5  (list 1 2 3 4) )
;(My-Insert-Element 1  (list 1 2 3 4) )



; MAKE Intersection between two sets
(define (My-Intersection set1 set2 NewSet)
  (if(null? set1)
     NewSet
     (if (My-Check-Membership (first set1) set2)
          (My-Intersection (rest set1) set2 (cons (first set1) NewSet) )
          (My-Intersection (rest set1) set2 NewSet)
     )
  )
)




;(displayln "My-Insertsection Function")
;(My-Intersection (list 1 4)  (list 1 2 3 4) (list) )
;(My-Intersection (list 5)  (list 1 2 3 4) (list) )


;;; Required FUNCTIONS


;Finding Perfect Number Test
(define (My-Find-Perfect-Number Number i Sum)

  (cond
    [(equal? Number Sum) #t]
    [(< Number i) #f]
    [ (= (remainder Number i) 0)
      (My-Find-Perfect-Number Number (+ i 1) (+ Sum i))]
    [else (My-Find-Perfect-Number Number (+ i 1) Sum)]
  )

)
;(displayln "finding Perfect number test")
;(My-Find-Perfect-Number 5 1 0)


; Checking if Number is Abundant
(define (My-Find-Abundant-Number Number i Sum)

  (cond
    [(< Number Sum) #t]
    [(= Number i) #f]
    [ (= (remainder Number i) 0)
      (My-Find-Abundant-Number Number (+ i 1) (+ Sum i))]
    [else (My-Find-Abundant-Number Number (+ i 1) Sum)]
  )

)

;(displayln "finding Abundant number test")
;(My-Find-Abundant-Number 108 1 0)


; Test for Deficient Number
(define (My-Find-Deficient-Number Number i Sum)

  (if (My-Find-Abundant-Number Number i Sum)
      #f
    (if(My-Find-Perfect-Number Number i Sum)
       #f
       #t
    )
  )

)

;(displayln "finding Deficient number test")
;(My-Find-Deficient-Number 6 1 0)
