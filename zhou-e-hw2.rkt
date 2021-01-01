;Eric Zhou, ezzhou
;BSL

;(1.)
; A Rental Plan is one of:
; - dvd (make-dvd Boolean Number Boolean)
; - streaming (make-streaming String String Boolean

; (make-dvd Boolean Number Boolean)
; Purpose: to create a dvd rental plan where:
; HD? is whether or not you want HD
; num-dvds is the amount of dvds shipped, between 1 and 4
; unlimited? is whether you want unlimited rental period or pay-per-view
(define-struct dvd (HD? num-dvds unlimited?))

(define dvd1 (make-dvd #false 4 #true))
(define dvd2 (make-dvd #true 3 #true))
(define dvd3 (make-dvd #true 1 #false))

; (make-streaming String Boolean Boolean) 
; Purpose: to create a streaming plan where:
; platform is the platform used for viewing
; HD? is whether or not you want HD
; unlimited? is whether you want unlimited streaming or pay-per-view
(define-struct streaming (platform HD? unlimited?))

(define streaming1 (make-streaming "PC" #true #false))
(define streaming2 (make-streaming "XBOX" #false #true))
(define streaming3 (make-streaming "Mac" #true #true))

;(2.)
; dvd Template
; fcn-for-dvd: dvd -> ???
; (define (fcn-for-dvd a-dvd)
;   (... (dvd-HD? a-dvd)
;        (dvd-num-dvds a-dvd)
;        (dvd-unlimited? a-dvd)))

; streaming Template
; fcn-for-streaming: streaming -> ???
; (define (fcn-for-streaming a-streaming)
;   (... (streaming-platform a-streaming)
;        (streaming-HD? a-streaming)
;        (streaming-unlimited? a-streaming)))

;(3.)
;Signature: dvd -> Number
;Purpose: to calculate the base monthly cost of a dvd plan given # of dvds
(define (dvd-base-cost a-dvd)
  (cond
    [(dvd-HD? a-dvd) (+ 6.99 (* (dvd-num-dvds a-dvd) 1.5))]
    [else (+ 6.99 (* (dvd-num-dvds a-dvd) 1))]))

;Signature: dvd -> Number
;Purpose: to calculate the total monthly cost of a dvd plan
(define (dvd-monthly-cost a-dvd)
  (cond
    [(dvd-unlimited? a-dvd) (+ (dvd-base-cost a-dvd) 3)]
    [else (dvd-base-cost a-dvd)]))

;Signature: streaming -> Number
;Purpose: to calculate the cost the base cost of a streaming plan
(define (streaming-base-cost a-streaming)
  (cond
    [(streaming-HD? a-streaming) 4.99]
    [else 2.99]))

;Signature: streaming -> Number
;Purpose: to calculate the total monthly cost of a streaming plan
(define (streaming-monthly-cost a-streaming)
  (cond
    [(streaming-unlimited? a-streaming) (+ (streaming-base-cost a-streaming) 5)]
    [else (streaming-base-cost a-streaming)]))

;Signature: Rental-Plan -> Number
;Purpose: To calculate the monthly cost of a rental plan
(define (monthly-cost a-plan)
  (cond
    [(dvd? a-plan) (dvd-monthly-cost a-plan)]
    [(streaming? a-plan) (streaming-monthly-cost a-plan)]))

(check-expect (monthly-cost dvd1) 13.99)
(check-expect (monthly-cost dvd2) 14.49)
(check-expect (monthly-cost dvd3) 8.49)

(check-expect (monthly-cost streaming1) 4.99)
(check-expect (monthly-cost streaming2) 7.99)
(check-expect (monthly-cost streaming3) 9.99)

;(4.)
;Signature: Rental-Plan -> Rental-Plan
;Purpose: to convert a SD rental plan into a HD rental plan
(define (make-high-def a-plan)
  (cond
    [(dvd? a-plan) (make-dvd #true (dvd-num-dvds a-plan) (dvd-unlimited? a-plan))]
    [(streaming? a-plan) (make-streaming (streaming-platform a-plan) #true (streaming-unlimited? a-plan))]))

(check-expect (make-high-def dvd2) dvd2)
(check-expect (make-high-def dvd1) (make-dvd #true 4 #true))

(check-expect (make-high-def streaming2) (make-streaming "XBOX" #true #true))
(check-expect (make-high-def streaming3) streaming3)

;(5.)
;; a ListOfString is one of
;; empty
;; (cons String ListOfString)
;; interp:  ListOfString represents a list of strings

;Signature: ListOfString -> Boolean
;Purpose: to determine if a List of Strings contains a fully numeric String
(define (contains-all-numbers? ListOfString)
  (cond
    [(empty? ListOfString) #false]
    [else
     (if (string-numeric? (first ListOfString))
         #true
         (contains-all-numbers? (rest ListOfString)))]))

(check-expect (contains-all-numbers? (cons "CS1101" (cons "A1" (cons "32" empty)))) true)
(check-expect (contains-all-numbers? (cons "CS1101" (cons "A-one" empty))) false)

;(6.)
;Signature: String -> Number
;Purpose: to count the number of x's in a string, ignoring case
(define (count str)
  (cond
    [(empty? str) 0]
    [(cons? str)
     (if (string-ci=? "x" (first str))
         (+ 1 (count (rest str)))
         (count (rest str)))]))

(check-expect (count (explode "x10w9xoiskxlnsax")) 4)
(check-expect (count (explode "af0soidsvsd09")) 0)
(check-expect (count (explode "XXXXxxxx")) 8)

;Signature: ListofString -> Number
;Purpose: to count the total number of x's in a ListOfString, ignoring case
(define (count-X ListOfString)
  (cond
    [(empty? ListOfString) 0]
    [(cons? ListOfString) (+ (count (explode (first ListOfString))) (count-X (rest ListOfString)))]))

(check-expect (count-X (cons "xoxo" (cons "2q0fwoiehdcxXx" (cons "xX_gamer_Xx" empty)))) 9)

;(7.)
;; a ListOfNaturals is one of
;; empty
;; (cons String ListOfNaturals)
;; interp:  ListOfNaturals represents a list of naturals

;Signature: ListOfString -> ListOfNaturals
;Purpose: to create a list of the lengths of each string in a list of strings
(define (length-of-strings los)
  (cond
    [(empty? los) null]
    [(cons? los) (cons (string-length (first los)) (length-of-strings (rest los)))]))

(define L1 (cons "test" (cons "six" (cons "eight" (cons "to be or not to be" empty)))))
(check-expect (length-of-strings L1) (cons 4 (cons 3 (cons 5 (cons 18 empty)))))
