
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;; remove duplicates from a list
(define remove-duplicates
  (lambda (bitvector)
    (cond ((null? bitvector) '())
          ((member (car bitvector) (cdr bitvector)) (remove-duplicates (cdr bitvector)))
          (else (cons (car bitvector) (remove-duplicates (cdr bitvector)))))
))

;; create the bitvector through hashing
(define create-bitvector
  (lambda (hashfunctionlist dict)
    (cond ((null? dict) '())
          (else (append (hash hashfunctionlist (car dict)) (create-bitvector hashfunctionlist (cdr dict)))))
))

;; hash function
(define hash
  (lambda (hashfunctionlist word)
    (cond ((null? hashfunctionlist) '())
          (else (cons (inexact->exact ((car hashfunctionlist) word)) (hash (cdr hashfunctionlist) word))))
))

;; check to see if the hash value from word exists in bitvector
(define check-word
  (lambda (hashfunctionlist word bitvector)
    (cond ((null? hashfunctionlist) #t)
          ((and (check-bitvector (inexact->exact((car hashfunctionlist) word)) bitvector) (check-word (cdr hashfunctionlist) word bitvector)) #t)
          (else #f))
))

(define check-bitvector
  (lambda (value bitvector)
    (define result (reduce contains bitvector value))
    (cond ((number? result) #f)
          ((boolean? result) #t))
))

;; Used in reduce
(define contains
  (lambda (a b)
    (cond ((eq? a b) #t)
          ((or (eq? a #t) (eq? b #t)) #t)
          (else b))
))
;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if (null? w)
        5387
        (+ (* 31 (key (cdr w))) (ctv (car w))))
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (modulo (key w) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
           (truncate (* size (- (* (key w) A) (truncate (* (key w) A)) ) )))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))


;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 538
;;  (hash-1 '(w a y))         ==> 635
;;  (hash-1 '(r a i n b o w)) ==> 308
;;
;;  (hash-2 '(h e l l o))     ==> 379
;;  (hash-2 '(w a y))         ==> 642
;;  (hash-2 '(r a i n b o w)) ==> 172
;;
;;  (hash-3 '(h e l l o))     ==> 415.0
;;  (hash-3 '(w a y))         ==> 390.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (define bitvector (remove-duplicates (create-bitvector hashfunctionlist dict)))
    (lambda (word)
      (check-word hashfunctionlist word bitvector)
)))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

