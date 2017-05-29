#lang racket

(provide (all-defined-out))


;;utility
(define (bytes-split bytes size)
  (define len (bytes-length bytes))
  (unless (= 0 (modulo len size))
    (raise-type-error
     'split-bytes "byte string of length divisible by size" 0 bytes))
  (for/list ([i (in-range (/ len size))])
    (subbytes bytes (* size i) (* size (add1 i)))))


;;PRE PROCESSING
(define (pre-process data)
  (define original-length (* (bytes-length data) 8))

  (define (add-the-bit-1-and-7-zeroes data)
    (bytes-append data (bytes #b10000000)))

  (define (add-padding data)
    (if (= (modulo (* 8 (bytes-length data)) 512) 448)
        data
        (add-padding (bytes-append data (bytes 0)))))

  (define (add-length data length)
    (bytes-append data (integer->integer-bytes length 8 #f #f)))
    
  
  (add-length
   (add-padding (add-the-bit-1-and-7-zeroes data))
   original-length))

;;CONSTANTS DEFINITION
(define s (list 7 12 17 22  7 12 17 22  7 12 17
                22  7 12 17 22 5  9 14 20  5  9
                14 20  5  9 14 20  5  9 14 20 4
                11 16 23  4 11 16 23  4 11 16 23
                4 11 16 23 6 10 15 21  6 10 15
                21  6 10 15 21  6 10 15 21))

(define K (map (λ (x) (inexact->exact x))
               (for/list ([i (in-range 0 64)])
                 (floor (*
                         (expt 2 32)
                         (abs (sin (add1 i))))))))

;;WORD-BITWISE OPERATIONS DEFINITION
#| A word is an unsigned 32 bits integer
 |#

(define (word-limit w)
  ;;limit the given number to a 32bits unsigned integer
  (bitwise-and w #xffffffff))

(define (word+ w0 w1)
  (word-limit (+ w0 w1)))


(define (leftrotate x c)
  (set! x (word-limit x))
  ;(printf "l-in:~a\n" x)
  (word-limit (bitwise-ior
               (arithmetic-shift x c)
               (arithmetic-shift x (* -1 (- 32 c))))))


;;MD5 IMPLEMENTATION
(define (md5 data)
  (define message (pre-process
                   (cond
                     [(bytes? data) data]
                     [(string? data) (string->bytes/utf-8 data)]
                     [else (raise-type-error
                            'md5 "string or bytes" data)])))
  
  ;(printf "message:~a\n" message)
  (define-values (fa fb fc fd)
    (for/fold
     ([a0 #x67452301]
      [b0 #xefcdab89]
      [c0 #x98badcfe]
      [d0 #x10325476])
     ([chunk (bytes-split message 64)])
      ;;split into chunks of 512bits
      ;(println chunk)
      (define M
        (for/list ([x (bytes-split chunk 4)])
          ;;split each chunk into sixteen words of 32bits
          ;;save them into M as integers
          (integer-bytes->integer x #f #f)))
      ;(printf "M:~a\n" M)

      ;;redefine for each chunk
      (define A a0)
      (define B b0)
      (define C c0)
      (define D d0)
      ;(printf "this-b:~a\n" B)

      ;;main loop
      (for ([i (in-range 0 64)])
        ;(printf "i:~a\n" i)
        (define-values (F g)
          (cond
            [(<= 0 i 15)
             (values
              (bitwise-xor D (bitwise-and B (bitwise-xor C D))) ;F
              i ;g
              )]
            [(<= 16 i 31)
             (values
              (bitwise-xor C (bitwise-and D (bitwise-xor B C))) ;F
              (modulo (add1 (* 5 i)) 16) ;g
              )]
            [(<= 32 i 47)
             (values
              (bitwise-xor B C D) ;F
              (modulo (word+ 5 (* 3 i)) 16) ;g
              )]
            [(<= 48 i 63)
             (values
              (bitwise-xor C (bitwise-ior B (bitwise-not D))) ;F
              (modulo (* 7 i) 16) ;g
              )]
            [else (error "main loop index error")]))

        (define dTemp D)
        (set! D C)
        (set! C B)
        (define sumTemp
          (+ A F (list-ref K i) (list-ref M g)))
        ;(printf "this-M:~a\n" (list-ref M g))
        ;(printf "this-A:~a\n" A)
        ;(printf "this-F:~a\n" F)
        ;(printf "this-K:~a\n" (list-ref K i))
        ;(printf "A+F:~a\n" (+ A F))
        ;(printf "F+K:~a\n" (+ F (list-ref K i)))
        ;(printf "A+F+K:~a\n" (+ A F (list-ref K i)))
        ;(printf "sumTemp:~a\n" sumTemp)
        (define rotated (leftrotate sumTemp (list-ref s i)))
        ;(printf "rotated:~a\n" rotated)
        (set! B (word+ B rotated))
        ;(printf "new-B:~a\n" B)
        (set! A dTemp)
        ;(printf "A:~a\nB:~a\nC:~a\nD:~a\n" A B C D))
        )
      ;;main loop ended

      ;;adding this chunk's hash to result
      (set! a0 (word+ a0 A))
      (set! b0 (word+ b0 B))
      (set! c0 (word+ c0 C))
      (set! d0 (word+ d0 D))
      ;(printf "a:~a\nb:~a\nc:~a\nd:~a\n" a0 b0 c0 d0)
      (values a0 b0 c0 d0)))
  
  (define all-bytes
    (bytes-append
     (integer->integer-bytes fa 4 #f #f)
     (integer->integer-bytes fb 4 #f #f)
     (integer->integer-bytes fc 4 #f #f)
     (integer->integer-bytes fd 4 #f #f)))
  (apply string-append
   (for/list ([b all-bytes])
     (~a #:width 2 #:pad-string "0" #:align 'right
         (number->string b 16)))))
