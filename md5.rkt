#lang racket

#|
Racket-md5
Copyright (C) 2017 Castiglia Vincenzo

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#


(provide md5)


;;helper function
;;used to split the message into 64 bytes chunks
;;and to split each chunk into sixteen 4 bytes word
(define (bytes-split bytes size)
  (define len (bytes-length bytes))
  (unless (= 0 (modulo len size))
    (raise-type-error
     'split-bytes "byte string of length divisible by size" 0 bytes))
  (for/list ([i (in-range (/ len size))])
    (subbytes bytes (* size i) (* size (add1 i)))))


;;PRE PROCESSING
;;at the end of the pre process the message will have
;;1) the original data
;;2) the bit 1
;;3) zeroes until the message length is congruent to 512 modulo 448
;;4) the original length in bits (64 bits integer)
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
(define s (list 7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
                5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
                4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
                6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21))

(define K (map (λ (x) (inexact->exact x))
               (for/list ([i (in-range 0 64)])
                 (floor (* (expt 2 32) (abs (sin (add1 i))))))))

;;WORD-BITWISE OPERATIONS DEFINITION
;; A word is an unsigned 32 bits integer

(define (word-limit w)
  ;;limit the given number to a 32bits unsigned integer
  (bitwise-and w #xffffffff))

(define (word+ w0 w1)
  (word-limit (+ w0 w1)))


(define (leftrotate x c)
  ;https://en.wikipedia.org/wiki/Circular_shift
  (set! x (word-limit x))
  (word-limit (bitwise-ior
               (arithmetic-shift x c)
               (arithmetic-shift x (* -1 (- 32 c))))))


;;MD5 IMPLEMENTATION
(define (md5 data)
  (define message
    (pre-process
     (cond ;;checks the type of the given data
       [(bytes? data) data]
       [(string? data) (string->bytes/utf-8 data)]
       [else (raise-type-error
              'md5 "string or bytes" data)])))

  
  (define-values (fa fb fc fd) ;;fa means final-A and so on
    (for/fold
     ;;define initial variables with constants
     ([a0 #x67452301]
      [b0 #xefcdab89]
      [c0 #x98badcfe]
      [d0 #x10325476])
     
     ;;split the message into chunks of 512bits
     ([chunk (bytes-split message 64)])
      ;;split each chunk into sixteen words of 32bits
      ;;save them into M as integers
      (define M (for/list ([x (bytes-split chunk 4)])
                  (integer-bytes->integer x #f #f)))

      ;;redefine for each chunk
      (define A a0)
      (define B b0)
      (define C c0)
      (define D d0)

      ;;main loop
      (for ([i (in-range 64)])
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

        ;;update variables for this iteration of the main loop
        (define dTemp D)
        (set! D C)
        (set! C B)
        (define sumTemp (+ A F (list-ref K i) (list-ref M g)))
        (define rotated (leftrotate sumTemp (list-ref s i)))
        (set! B (word+ B rotated))
        (set! A dTemp))
      ;;main loop ended

      ;;adding this chunk's hash to result
      (set! a0 (word+ a0 A))
      (set! b0 (word+ b0 B))
      (set! c0 (word+ c0 C))
      (set! d0 (word+ d0 D))
      (values a0 b0 c0 d0)))

  ;;generation of the digest in a hex string
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



#| UNIT TESTS |#
(require rackunit)

;;leftrotate tests
(check-equal?
 (leftrotate #b10011111000000001111111101010101 8)
 16733599
 "leftrotate")
(check-equal?
 (leftrotate #b10000000000000000000000000000001 2)
 6
 "leftrotate")

;;md5 tests
(check-equal?
 (md5 "")
 "d41d8cd98f00b204e9800998ecf8427e"
 "md5 of \"\"")
(check-equal?
 (md5 "The quick brown fox jumps over the lazy dog")
 "9e107d9d372bb6826bd81d3542a419d6"
 "md5 of \"The quick brown fox jumps over the lazy dog\"")
(check-equal?
 (md5 "The quick brown fox jumps over the lazy dog.")
 "e4d909c290d0fb1ca068ffaddf22cbd0"
 "md5 of \"The quick brown fox jumps over the lazy dog.\"")
