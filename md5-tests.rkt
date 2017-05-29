#lang racket/base

(require rackunit "md5.rkt")


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