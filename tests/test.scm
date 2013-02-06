#!r6rs
(import (rnrs)
	(msgpack)
	(srfi :78 lightweight-testing))

(check-set-mode! 'report-failed)

;; simple value checks
;; one byte values
(check (pack '()) => #vu8(#xC0))
(check (pack #t)  => #vu8(#xC3))
(check (pack #f)  => #vu8(#xC2))

;; fixnums
(check (pack 0)   => #vu8(0))
(check (pack 10)  => #vu8(#x0A))
(check (pack 127) => #vu8(127))
(check (pack -1)  => #vu8(#b11100001))
(check (pack -32) => #vu8(#b11100000))

;; integers
(check (pack 128)   => #vu8(#xCC 128))
(check (pack #x1020)   => #vu8(#xCD #x10 #x20))
(check (pack #x102030)   => #vu8(#xCE 0 #x10 #x20 #x30))
(check (pack #x1020304050)   => #vu8(#xCF 0 0 0 #x10 #x20 #x30 #x40 #x50))

(check (pack #xFF)   => #vu8(#xCC #xFF))
(check (pack #xFFFF) => #vu8(#xCD #xFF #xFF))
(check (pack #xFFFFFFFF) => #vu8(#xCE #xFF #xFF #xFF #xFF))
(check (pack #xFFFFFFFFFFFFFFFF) =>
       #vu8(#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

(check (pack #x-80)   => #vu8(#xD0 #x80))
(check (pack #x-8000) => #vu8(#xD1 #x80 #x00))
;; avoiding Mosh's bug
(define (print . args) (for-each display args) (newline))
(define-syntax check-with-guard
  (syntax-rules ()
    ((_ expr ...)
     (guard (e (#t (print "not executed:")
		   (for-each print '(expr ...))))
       expr ...))))
(check-with-guard
  (check (pack #x-80000000) => #vu8(#xD2 #x80 #x00 #x00 #x00))
  (check (pack #x-8000000000000000)
	 => #vu8(#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

(check (pack 3.14) => #vu8(#xCB 64 9 30 184 81 235 133 31))

(check (pack "30") => #vu8(#xA2 #x33 #x30))
(check (pack "abc") => #vu8(#b10100011 97 98 99))
(check (pack "0123456789abcdef0123456789abcdef")
       => #vu8(#xDA 0 32 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))

;; map
(check (pack '(("40" . ()))) => #vu8(#x81 #xA2 #x34 #x30 #xC0))

;; array
(check (pack '#(20)) => #vu8(#x91 #x14))

;; unpack
(check (unpack #vu8(1)) => 1)
(check (unpack #vu8(0 1) 1) => 1)
(check (unpack #vu8(#xFF)) => -1)
(check (unpack #vu8(#xE0)) => -32)

(check (unpack #vu8(#xCC 128))      => 128)
(check (unpack #vu8(#xCD #x10 #x20))=> #x1020)
(check (unpack #vu8(#xCE 0 #x10 #x20 #x30)) => #x102030)
(check (unpack #vu8(#xCF 0 0 0 #x10 #x20 #x30 #x40 #x50)) => #x1020304050)

(check (unpack #vu8(#xCC #xFF)) => #xFF)
(check (unpack #vu8(#xCD #xFF #xFF)) => #xFFFF)
(check (unpack #vu8(#xCE #xFF #xFF #xFF #xFF)) => #xFFFFFFFF)
(check (unpack #vu8(#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)) =>
       #xFFFFFFFFFFFFFFFF)

(check (unpack #vu8(#xD0 #x80)) => #x-80)
(check (unpack #vu8(#xD1 #x80 #x00)) => #x-8000)
(check (unpack #vu8(#xD2 #x80 #x00 #x00 #x00)) => #x-80000000)
(check (unpack #vu8(#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
       => #x-8000000000000000)

;; flonum
(check (unpack #vu8(#xCA 64 72 245 195))
       => (bytevector-ieee-single-ref #vu8(64 72 245 195) 0 (endianness big)))
(check (unpack #vu8(#xCB 64 9 30 184 81 235 133 31))
       => (bytevector-ieee-double-ref #vu8(64 9 30 184 81 235 133 31)
				      0 (endianness big)))

;; map
(check (unpack #vu8(#x81 #xA2 #x34 #x30 #xC0)) => '(("40" . ())))
(check (unpack #vu8(#x81 #xA2 #x34 #x30 #xC2)) => '(("40" . #f)))
(check (unpack (pack '((1 . 1) (2 . 2) (3 . 3) (4 . 4)
		       (5 . 5) (6 . 6) (7 . 7) (8 . 8)
		       (9 . 9) (10 . 10) (11 . 11) (12 . 12)
		       (13 . 13) (14 . 14) (15 . 15) (16 . 16))))
       => '((1 . 1) (2 . 2) (3 . 3) (4 . 4)
	    (5 . 5) (6 . 6) (7 . 7) (8 . 8)
	    (9 . 9) (10 . 10) (11 . 11) (12 . 12)
	    (13 . 13) (14 . 14) (15 . 15) (16 . 16)))

;; array
(check (unpack #vu8(#b10010011 1 2 3)) => '#(1 2 3))
(check (unpack (pack '#(1 2 3))) => '#(1 2 3))
(check (unpack (pack '#(1 2 3 4 5 6 7 8 9 0 11 12 13 14 15 16)))
       =>  '#(1 2 3 4 5 6 7 8 9 0 11 12 13 14 15 16))

;; raw
(check (unpack #vu8(#b10100011 #x30 #x31 #x32)) => "012")
(check (unpack #vu8(0 #b10100011 #x30 #x31 #x32) 1) => "012")
(check (unpack (pack "abc")) => "abc")
(check (unpack #vu8(#xDA 0 32 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))
       => "0123456789abcdef0123456789abcdef")
(check (unpack (pack "0123456789abcdef0123456789abcdef"))
       => "0123456789abcdef0123456789abcdef")

;; combine
(check (unpack (pack '(("foo" . #t) ("data" . #(1 2 3)))))
       => '(("foo" . #t) ("data" . #(1 2 3))))

(check-report)