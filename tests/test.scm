#!r6rs
(import (rnrs)
	(msgpack)
	(srfi :78 lightweight-testing))

(check-set-mode! 'report-failed)

;; simple value checks

;; fixnums
(check (pack 0)   => #vu8(0))
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

(check (pack "abc") => #vu8(#b10100011 97 98 99))
(check (pack "0123456789abcdef0123456789abcdef")
       => #vu8(#xDA 0 32 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102 48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))

;; map

;; array

(check-report)