;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; msgpack.sls - MessagePack library for R6RS Scheme
;;;
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; 6 Sep 2013 - adjust MessagePack update proposal v5

#!r6rs
(library (msgpack)
    (export pack! pack pack-size
	    unpack)
    (import (for (rnrs) run expand)
	    (for (rnrs eval) expand)
	    (rnrs mutable-pairs))

  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))
  ;; constant values
  (define-syntax define-constant
    (lambda (x)
      (define (eval-expr k expr)
	(if (pair? (syntax->datum expr))
	    (let ((r (eval (syntax->datum expr) (environment '(rnrs)))))
	      (datum->syntax k r))
	    expr))
      (syntax-case x ()
	((k name expr)
	 (with-syntax ((value (eval-expr #'k #'expr)))
	   #'(define-syntax name
	       (lambda (y)
		 (syntax-case y ()
		   (var (identifier? #'var) value)))))))))

  (define-constant $2^8  (- (expt 2  8) 1))
  (define-constant $2^16 (- (expt 2 16) 1))
  (define-constant $2^32 (- (expt 2 32) 1))

  ;; Tags from spec (ordered by category)
  ;; I wish it has more organised tag but this might be a
  ;; backward compatibility so don't blame... *sigh*
  ;;
  ;;                  | first byte  | first byte 
  ;;     format name  | (in binary) | (in hex)
  ;; -----------------+-------------+--------------
  ;; positive fixint  |  0xxxxxxx   | 0x00 - 0x7f
  ;; fixmap           |  1000xxxx   | 0x80 - 0x8f
  ;; fixarray         |  1001xxxx   | 0x90 - 0x9f
  ;; fixstr           |  101xxxxx   | 0xa0 - 0xbf
  ;; negative fixint  |  111xxxxx   | 0xe0 - 0xff
  ;; 
  ;; nil              |  11000000   | 0xc0
  ;; (never used)     |  11000001   | 0xc1
  ;; false 	      |	 11000010   | 0xc2
  ;; true 	      |	 11000011   | 0xc3
  ;; 
  ;; bin 8 	      |	 11000100   | 0xc4
  ;; bin 16 	      |	 11000101   | 0xc5
  ;; bin 32 	      |	 11000110   | 0xc6
  ;; ext 8 	      |	 11000111   | 0xc7
  ;; ext 16 	      |	 11001000   | 0xc8
  ;; ext 32 	      |	 11001001   | 0xc9
  ;;
  ;; float 32 	      |	 11001010   | 0xca
  ;; float 64 	      |	 11001011   | 0xcb
  ;;
  ;; uint 8 	      |	 11001100   | 0xcc
  ;; uint 16 	      |	 11001101   | 0xcd
  ;; uint 32 	      |	 11001110   | 0xce
  ;; uint 64 	      |	 11001111   | 0xcf
  ;; int 8 	      |	 11010000   | 0xd0
  ;; int 16 	      |	 11010001   | 0xd1
  ;; int 32 	      |	 11010010   | 0xd2
  ;; int 64 	      |	 11010011   | 0xd3
  ;;
  ;; fixext 1 	      |	 11010100   | 0xd4
  ;; fixext 2 	      |	 11010101   | 0xd5
  ;; fixext 4 	      |	 11010110   | 0xd6
  ;; fixext 8 	      |	 11010111   | 0xd7
  ;; fixext 16 	      |	 11011000   | 0xd8
  ;;
  ;; str 8 	      |	 11011001   | 0xd9
  ;; str 16 	      |	 11011010   | 0xda
  ;; str 32 	      |	 11011011   | 0xdb
  ;;
  ;; array 16 	      |	 11011100   | 0xdc
  ;; array 32 	      |	 11011101   | 0xdd
  ;; map 16 	      |	 11011110   | 0xde
  ;; map 32 	      |	 11011111   | 0xdf

  (define *pack-table* '())
  (define *unpack-table* (make-eqv-hashtable))
  (define (add-pack-table! pred proc)
    ;; the last in will be used if there is the same pred
    (set! *pack-table* (cons (cons pred proc) *pack-table*)))
  (define-syntax define-packer
    (syntax-rules ()
      ((_ (pred . args) body ...)
       (define-packer pred (lambda args body ...)))
      ((_ pred proc)
       (add-pack-table! pred proc))))

  (define-syntax define-unpacker
    (syntax-rules (unpack)
      ((_ tag (unpack args body ...))
       (define-unpacker tag (lambda args body ...)))
      ((_ tag proc)
       (hashtable-set! *unpack-table* tag proc))))


  ;; map and array header writer
  (define-syntax define-header-writer
    (syntax-rules ()
      ((_ name bv offset small-tag mid-tag large-tag)
       (define-header-writer name bv offset #f small-tag mid-tag large-tag))
      ((_ name bv offset fx-tag small-tag mid-tag large-tag)
       (define-header-writer name bv offset 
	 fx-tag small-tag mid-tag large-tag 15))
      ((_ name bv offset fx-tag small-tag mid-tag large-tag fx-len)
       (define (name n)
	 (cond ((and fx-tag (<= n fx-len))
		(let ((tag (bitwise-ior fx-tag n)))
		  (when bv
		    (bytevector-u8-set! bv offset tag))
		  (+ offset 1)))
	       ((and small-tag (<= n $2^8))
		(when bv
		  (bytevector-u8-set! bv offset small-tag)
		  (bytevector-u8-set! bv (+ offset 1) n))
		(+ offset 2))
	       ((<= n $2^16)
		(when bv
		  (bytevector-u8-set! bv offset mid-tag)
		  (bytevector-u16-set! bv (+ offset 1) n (endianness big)))
		(+ offset 3))
	       ((<= n $2^32)
		(when bv
		  (bytevector-u8-set! bv offset large-tag)
		  (bytevector-u32-set! bv (+ offset 1) n (endianness big)))
		(+ offset 5)))))))

  ;; helpers
  ;; integer packer helpers
  (define (pack-uint! bv message offset)
    (cond ((<= message #xFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCC)
	     (bytevector-u8-set! bv (+ offset 1) message))
	   (+ offset 2))
	  ((<= message #xFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCD)
	     (bytevector-u16-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 3))
	  ((<= message #xFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCE)
	     (bytevector-u32-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 5))
	  ((<= message #xFFFFFFFFFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCF)
	     (bytevector-u64-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 9))
	  (else
	   (error 'pack-uint! "given value is too big" message))))

  (define (pack-sint! bv message offset)
    (cond ((<= #x-80 message #x7F)
	   (when bv
	     (bytevector-u8-set! bv offset #xD0)
	     (bytevector-s8-set! bv (+ offset 1) message))
	   (+ offset 2))
	  ((<= #x-8000 message #x7FFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD1)
	     (bytevector-s16-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 3))
	  ((<= #x-80000000 message #x7FFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD2)
	     (bytevector-s32-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 5))
	  ((<= #x-8000000000000000 message #x7FFFFFFFFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD3)
	     (bytevector-s64-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 9))
	  (else
	   (error 'pack-sint! "given value is out of range" message))))


  (define (pack!* bv message offset)
    (do ((slot *pack-table* (cdr slot)))
	((or (null? slot) ((caar slot) message))
	 (if (null? slot)
	     (error 'pack "the message is not supported" message)
	     ((cdar slot) bv message offset)))))

  (define pack! 
    (case-lambda
     ((bv message) (pack! bv message 0))
     ((bv message offset) (pack!* bv message offset))))

  (define (pack-size message) (pack!* #f message 0))

  (define (pack message)
    (let ((bv (make-bytevector (pack-size message))))
      (pack! bv message 0)
      bv))

  ;; unpack
  (define (unpack-map bv offset count) 
    (let loop ((i 0)
	       (offset offset)
	       (r '()))
      (if (= i count)
	  (values offset (reverse r))
	  (let*-values (((key-off key) (unpack* bv offset))
			((value-off value) (unpack* bv key-off)))
	    (loop (+ i 1) value-off (cons (cons key value) r))))))

  (define (unpack-array bv offset count)
    (let ((v (make-vector count)))
      (let loop ((i 0) (offset offset))
	(if (= i count)
	    (values offset v)
	    (let-values (((off o) (unpack* bv offset)))
	      (vector-set! v i o)
	      (loop (+ i 1) off))))))

  (define (unpack-raw bv offset count)
    ;; For R6RS compatible, we can't use extended bytevector-copy.
    (let ((o (make-bytevector count)))
      (bytevector-copy! bv offset o 0 count)
      (values (+ offset count) (utf8->string o))))

  (define (unpack-fix-collection bv first offset)
    (case (bitwise-and first #x30)
      ((0)  (unpack-map bv (+ offset 1) (bitwise-and first #x0F)))
      ((16) (unpack-array bv (+ offset 1) (bitwise-and first #x0F)))
      (else (unpack-raw bv (+ offset 1) (bitwise-and first #x1F)))))

  (define (integer-ref indicator uint?)
    (case indicator
      ((1) (values 2 (if uint? bytevector-u16-ref bytevector-s16-ref)))
      ((2) (values 4 (if uint? bytevector-u32-ref bytevector-s32-ref)))
      ((3) (values 8 (if uint? bytevector-u64-ref bytevector-s64-ref)))
      (else (error 'integer-ref "invalid indicator"))))

  (define (unpack-uint bv type offset)
    (if (= type #xCC)
	(values (+ offset 1) (bytevector-u8-ref bv offset))
	(let-values (((off ref) (integer-ref (bitwise-and type #x03) #t)))
	  (values (+ offset off) (ref bv offset (endianness big))))))

  (define (unpack-sint bv type offset)
    (if (= type #xD0)
	(values (+ offset 1) (bytevector-s8-ref bv offset))
	(let-values (((off ref) (integer-ref (bitwise-and type #x03) #f)))
	  (values (+ offset off) (ref bv offset (endianness big))))))

  (define (collection-ref short?)
    (if short?
	(values 3 bytevector-u16-ref)
	(values 5 bytevector-u32-ref)))

  (define (unpack* bv offset)
    (let ((type (bytevector-u8-ref bv offset)))
      (cond ((= #xC0 type) (values (+ offset 1) '()))
	    ((= #xC2 type) (values (+ offset 1) #f))
	    ((= #xC3 type) (values (+ offset 1) #t))
	    ((zero? (bitwise-and type #x80))  ;; positive fixnum
	     (values (+ offset 1) (bitwise-and type #x7FF)))
	    ((= #xE0 (bitwise-and type #xE0)) ;; negative fixnum
	     (values (+ offset 1) (- (bitwise-and type #x1F) 32)))
	    ((= #x80 (bitwise-and type #xC0)) ;; fix map, array or raw
	     (unpack-fix-collection bv type offset))
	    ((= #x32 (ash type -2)) ;; flonum
	     (if (even? type)
		 (values
		  (+ offset 5)
		  (bytevector-ieee-single-ref bv (+ offset 1) (endianness big)))
		 (values
		  (+ offset 9)
		  (bytevector-ieee-double-ref bv (+ offset 1)
					      (endianness big)))))
	    ((= #x33 (ash type -2)) ;; unsigned int
	     (unpack-uint bv type (+ offset 1)))
	    ((= #x34 (ash type -2)) ;; signed int
	     (unpack-sint bv type (+ offset 1)))
	    ((= #b110110 (ash type -2)) ;; raw 16 or 32
	     (let-values (((off ref) (collection-ref (even? type))))
	       (unpack-raw bv (+ offset off)
			   (ref bv (+ offset 1) (endianness big)))))
	    ((= #b1101110 (ash type -1)) ;; array 16 or 32
	     (let-values (((off ref) (collection-ref (even? type))))
	       (unpack-array bv (+ offset off)
			     (ref bv (+ offset 1) (endianness big)))))
	    ((= #b1101111 (ash type -1)) ;; map 16 or 32
	     (let-values (((off ref) (collection-ref (even? type))))
	       (unpack-map bv (+ offset off)
			   (ref bv (+ offset 1) (endianness big)))))
	    (else (error 'unpack "unknown tag" type bv)))))


  (define unpack
    (case-lambda
     ((bv) (unpack bv 0))
     ((bv offset)
      (let-values (((_ o) (unpack* bv offset)))
	  o))))

  ;; packers R6RS doesn't allow to put this in between so the bottom
  (define-packer (null? bv message offset)
    (when bv (bytevector-u8-set! bv offset #xC0))
    (+ offset 1))

  (define-packer (boolean? bv message offset)
    (when bv (bytevector-u8-set! bv offset (if message #xC3 #xC2)))
    (+ offset 1))

  ;; bin 8 16 32
  ;; these doesn't have fx*** format
  (define-packer (bytevector? bv message offset)
    (define-header-writer write-header! bv offset #xC4 #xC5 #xC6)
    (let* ((len (bytevector-length message))
	   (new-offset (write-header! len)))
      (when bv
	(bytevector-copy! message 0 bv new-offset len))
      (+ new-offset len)))
  ;; ext must be done by user (user extension right?)
  
  ;; float 32/ float 64 (we don't support float 32 though)
  (define-packer (flonum? bv message offset)
    ;; we use only double, so floating points are always big...
    ;; FIXME Is there a way to determine if the message is float or double?
    (when bv
      (bytevector-u8-set! bv offset #xCB)
      (bytevector-ieee-double-set! bv (+ offset 1) message (endianness big)))
    (+ offset 9))

  (define-packer (integer? bv message offset)
    (cond ((<= 0 message 127)
	   (when bv
	     (bytevector-u8-set! bv offset message))
	   (+ offset 1))
	  ((<= -32 message -1)
	   (when bv
	     (bytevector-u8-set! bv offset 
				 (bitwise-ior #b11100000 (abs message))))
	   (+ offset 1))
	  ((positive? message) (pack-uint! bv message offset))
	  ((negative? message) (pack-sint! bv message offset))
	  (else (error 'pack-integer! "should not be here!" message))))

  (define-packer (string? bv message offset)
    (define-header-writer write-header! bv offset #b10100000 #xD9 #xDA #xDB 31)
    (let* ((message (string->utf8 message))
	   (msg-len (bytevector-length message))
	   (new-offset (write-header! msg-len)))
      (when bv
	(bytevector-copy! message 0 bv new-offset msg-len))
      (+ new-offset msg-len)))


  (define-packer (pair? bv message offset)
    (define-header-writer write-header! bv offset #b10000000 #f #xDE #xDF)
    (let ((new-offset (write-header! (length message))))
      (let loop ((message message)
		 (offset new-offset))
	(cond ((null? message) offset)
	      ((pair? (car message))
	       ;; should we reject if the key is pair/vector?
	       (let* ((key-off (pack!* bv (caar message) offset))
		      (value-off (pack!* bv (cdar message) key-off)))
		 (loop (cdr message) value-off)))
	      (else
	       (error 'pack-map! "alist is required" message))))))

  (define-packer (vector? bv message offset)
    (define-header-writer write-header! bv offset #b10010000 #xDC #xDD)
    (let* ((msg-len (vector-length message))
	   (new-offset (write-header! msg-len)))
      (let loop ((i 0) (offset new-offset))
	(if (= i msg-len)
	    offset
	    (loop (+ i 1) (pack!* bv (vector-ref message i) offset))))))

)