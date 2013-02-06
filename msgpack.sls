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

#!r6rs
(library (msgpack)
    (export pack! pack pack-size
	    #;unpack
	    )
    (import (for (rnrs) run expand)
	    (for (rnrs eval) expand)
	    ;; Ypsilon doesn't allow this style
	    ;; (srfi :39 parameters)
	    (srfi :39))
  ;; for convenience
  (define-syntax let-optionals*
    (syntax-rules ()
      ((let-optionals* arg (opt-clause ...) body ...)
       (let ((rest arg))
         (%let-optionals* rest (opt-clause ...) body ...)))))

  (define-syntax %let-optionals*
    (syntax-rules ()
      ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
       (call-with-values (lambda () (xparser arg))
         (lambda (rest var ...)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default) opt-clause ...) body ...)
       (call-with-values (lambda () (if (null? arg) (values default '())
                                        (values (car arg) (cdr arg))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
       (call-with-values (lambda ()
                           (if (null? arg) (values default '())
                               (let ((var (car arg)))
                                 (if test (values var (cdr arg))
                                     (error "arg failed LET-OPT test" var)))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default test supplied?) opt-clause ...)
			body ...)
       (call-with-values (lambda ()
                           (if (null? arg) (values default #f '())
                               (let ((var (car arg)))
                                 (if test (values var #t (cdr arg))
                                     (error "arg failed LET-OPT test" var)))))
         (lambda (var supplied? rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg (rest) body ...)
       (let ((rest arg)) body ...))

      ((%let-optionals* arg () body ...)
       (if (null? arg) (begin body ...)
           (error "Too many arguments in let-opt" arg)))))


  ;; internal parameters
  ;; for pack
  (define *string->message* (make-parameter string->utf8))

  ;; for unpack
  (define *message->string* (make-parameter utf8->string))

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

  (define-constant $2^16 (- (expt 2 16) 1))
  (define-constant $2^32 (- (expt 2 32) 1))

  ;; map and array header writer
  (define-syntax define-header-writer
    (syntax-rules ()
      ((_ name bv offset small-tag mid-tag large-tag)
       (define-header-writer name bv offset small-tag mid-tag large-tag 15))
      ((_ name bv offset small-tag mid-tag large-tag small-len)
       (define (name n)
	 (cond ((<= n small-len)
		(let ((tag (bitwise-ior small-tag n)))
		  (when bv
		    (bytevector-u8-set! bv offset tag))
		  (+ offset 1)))
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

  (define (pack-map! bv message offset)
    (define-header-writer write-header! bv offset #b10000000 #xDE #xDF)
    (let ((new-offset (write-header! (length message))))
      (let loop ((message message)
		 (offset new-offset))
	(cond ((null? message) offset)
	      ((pair? (car message))
	       (let* ((key-off (pack-atom! bv (caar message) offset))
		      (value-off (pack!* bv (cdar message) key-off)))
		 (loop (cdr message) value-off)))
	      (else
	       (error 'pack-map! "alist is required" message))))))

  (define (pack-array! bv message offset)
    (define-header-writer write-header! bv offset #b10010000 #xDC #xDD)
    (let* ((msg-len (vector-length message))
	   (new-offset (write-header! msg-len)))
      (let loop ((i 0) (offset new-offset))
	(if (= i msg-len)
	    offset
	    (loop (+ i 1) (pack!* bv (vector-ref message i) offset))))))

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

  (define (pack-integer! bv message offset)
    ;; for now only fixnum
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

  (define (pack-real! bv message offset)
    ;; we use only double, so floating points are always big...
    ;; FIXME
    ;; Is there a way to determine if the message is float or doubl?
    (when bv
      (bytevector-u8-set! bv offset #xCB)
      (bytevector-ieee-double-set! bv (+ offset 1) message (endianness big)))
    (+ offset 9))

  (define (pack-raw-bytes! bv message offset)
    (define-header-writer write-header! bv offset #b10100000 #xDA #xDB 31)
    (let* ((msg-len (bytevector-length message))
	   (new-offset (write-header! msg-len)))
      (when bv
	(bytevector-copy! message 0 bv new-offset msg-len))
      (+ new-offset msg-len)))

  (define (pack-atom! bv message offset)
    (cond ((flonum? message)
	   (pack-real! bv message offset))
	  ((integer? message)
	   (pack-integer! bv message offset))
	  ((boolean? message)
	   (when bv (bytevector-u8-set! bv offset (if message #xC3 #xC2)))
	   (+ offset 1))
	  ((null? message)
	   (when bv (bytevector-u8-set! bv offset #xC0))
	   (+ offset 1))
	  ((string? message)
	   (pack-atom! bv ((*string->message*) message) offset))
	  ((bytevector? message)
	   (pack-raw-bytes! bv message offset))
	  (else
	   (error 'pack-atom! "not supported yet" message))))

  (define (pack!* bv message offset)
    (cond ((pair? message) (pack-map! bv message offset))
	  ((vector? message) (pack-array! bv message offset))
	  (else (pack-atom! bv message offset))))

  (define (pack! bv message . opt)
    (let-optionals* opt ((offset 0)
			 (string->message string->utf8))
      (parameterize ((*string->message* string->message))
	(pack!* bv message offset))))

  (define (pack-size message . opt)
    (parameterize ((*string->message* (if (null? opt) string->utf8 (car opt))))
      (pack!* #f message 0)))

  (define (pack message . opt)
    (let ((bv (make-bytevector (apply pack-size message opt))))
      (apply pack! bv message opt)
      bv))

)