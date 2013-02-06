MessagePack for R6RS Scheme
========

This is an implementation of [MessagePack](http://msgpack.org/) for 
R6RS Scheme.

API references
--------

[Function] (pack! bv message)   
[Function] (pack! bv message offset)   
[Function] (pack! bv message offset string->message)

Pack *message* to message pack format bytevector and put it into the
*bv* destructively. Given *bv* must have enough length to hold the message.

Optional argument *offset* indicates where to start with, default is 0.  
Optional argument *string->message* specify how to convert string object
to message, default is **string->utf8**

[Function] (pack message)   
[Function] (pack message string->message)

The same as **pack!** but this one creates a new bytevector.

[Function] (pack-size message)

Calculate the converted message size.

[Function] (unpack bv)   
[Function] (unpack bv offset)  
[Function] (unpack bv offset message->string)

Unpack the given message format bytevector to Scheme object.

Optional argument *offset* indicates where to start with, default is 0.  
Optional argument *message->string* specify how to convert raw bytes to
Scheme object, default is **utf8->string**.

Tested Scheme implementations
--------

[Sagittarius Scheme](http://code.google.com/p/sagittarius-scheme/)

[Mosh Scheme](http://code.google.com/p/mosh-scheme/)

[Ypsilon](http://code.google.com/p/ypsilon/)


The library is written in R6RS and using SRFI-39 so other R6RS
implementation might be able to use this library as well.


TODO
--------

* More testing


