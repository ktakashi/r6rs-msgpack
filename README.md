MessagePack for R6RS Scheme
========

This is an implementation of [MessagePack](http://msgpack.org/) for 
R6RS Scheme.


API references
--------

_Function_ (pack! bv message)   
_Function_ (pack! bv message offset)   
_Function_ (pack! bv message offset string->message)

Pack *message* to message pack format bytevector and put it into the
*bv* destructively. Given *bv* must have enough length to hold the message.

Optional argument *offset* indicates where to start with, default is 0.  
Optional argument *string->message* specify how to convert string object
to message, default is **string->utf8**

_Function_ (pack message)   
_Function_ (pack message string->message)

The same as **pack!** but this one creates a new bytevector.

_Function_ (pack-size message)

Calculate the converted message size.

_Function_ (unpack bv)   
_Function_ (unpack bv offset)  
_Function_ (unpack bv offset message->string)

Unpack the given message format bytevector to Scheme object.

Optional argument *offset* indicates where to start with, default is 0.  
Optional argument *message->string* specify how to convert raw bytes to
Scheme object, default is **utf8->string**.


Conversion rules
--------

As you already know, Scheme doesn't have static types so the conversion of
Scheme objects to message pack data might cause unexpected results. To avoid
it, I will describe how conversion works.

### Scheme to message pack

#### Integer conversion

The library automatically decides proper size. More specifically, if it
can fit to message pack's fixnum then library uses it, so are uint8-64.
If the number is too big, then an error is raised. Users must know it tries
to use uint as much as possible. If the given number was negative then
sint will be used.

#### Floating point conversion

Unfortunately R6RS doesn't have difference between float and double. So
when flonum is given then it always converts to double number.

#### Collection conversion

Message pack has collections which are map and array. And these are associated
with alist (association list) and vector respectively. When you want to convert
alist to message pack data, then you need to make sure the _cdr_ part will be
the data and if you put _(("key" "value))__ then it will be converted to nested
map.

The collection size calculation is done automatically. It tries to use the
smallest size.

#### Raw bytes conversion

The library is designed to use Scheme string as raw bytes. However in Scheme
string is not a byte array. So users need to be able to specify how to
convert it. If you see the __pack!__ or __pack__ reference these have 
optional argument _string->message_ and conversion uses it.

### Message pack to Scheme

The other way around is easy, it can simply restore the byte data to Scheme
object. Following describes the conversion rules;

Positive fixnum -> integer   
Negative fixnum -> integer   
uint8, uint16, uint32, uint64 -> integer   
sint8, sint16, sint32, sint64 -> integer   
Map -> alist   
Array -> vector   
Raw bytes -> string

If you want to get raw bytes as a bytevector then you can pass 
__(lambda (x) x)__ equivalent procedure to _message->string_ optional argument.


Tested Scheme implementations
--------

[Sagittarius Scheme](http://code.google.com/p/sagittarius-scheme/)

[Mosh Scheme](http://code.google.com/p/mosh-scheme/)

[Ypsilon](http://code.google.com/p/ypsilon/)


The library is written in R6RS and using SRFI-39 so other R6RS
implementation might be able to use this library as well.

Your contributions are always welcome.

TODO
--------

* More testing


