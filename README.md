MessagePack for R6RS Scheme
========

This is an implementation of [MessagePack](http://msgpack.org/) for 
R6RS Scheme.


API references
--------

_Function_ (pack! bv message)   
_Function_ (pack! bv message offset)   

Pack *message* to message pack format bytevector and put it into the
*bv* destructively. Given *bv* must have enough length to hold the message.

Optional argument *offset* indicates where to start with, default is 0.  

_Function_ (pack message)   

The same as **pack!** but this one creates a new bytevector.

_Function_ (pack-size message)

Calculate the converted message size.

_Function_ (unpack bv)   
_Function_ (unpack bv offset)  

Unpack the given message format bytevector to Scheme object.

Optional argument *offset* indicates where to start with, default is 0.  

_Function_ (get-unpack in)

Unpack the given binary input port to Scheme object.

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

### Message pack to Scheme

The other way around is easy, it can simply restore the byte data to Scheme
object. Following describes the conversion rules;

Positive fixnum -> integer   
Negative fixnum -> integer   
uint8, uint16, uint32, uint64 -> integer   
sint8, sint16, sint32, sint64 -> integer   
Map -> alist   
Array -> vector   
fixstr, str8, str16, str32 -> string
bit8, bit16, bit32 -> bytevector

Tested Scheme implementations
--------

[Sagittarius Scheme](http://code.google.com/p/sagittarius-scheme/)

[Mosh Scheme](http://code.google.com/p/mosh-scheme/)

[Ypsilon](http://code.google.com/p/ypsilon/)

Your contributions are always welcome.

TODO
--------

* More testing
* Extended type handling