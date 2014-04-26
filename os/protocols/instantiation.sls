#!r6rs
(library (os protocols instantiation)
  ;
  ;   Object allocation and instantiation protocol
  ;
  (export make
            allocate
            initialize )

    (import (os boot meta generics)) )
