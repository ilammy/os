#!r6rs
(library (os protocols instantiation)
  ;
  ;   Object allocation and instantiation protocol
  ;
  (export make
            allocate
            initialize
              initialize-with-initargs-in-class! )

    (import (os boot meta generics)) )
