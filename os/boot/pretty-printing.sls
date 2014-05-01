#!r6rs
(library (os boot pretty-printing)
  ;
  ;   Generic specialization bootstrapping
  ;
  (export)

  (import (os internal primitives)
          (os internal pretty-printing) )

  (set-primitive-write-proc!   to-write)
  (set-primitive-display-proc! to-display) )
