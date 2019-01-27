;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GC Object Layouts
;;
;;  The garbage collector supports certain object layouts.  In the
;;  future, the GC can be expanded to support other in-memory formats.
;;  Until that time, we can utilize these formats to represent
;;  Scheme objects at runtime in memory.
;;
;;  object layout
;;  +-----+-----+-----+-
;;  | CP  | GCDW| M0  | ...
;;  +-----+-----+-----+-
;;  ^
;;
;;  CP == class descriptor layout
;;  +-----+-----+-----+-
;;  | NM  | F0  | F1  | ...
;;  +-----+-----+-----+-
;;
;;  array layout
;;  +-----+-----+-----+-
;;  | NW  | GCDW| IDX0| ...
;;  +-----+-----+-----+-
;;              ^
;;
;;  Notice that for a typical array object, the pointer is aliased and points
;;  at IDX0 instead of at the beginning of the array object.
;;
;;  ##### Scheme object layouts #####
;;
;;  Primitive - Similar to class instance
;;  +-----+-----+
;;  | DATA| TAG |...
;;  +-----+-----+
;;  ^
;;
;;  Integers and such are encoded in this form.  The pointer is not aliased
;;  and points directly to the data field.  The data field is 1 word and
;;  contains the actual payload of the object.  The GCDW, labeled TAG above,
;;  indicates to the GC code that this object has no fields to mark.
;;  Strings and symbols are actually encoded this way, but with trailing
;;  fields to store the additional data (this is the reason for the ellipsis
;;  in the diagram above -- most instances of this type are actually fixed to
;;  2 words).  This is a convenient representation for strings and symbols
;;  because they do not contain pointers and thus we wish for the GC to treat
;;  them as atomic.
;;
;;  Compound - Encoded as non-aliased arrays
;;  +-----+-----+-----+-
;;  | NW  | ATAG| IDX0| ...
;;  +-----+-----+-----+-
;;  ^
;;
;;  To the GC, this looks just like an array but with a non-aliased pointer.
;;  The ATAG is a normal GCDW, but with a DONE field set to a type tag
;;  value.  Pairs and vectors are encoded this way.
;;
;;  For the format of the GCDW (Garbage Collector Descriptor Word) see
;;  rtl/gc-defines.sasm.  There are several bits there we can exploit
;;  for representing Scheme objects.
;;
;;  The Scheme objects we need to support are:
;;   - int (in general, number tower in the future)
;;   - boolean
;;   - character
;;   - pair
;;   - vector
;;   - string
;;   - symbol
;;   - nil
;;   - eof
;;   - input-port
;;   - output-port
;;   - procedure

(define gc-array-mask -2147483648)

