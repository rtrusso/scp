# Project Status

The compiler platform is currently able to bootstrap on top of an
interpreter, and the result of the bootstrap process is able to
compile itself.

The backend is capable of generating x86 and MIPS assembly source, a
separate tool is used to assemble that source to object code.

## Implementation Language Choice

The project started out in R5RS Scheme, in large part due to
inspiration from Wizard Book, and the author was determined to
bootstrap the compiler in that language. Scheme is a useful
programming language but has a very small community by modern
standards, which might limit the overall interest in the project.

The plan is to adapt more of the components of the project over to a
more mainstream language, however keep the Scheme frontend of the
compiler written in Scheme. JavaScript seems like an ideal choice
given many language similarities with respect to Scheme and the
amazing community and tools that have been built up around JavaScript
over the years.

## R5RS Scheme Implementation

The Scheme implementation provided by the compiler and associated
runtime library is not trivial, however it does not provide a complete
implementation of R5RS. Notably it is lacking a number tower and
support for continuations.

Continuation support was planned and there is a continuation-passing
style transform module partially implemented in the compiler. However
this was not necessary for bootstrap so left for a later project.

Similarly the number tower work was not necessary for bootstrap and
left for a later project.

The Scheme frontend is fairly simplistic, and a lot of work needs to
be done to make the output of the Scheme frontend more efficient. In
particular, eliminating environment allocation would be a big
improvement. In general, many of the ideas of the Rabbit scheme
compiler were planned to be implemented but are not done for the
initial release.

## Runtime Library

The memory management heap implementation needs some work to allow the
heap to grow and shrink. Currently it can be configured at compile
time to be large or small depending on the expected working set size.

The garbage collector is conservative and very simple, it uses a
mark-and-sweep algorithm. Pointer reversal was planned but not yet
implemented. Pointer reversal is needed to make the system reliable,
when working on large working sets it is not hard to blow the stack
and crash the executables during GC.

The Scheme runtime library needs various improvements, for example it
should support some bit twiddling to distinguish integers from
pointers at runtime, so that not all integers need to be boxed as
objects. Another planned improvement is to represent Scheme objects in
the same format as that expected by the Java frontend so some runtime
library code may be reused and shared.

## Tools

The phases of the compiler are all separate tools, currently these
phases are not wrapped up into a convenient command-line tool
("driver" is the archaic term among compiler developers for such a
tool such as cl.exe). So one has to script the phases of the compiler
by hand which is a nuisance. A more convenient tool wrapping the
phases was planned but not required for bootstrap.

