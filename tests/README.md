Java-coccinelle design decisions
================================

* Handling Exceptions
** will have to permit users to specify patterns in the try block and catch block
** For the time being, can treat try as an `if` 
** maybe exceptions can be treated as deadcode?
** For unsafe transformations, emit warnings

* Generics
** -c++ option handles this somewhat
*** From  some early tests, either I am confused or this doesn't handle generics/templates that well (?)


* Fields
** Just ignore, since coccinelle will only reason about intraprocedural flow
** just treat non-local variable declarations as fields

* Try to make it modular
** Support for parsing byte-code, or Jimple, amy be added in future through extensible modules instead. Maybe through command line options


* `import static java.lang.Math.sqrt;` then doing `sqrt(9);` vs `Math.sqrt(9);` vs `java.lang.Math.sqrt(9);` 
** may need to make this configurable as well.
*** users can decide if sqrt == Math.sqrt == java.lang.Math.sqrt, or treat them separately.

* Do not fail when failing to parse unsupported syntax
** just emit warnings, ignore the unsupported parts
