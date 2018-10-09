@func_neg@
identifier i, f;
@@

i =
(
 f()
|
 f(...)
)
... when exists
 (i >= 0)


@forgot_to_check@
identifier func_neg.i, func_neg.f;
expression E;
statement S, S1;
@@

i =
(
 f()
|
 f(...)
)
...
if (i >= 0 && ...) 
S else {
...
* i
...
}

