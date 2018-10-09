
@func_neg@
identifier i, f;
type t;
@@

(
 t i = f();
|
 t i = f(...);
|
 i = f();
|
 t i = f(...);
)
... when exists
 (i >= 0)


@forgot_check exists@
identifier func_neg.i, func_neg.f;
expression E;
statement S, S1;
binary operator bop;
type t;
@@
(
 t i = f();
|
 t i = f(...);
|
 i = f();
|
 t i = f(...);
)
...
if (i >= 0 bop !E) 
S else {
... 
i
...
}

@add_branch @
identifier func_neg.i;
identifier func_neg.f;
statement S,S1;
type t;
expression forgot_check.E;
@@

(
+ if (E) {
+  add(x, y);
+  return null;
+ }
t i = f(...);
|
+ if (E) {
+  add(x, y);
+  return null;
+ }
i = f(...);
)
...
if
-  (i >=0 && !E) 
+  (i >= 0)
S else S1
...