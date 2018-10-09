// Finds field access of identifiers assigned to getRenderedForDataset() that did not check for null previously, but there exists a check for null for its return value in the same function/file.


@test1@
identifier i, f, a;
statement S,S1;
expression E;
position p,p1;
@@
 i = getRendererForDataset(...)
...
 if (i != null) {
... when != return;
    when != return E;
 } else S1
...
 a@p = i.f()


@second@
identifier test1.i, test1.f, test1.a;
statement S;
type t;
position test1.p;
expression E;
@@


(
+ if (i == null) continue;
  t a = i.f();
|
+ if (i == null) continue;
  a = i.f(...);
|
+ if (i == null) continue;
  t a = i.f();
|
+ if (i == null) continue;
  a = i.f();
)


