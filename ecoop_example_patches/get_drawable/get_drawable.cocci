@@
expression E, p;
identifier f, ctx;
parameter list ps, ps2;
@@

f(..., Context ctx, ...) {
...
- E.getDrawable(p)
+ E.getDrawable(p, ctx.getTheme())
...
}

@@
expression E, p;
@@
- E.getDrawable(p)
// here we assume that most call sites of getDrawable are within activities
+ E.getDrawable(p, getContext()
+                  .getTheme())
