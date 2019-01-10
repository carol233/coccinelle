@@
expression E;
expression p;
@@
- E.getDrawable(p)
+ E.getDrawable(p, getContext()
+                 .getTheme())