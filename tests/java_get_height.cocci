@rule1@
Display display;
identifier p;
type T;
@@
(
- p = new Point(display.getWidth (),
-                display.getHeight());
+ p = new Point ();
+ d.getSize(p);
|
- T p = new Point(display.getWidth (),
-                 display.getHeight ());
+ T p = new Point ();
+ d.getSize(p);
)

<...
(
- display.getHeight ()
+ p.y
|
- display.getWidth ()
+ p.x
)
...>

@rule2@
identifier display , f;
expression E;
@@
f(...) {
... when exists
Display display = E;
+ Point p = new Point ();
+ display.getSize(p);
<... when != Point (...)
(
- display.getHeight ()
+ p.y
|
- display.getWidth ()
+ p.x
)
...>
}