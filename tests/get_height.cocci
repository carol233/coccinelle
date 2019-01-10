
@rule1@
Display display;
identifier p;
type T;
@@

(
- p = new Point(..., display.getHeight());
+ p = new Point();
+ d.getSize(p);
|
- T p = new Point(..., display.getHeight());
+ T p = new Point();
+ d.getSize(p);
)
<...
(
- display.getHeight()
+ p.y
|
- display.getWidth()
+ p.x
)
...>

@rule2@
identifier display, f;
expression E;
@@
f(...) {
Display display = E;
+ Point p = new Point();
+ display.getSize(p);

<... when != Point(...)
(
- display.getHeight()
+ p.y
|
- display.getWidth()
+ p.x
)
...>
}