// Finds field access of identifiers assigned to getRenderedForDataset() that did not check for null previously.


@first@
identifier i, f;
statement S,S1;
expression E;
@@
 i = getRendererForDataset(...)
...
 if (i != null) {
... when != return;
    when != return E;
 } else S1
...
* i.f



