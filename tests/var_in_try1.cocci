@@

identifier v;
type T;
expression E, E1;
@@

-T v;
...
-try (v = E) {
+try (T v = E) {
... when != v = E1
}

