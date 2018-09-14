@@

identifier v;
type T;
expression E;
@@

-T v;
...
-try (v = E) {
+try (T v = E) {
...

