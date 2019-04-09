@@
identifier p1, p2, p3;
@@
- onConsoleMessage(String p1, int p2, String p3) {
+ onConsoleMessage(ConsoleMessage cs) {
<...
(
- p1
+ cs.message ()
|
- p2
+ cs.lineNumber ()
|
- p3
+ cs.sourceId ()
)
...>
}