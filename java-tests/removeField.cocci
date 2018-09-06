@@
class C;
identifier F;
@@

C {
- field2
+ public String getField2()  {
+     abc();
+}
}

F {
...
<...
- C.field2
+ C.getField2()
...>
...

}

