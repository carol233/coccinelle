@find@
identifier i; statement S1;
@@

i(<... AverageType ...>) {

...

  switch (...) {
+      case GEOMETRIC:
+             System.out.Println("Geometric")
       default:
              S1
  }

...

}

@add@

@@


enum AverageType {
   ...
+  GEOMETRIC
}


