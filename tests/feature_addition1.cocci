@find@
identifier i; statement S1; expression E;
@@

i(<... AverageType ...>) {

...

  switch (...) {
     if (<... 
-       E == AverageType.MEDIAN 
+       E == AverageType.MEDIAN || E == AverageType.GEOMETRIC
      ...>) 
  }

...

}

@add@

@@


enum AverageType {
   ...
+  GEOMETRIC
}
