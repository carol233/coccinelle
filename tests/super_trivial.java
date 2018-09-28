public class Run {

    @OverrideX(X = true, Y="ok")
 void Main(int i ) {
     super.main(args);
     int i = 0;
     i = 7;
     Main(i);
     synchronized (Thing) {
         int i = 6;
         i = 7;
    }
 }
}
