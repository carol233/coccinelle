package thing;

import static java.lang.Math.sqrt;
import java.util.ArrayList;

@SuppressWarnings("some_warning")
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Target({ElementType.TYPE})
public static class Ahhh {
    int q = 5;
    public static final THing t = new Thing("param0");

    static {

        q = 10;
    }

    @OverrideX(X = true, Y="ok")
    @OverrideY(A = 5, Y="ok")
    @Deprecated
    Object main1(String[] args) {
       super();
        int i = 5;
        f(i);
        int j = 0;
        g(i);
    }

    static void main(@SuppressWarnings("abc")   String[] args) {
       super.main(args);

       synchronized {
    int i = 5;
	  f(i);
	  int j = 0;
	  g(i);
    j = Constants.HELLO;
       }


       synchronized (Thing.class) {
           int i = 6;
           i = 7;
       }

	}

}
