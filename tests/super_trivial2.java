package thing;

import static java.lang.Math.sqrt;
import java.util.ArrayList;

@SuppressWarnings("some_warning")
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Target({ElementType.TYPE})
public class Ahhh {

    @OverrideX(X = true, Y="ok")
    @OverrideY(A = 5, Y="ok")
    @Deprecated
    @Target({ElementType.TYPE,ElementType.METHOD,
                ElementType.CONSTRUCTOR,ElementType.ANNOTATION_TYPE,
                ElementType.PACKAGE,ElementType.FIELD,ElementType.LOCAL_VARIABLE})
    Object main1(String[] args) {
       super();
        int i = 5;
        f(i);
        int j = 0;
        g(i);
    }

    void main(@SuppressWarnings("abc")  String[] args) {
       super.main(args);
    int i = 5;
	  f(i);
	  int j = 0;
	  g(i);
	}

}
