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
                Object main1(String args) {
        super();
        int i = 50;
        f(i);
        this.j = 50;
        g(i);

        gg(j);
        System.out.println(args);
    }

    class EnglishGreeting implements HelloWorld {
        String name = "world";
        public void greet() {
            greetSomeone("world");
        }
        public void greetSomeone(String someone) {
            name = someone;
            System.out.println("Hello " + name);
            Object B = new Thing();
            Object A = new Thing();
            boolean b = A.equals(B);
            boolean b1 = B.equals(A);
        }
    }
      

    void main(@SuppressWarnings("abc")  String... args) {
        super.main(args);
        f(i);
        int j = 0;
        g(i);

        k = 50;
        g(k);

        Object B = new Thing();
        Object A = new Thing();
        boolean b = A.equals(B);
        boolean b1 = B.equals(A);
        boolean b2 = A.equals(A);


        System.out.println(args);
    }

}
