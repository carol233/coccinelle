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
        class InnerEnglishGreeting implements HelloWorld {
            String name = "world";
            public void greet() {
                greetSomeone("world");
            }
            public void greetSomeoneInnerEnglish(String someone) {
                name = someone;
                System.out.println("Hello " + name);
                Object B = new Thing();
                Thing A = new Thing();

                if (bla_boolean_h()) {
                    A.init();
                } else {
                    j = 0;
                }
                boolean b = A.equals(B);
                boolean b1 = B.equals(A);
            }
        }

        List<String> strings = new ArrayList<String>();
        strings.add("AA");
        strings.add("z");
        Collections.sort(strings, new Comparator<String>(){
                public int compare(String o1, String o2) {
                    int i;
                    if (o1.length() > 1) {
                        i = 10;
                    } else {
                        i = 100;
                    }
                    int j = o2.length();

                    if (i < j) {
                        return 1
                    }  if (i == j) {
                        return 0;
                    }
                    return  1;
                }
            });
        print(strings);
        
    }

    class EnglishGreeting implements HelloWorld {
        String name = "world";
        public void greet() {
            greetSomeone("world");
            int j = name.length();
            int k = "world".length();
        }
        public void greetSomeoneEng(String someone) {
            name = someone;
            System.out.println("Hello " + name);
            Object B = new Thing();
            Thing A = new Thing();
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
        Thing A = new Thing();
        boolean b = A.equals(B);
        boolean b1 = B.equals(A);
        boolean b2 = A.equals(A);


        System.out.println(args);
    }

}
