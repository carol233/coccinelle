class Ahhh {
    public static void hello() {
        System.out.println("hello");
        int j = 0;
        try {
            abc(j);
            f(j);
        } catch (Exception e) {
            z(j);
        }
        OtherClass.g(j);
    }
    
    public static void hello1() {
        System.out.println("hello");
        int j = 0;
        try {
            abc(j);
            f(j);
        } catch (Exception e) {
            f(j);
        }
        OtherClass.g(j);
    }

    public static void hello2() {
        System.out.println("hello");
        int j = 0;
        try {
            abc(j);
           
        } catch (Exception e) {
            q(j);
        } finally {
            f(j);
        }
        OtherClass.g(j);
    }

    public static void hello3() {
        System.out.println("hello");
        int j = 0;
        try {
            abc(j);
            f(j);
        } catch (Exception e) {
            f(j);
        } finally {
            ok(j);
        }
        OtherClass.g(j);
    }

    public static void main(String[] args) {
        int i = 5;
        f(i);
        int j = 0;
        OtherClass.g(i);
    }


}
