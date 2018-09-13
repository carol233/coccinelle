class Ahhh {
    public static void hello(int iii) {
        System.out.println("hello");
        int j = 0;
        if (true) {
            f(iii);
        } else {
            f(j);
            // comments
        }
        // this should be changed
        OtherClass.g(iii);
    }
    public static void main(String[] args) {
        int i = 5;
        f(i);
        int j = 0;
        OtherClass.g(i);

    }


}
