class Ahhh {
    public static void hello() {
        System.out.println("hello");
        int j = 0;
        if (true) {
            f(j);
        } else {
            f(j);
            // comments
        }
        // this should be changed
        OtherClass.g(j);
    }
    public static void main(String[] args) {
        int i = 5;
        f(i);
        int j = 0;
        OtherClass.g(i);
    }


}
