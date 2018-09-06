public class Big {
    private Big(int i, int j, AnotherClass a) {
        System.out.Println("ctor");
    }

    public static Big getInstance() {
        // blah blah
        return new Big(0, 1, null);
    }

}
