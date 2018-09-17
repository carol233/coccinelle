class Ahhh extends B {

    public T t;

    public static void hello() {
        System.out.println("hello");
        int j = 1__00;
        if (true) {
            f(j);
        } else {
            f(j);
            // comments
        }
        // static method call
        OtherClass.g(j);

        boolean b = true;
        b = false;
    }

    private int k;
    public static final Double CONSTANT_1 = 5;

    public static void main(String[] args) {
        // no arg constructor
        Thing obj = new Thing();
        int i = 5;
        f(i);
        int j = 0;
        OtherClass.g(i);

        i = OtherClass.staticValue;

        {

            i = OtherClass.STATIC_FINAL_VALUE;
        }

        // side-effect on a field
        k = 10;

        // constructor, with usage of enum
        Thing obj2 = new Thing(j, ThingEnum.VALUE_1);

        obj2.doSomething(ThingEnum2.VALUE_2);
        obj2.getValue();

        // using `this`,  set to constant from another class
        this.k = Integer.MAX_VALUE;

        
        if (this.k > CONSTANT_1) {
            System.out.println("true");

        } 

    }
}
