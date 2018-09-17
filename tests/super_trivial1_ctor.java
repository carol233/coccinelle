
class Ctor<T> extends B {

    public T t;

    public Ctor(Integer i) {
        super(5);
        int j = 100;
        r(Integer.MIN_VALUE);
        s(Integer.MAX_VALUE);
        if (true) {
            f(Integer.MAX_VALUE);
        } else {
            f(j);
        }
        Thing t = new Thing();
        OtherClass.huh(Integer.MAX_VALUE);
        Thing thing = new Thing(OtherClass.g(t));
        Thing thing = new Thing(OtherClass.anotherThing(this.abc, 500));
        asd = Integer.MAX_VALUE;

        b = false;
    }

}
