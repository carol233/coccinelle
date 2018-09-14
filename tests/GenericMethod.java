public class GenericMethods {
    public static <T> identity (T t) {

        return T;
    }

    public static <T> List<T> makeEmptyList() {

        return new ArrayList<T>();
    }

    public void run() {

        List<Integer> ints = GenericMethods.<Integer>makeEmptyList();
        List<Integer> ints2 = makeEmptyList(); //type inference
    }
    

}
