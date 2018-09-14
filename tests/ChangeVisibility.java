public class Klass {
    
    
    // TODO need to find all external usages of this method
    public static String rev(String s) {
        return new StringBuffer(s).reverse().toString();
    }

    public static String doSomething() {
        rev("okay");
        System.out.println("...");
    }

}
