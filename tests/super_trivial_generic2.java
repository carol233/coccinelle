public class Generics {

    public static void main(List<String> strings) {
        int i  = 5;

        List<String> list = new ArrayList<String>();
        List  list = new ArrayList <String> ();
        List <String>  list = new ArrayList <String> ();
        List<String> list2 = new ArrayList<>();
        if (list.length() < strings.length()) {
            i  = 10;
        }
        String ok = "ok";
        System.out.println(ok);
    }

}
