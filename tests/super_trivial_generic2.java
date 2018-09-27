public class Generics {

    public static  void main(List<String> strings, T t) {
        int i  = 5;

        List<String> list = new ArrayList<String>();

        // ArrayList < HashMap< HashMap  <String,  Integer>, HashMap < String, Integer> >> list = new ArrayList < HashMap< HashMap  <String,  Integer>, HashMap < String, Integer> >> ();
              
        if (list.length() < strings.length()) {
            i  = 10;
        }
        Map<String, Object> weirdMap = new HashMap<>();
        Map<String, Map<String, Object> > weirdMap = new HashMap<>();


        Map<String, Map<String, Integer > > map = new HashMap<>();
        Map<Thing extends String, Map<String, Integer > > map = new HashMap<>();
        Map<Thing super String, Map<String, Integer > > map = new HashMap<>();
        Map<? super String, Map<String, Integer > > map = new HashMap<>();
        Map<? extends String, Map<String, Integer > > map = new HashMap<>();
        Map<String, Map<String, Integer>> map = new HashMap<>();

        weirdMap.containsKey("weirdKey");

        OtherThing.<Integer>makeEmptyList();

        String ok = "ok";
        System.out.println(ok);
    }


}
