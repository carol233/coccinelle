package mining;

public class RemoveField {
    public String field1;
    private String field2;

    public String doStuff() {
        return field2;
    }

    public String doStuff1() {
        int f = 1;
        System.out.println("ok");
		
        // stuff
        String ok = field1  + " 5";
        System.out.println(ok);
		
        return field1;
    }

    public String doStuff2() {
        int q = 1;
        System.out.println("ok");
		
        // stuff
        String ok = this.field1  + q;
        System.out.println(ok);
		
        return field1;
    }
	
    public class Scratch2 {
        RemoveField scratch1;
		
        public String doStuffA() {
            return scratch1.field1;
        }
		
    }
}
 
