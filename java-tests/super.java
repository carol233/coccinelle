package hello;

public class Scratch {
    public static abstract class ClassA {
		
        public int publicVar = 5;
        private String privateVar = "not visible";
        protected String protectedVar = "visible";
		
        public String run() {
            System.out.println("HELLO");
            return privateVar;
        }
		
    }
	
    public static final class ClassB extends ClassA {
        public String run() {
            System.out.println("HEY");
            super.run();
            System.out.println("World");
            protectedVar = "edited";
            return protectedVar;
        }
    }
}
