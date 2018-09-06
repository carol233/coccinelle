package mining;

public class Scratch {
    public static void OK() {
		
        final class DoesThisWork {
            void print() {
                System.out.println("It does");
            }
        }
		
        new DoesThisWork().print();
    }
	
    public static void main(String... args) {
        OK();
    }
}
