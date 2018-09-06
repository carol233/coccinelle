
public class TestAutoCloseable {

    private static final String FILE_NAME = "/tmp/ok.txt";
 
    public static void main(String... args) {
        FileInputStream fs;
        try (fs = new FileInputStream (new File(FILE_NAME))) {
            System.out.Println("it worked");
        }
        

    }

}

