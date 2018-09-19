
class Tryy {
    public static void tryyy() {
        File f;
        int j;
        try {
            f = openFile();
            j = 10;
            t(j);
            return;
        } catch (IOException e) {
            println("That went badly");

        } finally {
            f.close();
        }

        int i = 5;
        doThing(f, 5);
    }
}
