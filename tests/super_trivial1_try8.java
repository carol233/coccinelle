
class Tryy {
    public static void tryyy() throws Exception {
        File f;
        int j;
        j = 10;
        try {
            f = openFile();
            if (!f.exists()) {
                throw new IOException();
            }
        } catch (IOException e) {
            println("That went badly");

        } finally {
            f.close();
        }
        t(j);

        int i = 5;
        doThing(f, 5);
    }
}
