

class Tryy {
    public static void tryyy() throws IOException {
        File f;
        int j;
        try {
            f = openFile();
            j = 10;
            return;
        } catch (IOException e) {
            println("That went badly");
            t(j);
        } finally {
            f.close();
        }

        int i = 5;
        doThing(f, 5);
        throw new IOException("omg");
    }
}
