class Tryy {


    public static void tryyy() {
        File f;
        int j;
        try {
            j = 5;
            f = openFile();

        } catch (IOException e) {
            j = 5;
            println("That went badly");

        } finally {
            f.close();
            return;
        }
        // deadcode
        int i = 5;
        doThing(f, 5);
        t(j);
    }


}
