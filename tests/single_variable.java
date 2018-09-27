public abstract class Single {
    public static void main() {


        wrap( new Thing() {
                @Override
                public int compare() {
                    int i = 5;
                    i = 6;
                    return 1;
                }
            });

        int i = 0;
        i = 5;

    }

}

