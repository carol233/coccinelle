public class TestA{

    public static void dragQuarterScreenDown(InstrumentationTestCase test, Activity activity) {
        Display display = activity.getWindowManager().getDefaultDisplay();
        int screenHeight = display.getHeight();
        int screenWidth = display.getWidth();

        final float x = screenWidth / 2.0f;
        final float fromY = screenHeight * 0.5f;
        final float toY = screenHeight * 0.75f;

        drag(test, x, x, fromY, toY, 4);

    }
}
