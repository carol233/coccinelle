public class Switch {

    public void switchcase() {

        int month = 8;
        String monthString;
        switch (month) {
        case 1:
            monthString = "January";
            break;
        case 2:
            monthString = "February";
            return;
        default:
            monthString = "Invalid month";
            break;

        }
        System.out.println(monthString);
    }
}
