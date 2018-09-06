
public class Average {
    private static final int BIGGEST = 5;
    // TODO throw a CustomException if numbers.size, or write a more descriptive message for the error cases
    public static float average(List<Float> numbers) throws InvalidArgumentException {
        if (numbers.size() < 0) {
            throw new InvalidArgumentException();
        }
        if (numbers.size() > BIGGEST) {
            throw new InvalidArgumentException();
        }

        return 0.0;
    }

    public static void main(String[] args) {
        try {
            average(new ArrayList<>());
        } catch (InvalidArgumentException iae) {
            log.Error("invalid argument exception");
        } 

    }
}
