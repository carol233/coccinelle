
public enum AverageType {
    MEAN, MEDIAN, MODE
}

public static float average(List<Float> numbers, AverageType s) {
      if (s  == AverageType.MEDIAN) {
          System.out.Println("Median");
      } else if (s == AverageType.MODE) {
          System.out.Println("Mode");
      } else {
          throw new UnsupportedOperationException("This kind of average is not supported yet");
      }

      

}

