public class Exn {

    public static String branchOnException() {
        try {
            if (Math.sqrt(4) > 0) {
                throw new Exception("always")
            } 
            return "ok";
        } catch (Exception e) {
            OtherClass.Log("bad");

        }


    }

}
