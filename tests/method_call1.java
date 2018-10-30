public class MethodTest {

    public void ok() {
        // Thing a = new Thing();
        a.ok(a.doStuff());
        b.doStuff();
       
        // for the funcall to match, need RegularName (doStuff ==  RecordAccess (x, doStuff)
    }

    public void ok() {
        // Thing a = new Thing();
        a.ok(a.doStuff());
    
        // for the funcall to match, need RegularName (doStuff ==  RecordAccess (x, doStuff)
    }

}
