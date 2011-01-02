import umd.cs.shop2.*;

public class MyQuery implements QueryWatcher {
    SHOP2GUI obj;
    
    public MyQuery() {
        obj = new SHOP2GUI();
    }
    
    
    public String processQuery(String query) {
        String retval = "T";
        
        obj.processInput(query);        
       
	    return retval;
    }
}