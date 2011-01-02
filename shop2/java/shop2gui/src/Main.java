import umd.cs.shop2.*;
import java.util.*;

public class Main {
    static final String DOMAINFILE = "./sample/d1.lisp"; /* enter domain filename here */
    static final String GOAL = "(main-goal)"; /* enter goal task name here */
    static final String EXTRAFILE = "extraFile.lisp";

    public static void main(String[] args) {
    	State startState;
    	TaskAtom goalTask = new TaskAtom(GOAL); 
	    MyQuery query = new MyQuery();
    	String[] facts = {
    		/* enter state atoms here */  
	    "(bar)", 
	    "(baz)"
	    };

	    Shop.setVerbosity(0);
	    startState = makeState(facts);
	    Shop shopA = new Shop(DOMAINFILE, startState, goalTask, EXTRAFILE, 
			 "", query); 

	    Shop.kill();
    }


    static State makeState(String[] strings) {
	    State newState = new State();
	    for (int i = 0; i < strings.length; i++)
	     newState.addElement(new LogicalAtom(strings[i]));
	    return newState;
    }

}