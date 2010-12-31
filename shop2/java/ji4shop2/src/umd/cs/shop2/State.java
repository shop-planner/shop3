package umd.cs.shop2;
import java.util.*;
import java.io.*;

/**
 * A collection of {@link LogicalAtom} elements used as an input to
 * the {@linkplain Shop#Shop(String, State, TaskAtom) Shop constructor
 * for embedded use}.
 *
 * @author murdock
 * @version 0.1, 2002-10-04 */
public class State extends Vector
{
    /**
     * @return A string representation, e.g., "((on b1 b2) (on b2 b3))"
     */
    public String toString() {
	String retval = "(";
	Enumeration e = elements();

	while(e.hasMoreElements())
	    {
		LogicalAtom a;
		a = (LogicalAtom)e.nextElement();
		retval = retval + "\n " + a.toString();
	    }

	retval = retval + ")";
	return retval;
      }
}
