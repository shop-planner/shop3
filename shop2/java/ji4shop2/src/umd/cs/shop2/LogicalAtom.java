package umd.cs.shop2;
import java.io.*;
import java.util.*;

/**
 * An individual assertion in SHOP2 syntax.  A typical example of the
 * text of a LogicalAtom is "(on block1 block2)".  The text may also
 * contain nested lists, e.g., "(route-explored ((p1 p2) (p2 p3) (p3
 * p1)))". <p>LogicalAtoms are used in JSHOP2 as elements of {@link
 * State State} obects (used as an input to the {@linkplain
 * Shop#Shop(String, State, TaskAtom) Shop constructor for embedded
 * use}).  In addition, LogicalAtoms are used as elements in the Shop
 * {@link Shop#addList addList} and {@link Shop#deleteList} fields.
 *
 * @author murdock
 * @version 0.1, 2002-10-04 */
public class LogicalAtom
{
    /**
     * The SHOP2 syntax text, e.g., "(on block1 block2)"
     * @see #toString()
     */
    String text;

    /**
     * @param t The value for {@link #text}
     */
    public LogicalAtom( String t)
    {
        text = t;
    }
    
    /**
     * @return The value of {@link #text}
     */
    public String toString()
    {
        return text;
    }
 }

