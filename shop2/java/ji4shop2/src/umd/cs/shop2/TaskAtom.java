package umd.cs.shop2;
import java.io.*;
import java.util.*;

/**
 * A task plus the arguments for that task.  Examples: (!put block1
 * block2), (move-tower (block1 block2 block3) block4).  Used as an
 * input to the {@linkplain Shop#Shop(String, State, TaskAtom) Shop
 * constructor for embedded use}).  Also used as the content of the
 * {@link TaskNode} elements of the {@link Shop#tree tree} returned by
 * SHOP2.
 */
public class TaskAtom
{
    /**
     * Text, e.g., "(move-tower (block1 block2 block3) block4)"
     * @see #toString()
     */
    String text;

    /**
     * An operator in an output task tree is labeled as primitive.
     * TaskAtoms used as inputs and all TaskAtoms that are not
     * operators are not labeled as primitive
     * @see #isPrimitive()
     */
    boolean isPrimitive;

    /**
     * Only used if {@link #isPrimitive} is true.  The operator cost
     * reported by SHOP2.
     * @see #cost()
     */
    double cost;

    /**
     * Only used if {@link #isPrimitive} is true.  The sequential
     * position (starting with 0) of this operator in the plan
     * returned by SHOP2.
     * @see #position()
     */
    int position;

    /**
     * Constructor for non-primitive TaskAtom
     * @param t The {@link #text text}
     */
    public TaskAtom( String t)
    {
	text = t;
	isPrimitive = false;
    }

    /**
     * Constructor for primitive TaskAtom
     * @param t The {@link #text text}
     * @param c The {@link #cost cost}
     * @param p The {@link #position position}
     */
    public TaskAtom( String t, double c, int p)
    {
	text = t;
	cost = c;
	position = p;
	isPrimitive = true;
    }

    /**
     * @return The {@link #text text}
     */
    public String taskString()
    {
	return text;
    }

    /**
     * @return The {@link #text text} and the cost (if any) in
     * brackets, e.g., "[(!put block1 block2) 1.0]", "[(move-tower
     * (block1 block2 block3) block4)]"
     */
    public String toString()
    {
	String retval = "[" + text;

	if (isPrimitive)
	    retval = retval + " " + Double.toString(cost);

	retval = retval + "]";

	return retval;
    }

    /**
     * @return The {@link #cost cost}
     */
    public double cost()
    {
	return cost;
    }

    /**
     * @return The {@link #position position}
     */
    public int position()
    {
	return position;
    }

    /**
     * @return The value of {@link #isPrimitive isPrimitive}
     */
    public boolean isPrimitive()
    {
        return isPrimitive;
    }
}

