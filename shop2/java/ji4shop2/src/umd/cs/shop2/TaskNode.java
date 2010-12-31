package umd.cs.shop2;
import java.util.*;

/**
 * An element of the {@link Shop#tree tree} returned by SHOP2.
 *
 * @author murdock
 * @version 0.1, 2002-10-04
 */
public class TaskNode
{
    /**
     * The main content of the node.  In a complete tree, the atom 
     * {@linkplain TaskAtom#isPrimitive is primitive} if and only if
     * this node has no {@link #children}.
     */
    TaskAtom atom;

    /**
     * TaskNode elements that are one level down in the tree.
     */
    Vector children;

    /**
     * @param a The {@link #atom atom}
     */
    TaskNode( TaskAtom a)
    {
	atom = a;
        children = new Vector();
    }
    
    /**
     * @param ca A new node for {@link #children children}
     */
    void addChild ( TaskNode ca )
    {
	children.add(ca);
    }

    /**
     * @return The {@link #atom atom}
     */
    public TaskAtom atom()
    {
        return atom;
    }
    
    /**
     * @return The {@link #children children}
     */
    public Vector children()
    {
        return children;
    }

    /**
     * @return A formatted version of the node, including all descendants.
     */
    public String toString()
    {
	return toStringSpaced(0);
    }

    /**
     * A helper for {@link #toString() toString()} that recursively
     * steps through the children of the node.
     * @param spaces Number of spaces to indent
     * @return A formatted version of the node, including all descendants.
     */
    private String toStringSpaced(int spaces)
    {
	String retval = "";
	double c;
        Enumeration e = children.elements();

	for ( int i=0; i<spaces; i++)
	    {
		retval = retval + " ";
	    }

	retval = retval + "[" + atom.taskString();

	if (atom.isPrimitive())
	    {
		c = atom.cost();
		retval = retval + " " + Double.toString(c);
	    }

        while(e.hasMoreElements())
            {
                TaskNode n;
                n = (TaskNode)e.nextElement();
                retval = retval + "\n " + n.toStringSpaced(spaces+1);
            }

	retval = retval+"]";
	return retval;
    }


    /**
     * Extracts the atoms of any of this node and its descendants that
     * are primitive and orders them according to their {@link
     * TaskAtom#position position}.  If this method is invoked for the
     * root of a plan tree, it will return the plan produced by SHOP2.
     * @return A vector containing the {@link TaskAtom TaskAtom}
     * elements in order.
     */
    public Vector plan()
    {
	Vector partialPlan = new Vector();
	fillPlan(partialPlan);
	return partialPlan;
    }

    /**
     * Helper for {@link #plan()}
     * @param partialPlan The plan, as it is being built
     */
    private void fillPlan(Vector partialPlan) {
	int pos;
        Enumeration e = children.elements();

	if (atom.isPrimitive()) {
	    pos = atom.position();
	    if (pos >= partialPlan.size())
		partialPlan.setSize(pos+1);
	    partialPlan.setElementAt(atom,atom.position());
	}

        while(e.hasMoreElements())
            {
                TaskNode n;
                n = (TaskNode)e.nextElement();
		n.fillPlan(partialPlan);
            }
    }
}
