package umd.cs.shop2;
import java.util.*;
import java.io.*;
import umd.cs.javalisp.*;

/**
 * This is the main class for the Java interface to SHOP2.  The shop2
 * package enables the access of the SHOP2 hierarchical planning
 * system from within Java.  SHOP2 runs in Common Lisp; this package
 * provides code and data structures for running SHOP2 in an external
 * Lisp process and for accessing the results that SHOP2 produces.
 * <p>The Java interface to SHOP2 has two major uses: (1) it can be
 * run as a complete, stand-alone program; (2) it can be embedded in a
 * seperate Java program to allow that program to use SHOP2's planning
 * capabilities.
 *
 * @author murdock
 * @version 0.1, 2002-10-04
 */
public final class Shop

{
    /**
     * Name of file that holds configuration info (specifically
     * {@link #SHOP2FILE SHOP2FILE}).
     */
    static final String CONFIGFILE = "config.txt";

    /**
     * File name for the SHOP2 program.  The default value may be
     * overridden by the configuration file, because some people will
     * have SHOP2 installed in different locations.
     */
    public static String SHOP2FILE = "umd/cs/shop2/shop2";

    /**
     * File name for the Lisp code that acts as a bridge between SHOP2
     * and this Java interface.
     */
    public static final String JSHOP2FILE = "umd/cs/shop2/jshop2.lisp";

    /**
     * The primary result of the SHOP2 planning process: the plan
     * tree.  This tree encodes not only the operators of the final
     * plan (including the costs and ordering of the operators) but
     * also the higher level tasks that were decomposed into those
     * operators.
     */
    private TaskNode tree = null;

    /**
     * The list of atoms added to the state by the cumulative effect
     * of all of the operators in the the plan produced when the Shop
     * constructor was invoked (i.e., those atoms that are in the
     * final state of the plan but not the start state).
     */
    private Vector addList = new Vector();

    /**
     * The list of atoms deleted from the state by the cumulative
     * effect of all of the operators in the the plan produced when
     * the Shop constructor was invoked (i.e., those atoms that are in
     * the start state of the plan but not the final state).
     */
    private Vector deleteList = new Vector();


    /**
     * A number from 0 to 3 specifying how much information should be
     * provided to the user during planning.  If verbosity is greater
     * than 0, all text sent to or received from the Lisp process that
     * runs SHOP2 is displayed.  The verbosity number is also sent to
     * the Lisp process, which adjusts the amount of information it
     * sends based on the verbosity level (see the SHOP2 manual for
     * details).
     */
    private static int verbosity = 0;

    /**
     * The external Lisp process from which SHOP2 is run.  This field
     * is only used by the Shop object constructors, i.e., all access
     * to Lisp is done when the object is created and subsequent
     * accesses to the object merely invoke data that was stored in
     * the Java process while the constructor was executing.  <p>Note
     * that this field is static.  When a Shop constructor is
     * finished, it leaves the Lisp process running; if another Shop
     * object is constructed, it will use that same Lisp process
     * rather than restarting Lisp.  */
    private static Javalisp jl = null;

/**
 * The main routine for running the Java SHOP2 interface in stand
 * alone mode.  This routine is not used when the interface is
 * embedded in another program.  The main routine loads domain and
 * problem files (specified on the command line), runs SHOP2 on those
 * files, and then terminates SHOP2.
 *
 * @param args Command line arguments: domain file name, problem file
 * name, optional {@link #verbosity verbosity}
 * @see #Shop(String, String) Shop object constructor
 */
    public static void main(String[] args)
    {
	Shop jsjs;
	int level, i;

	if( args.length < 2 ||args.length > 3){
	    System.out.println("Usage :" );
	    System.out.println(" java Shop <domainDef-file-name> <problemDef-file-name> [verbosity]");
	    System.out.println("[verbosity] is an integer from 0 to 3. The default value is 0." );
	    return;
	}

      try{ 
	  for ( i=2; i< args.length;i++)
	      {
		  level=Integer.valueOf(args[i]).intValue();
		  verbosity=level;
	      }
      }catch (NumberFormatException e) {
          System.out.println("Invalid parameter" );
          return;
      }

      jsjs =  new Shop(args[0],args[1]);
      // Code for debugging embedded use constructor:
      //  new Shop("sample/d1.lisp", new State(), new TaskAtom("(main-goal)"));
      //  jsjs.printResults();
      
      Shop.kill();
    } // main


/**
 * This constructor starts SHOP2 (unless it is already running), and
 * then loads the specified domain file and problem file, and then it
 * has SHOP2 solve all of the problems that are defined in the problem
 * file (printing the results as it goes).  The {@link #tree tree},
 * {@link #addList addList}, and {@link #deleteList deleteList} fields
 * are set by this constructor to be the plan tree, add list, and
 * delete list for the <i>last</i> problem solved.
 * <p>This is the contructor invoked by the {@link #main(String [])
 * main} routine.  It is primarily created for use in stand-alone
 * operation of SHOP2, and it does not provide much fine-grained
 * control (i.e., it runs all of the plans in the given problem file
 * and it prints out all of the results that it produces).
 *
 * @param nameDomainFile The path and file name for a file that contains a
 * SHOP2 domain description (i.e., a defdomain or make-domain statement).
 * @param nameProblemFile The path and file name for a file that
 * contains one or more SHOP2 problem descriptions (i.e., defproblem
 * or make-problem statements).
 * @see #Shop(String, State, TaskAtom) other contructor
 */
    public Shop(String nameDomainFile , String nameProblemFile)
    {
	String retval, problemName, taskListString;
	boolean planListDone;

	startDomain(nameDomainFile);
	retval = jl.requestLisp("(load \"" + nameProblemFile + "\")");

	problemName = jl.requestLisp("(jshop-get-problem-name)");
	while ( !problemName.equalsIgnoreCase("NIL") ) {
	    solveProblem( problemName );
	    planListDone = jl.requestLisp("(jshop-next-plan)")
		.equalsIgnoreCase("NIL"); 
	    if (planListDone)
		System.out.println("JSHOP2: No plan found");

	    while (!planListDone) {
		taskListString = jl.requestLisp("(get '" +
						problemName + " :tasks)");
		extractResults(taskListString);
		printResults();
		planListDone = jl.requestLisp("(jshop-next-plan)")
		    .equalsIgnoreCase("NIL"); 
	    }
	    problemName = jl.requestLisp("(jshop-get-problem-name)");
	}
    }

    /**
     * This constructor starts SHOP2 (unless it is already running),
     * and then loads the specified domain file, and then it has SHOP2
     * create a plan for the specified task in the specified state.
     * The {@link #tree tree}, {@link #addList addList}, and {@link
     * #deleteList deleteList} fields are set by this constructor to
     * be the plan tree, add list, and delete list for the task and
     * state.
     * <p>This contructor is not used in stand-alone execution; it is
     * intended for use in programs that embed this package.  As such,
     * it provides relatively fine-grained control over execution
     * (i.e., it plans for a specified task in a specified domain and
     * does not do anything with the result, since it is assumed that
     * the calling program will access the results using the
     * {@link #getTree() getTree}, {@link #getAddList() getAddList}, and
     * {@link #getDeleteList} methods).
     *
     * @param nameDomainFile The path and file name for a file that contains a
     * SHOP2 domain description (i.e., a defdomain or make-domain statement).
     * @param startState The world state at the start of the planning process.
     * contains one or more SHOP2 problem descriptions (i.e., defproblem
     * or make-problem statements).
     * @param task The main task for which a plan should be created.
     * @see #Shop(String, String) other contructor
     */
    public Shop( String nameDomainFile, State startState, TaskAtom task)
    {
	String stateString, taskListString, retval;

	startDomain(nameDomainFile);
	stateString = startState.toString();
	taskListString = task.taskString();

	/*
	System.out.println("(defproblem jshop-problem () "
			   + stateString + taskListString + ")");
	System.exit(0);
	*/

	retval = jl.requestLisp("(defproblem jshop-problem () "
				+ stateString + taskListString + ")");

	solveProblem("jshop-problem");
	retval = jl.requestLisp("(jshop-next-plan)");
	extractResults(taskListString);
     }

    /**
     * @return The root node of a tree encoding the plan produced when the
     * Shop constructor was invoked.
     */
    public TaskNode getTree()
    {
        return tree;
    }

    /**
     * @return The {@link #addList addList}
     */
    public Vector getAddList()
    {
      return addList;
    }

    /**
     * @return The {@link #deleteList deleteList}
     */
    public Vector getDeleteList()
    {
      return deleteList;
    }


    /**
     * Terminates the external Lisp process that runs SHOP2.
     */
    public static void kill() {
	if (Javalisp.active())
	    jl.endLisp();
    }

    /**
     * Prints the plan, the plan tree, the add list, and the delete list.
     */
    public void printResults() {
	LogicalAtom a;
	Enumeration e;

	if (verbosity>0)
	    System.out.println("");

	System.out.println("***********************************************");
	System.out.println("Plan:");
	System.out.println(tree.plan().toString());
	System.out.println("***********************************************");
	System.out.println("Plan Hierarchy:");
	System.out.println(tree.toString());
	System.out.println("***********************************************");
	System.out.println("Add list:");
	e = addList.elements();
        while(e.hasMoreElements())
            {
                a = (LogicalAtom)e.nextElement();
		System.out.println("  " + a.toString());
            }
	System.out.println("***********************************************");
	System.out.println("Delete list:");
	e = deleteList.elements();
        while(e.hasMoreElements())
            {
                a = (LogicalAtom)e.nextElement();
		System.out.println("  " + a.toString());
            }
	System.out.println("***********************************************");
    }

    /**
     * First this routine starts SHOP2 if it is not already running.
     * Then it loads the SHOP2 domain file.
     */
    private void startDomain(String nameDomainFile)
    {
	String retval;
	start();
	retval = jl.requestLisp("(load \"" + nameDomainFile + "\")");
    }

    /**
     * This routine starts SHOP2 if it is not already running.
     */
    public static void start()
    {
	String retval;
	boolean viewLisp = (verbosity>0);

	if (jl == null)
	    {
		loadConfigFile();
		jl = new Javalisp(viewLisp);
		retval = 
		    jl.requestLisp("(load \"" + SHOP2FILE + "\")");
		retval = 
		    jl.requestLisp("(load \"" + JSHOP2FILE + "\")");
	    }
    }

    /**
     * True if and only if the connection to SHOP2 has successfully
     * been activated.  External functions should not call Shop
     * routines (other than start() and terminate() if this function
     * returns false.
     */
    public static boolean active() {
	return Javalisp.active();
    }

    /**
     * Runs the main SHOP2 planning routine.
     * @param problemName The name of a planning problem defined using a
     * defproblem statement in SHOP2.
     */
    private void solveProblem ( String problemName )
    {
	String whichProbs, retval;

	retval = jl.requestLisp("(jshop-find-plans '"
				+ problemName
				+ " :verbose " + verbosity + ")");
    }

    /**
     * Gets the {@link #tree tree}, {@link #addList}, and {@link
     * #deleteList} from a the completed execution of a single
     * planning problem.
     * @param taskString The task list portion of the original
     * problem.  If this list contains multiple tasks, the entire
     * string is used as the name of a root node that combines all of
     * the top level tasks in the tree.  The taskString is ignored if
     * the problem had only one top level task (this is always the
     * case when this object was created using the {@linkplain
     * #Shop(String, State, TaskAtom) constructor for embedded use}
     * because that constructor takes a single {@link TaskAtom
     * TaskAtom} as an argument.
     */
    private void extractResults(String taskString)
    {
	String retval, logicalAtomString, stateString;

	tree = new TaskNode(new TaskAtom(taskString));
	extractTree(tree);
	if (tree.children().size()==1)
	    tree = (TaskNode)(tree.children().firstElement());

	logicalAtomString = jl.requestLisp("(jshop-get-add-atom)");
	while ( !logicalAtomString.equalsIgnoreCase("NIL") ) {
	    addList.add(new LogicalAtom(logicalAtomString));
	    logicalAtomString = jl.requestLisp("(jshop-get-add-atom)");
	}

	logicalAtomString = jl.requestLisp("(jshop-get-delete-atom)");
	while ( !logicalAtomString.equalsIgnoreCase("NIL") ) {
	    deleteList.add(new LogicalAtom(logicalAtomString));
	    logicalAtomString = jl.requestLisp("(jshop-get-delete-atom)");
	}
    }

    /**
     * Helper for {@link #extractResults(String)} that recursively
     * gets the {@link #tree tree} from a completed execution of a
     * single planning problem.
     * @param parent The node from the higher level recursive call.
     */
    private void extractTree(TaskNode parent)
    {
	TaskNode newTaskNode;
	String taskAtomString, costString, positionString;
	double cost;
	int position;

	taskAtomString = jl.requestLisp("(jshop-get-tree-element)");
	while ( !taskAtomString.equalsIgnoreCase("NIL") ) {
	    if ( taskAtomString.equalsIgnoreCase(":OPERATOR") ) {
		costString = jl.requestLisp("(jshop-get-tree-element)");
		cost = Double.valueOf(costString).floatValue();
		taskAtomString = jl.requestLisp("(jshop-get-tree-element)");
		positionString = jl.requestLisp("(jshop-get-tree-element)");
		position = Integer.valueOf(positionString).intValue();
		newTaskNode = new TaskNode(new TaskAtom(taskAtomString,cost,
						    position));
	    } else {
		newTaskNode = new TaskNode(new TaskAtom(taskAtomString));
	    }

	    parent.addChild(newTaskNode);
	    extractTree(newTaskNode);

	    taskAtomString = jl.requestLisp("(jshop-get-tree-element)");
	}
    }

    /**
     * Loads configuration information from {@link #CONFIGFILE
     * CONFIGFILE}
     */
    private static void loadConfigFile() {
	String line;
	try {
	    BufferedReader config = 
		new BufferedReader(new FileReader(CONFIGFILE));
	    line = config.readLine();
	    while (line != null) {
		if (line.indexOf("SHOP2FILE=")==0)
		    SHOP2FILE=line.substring(line.indexOf("=")+1);
		line = config.readLine();
	    }
	    config.close();
	} catch (java.io.IOException e) {
	    System.err.println("Warning: Unable to load configuration file: " 
			       + CONFIGFILE);
	}
    }
}
