package umd.cs.javalisp;
import java.lang.Runtime.*;
import java.io.*;

/**
 * Simple but vauguely general-purpose mechanism for invoking Lisp
 * commands from within Java using files.
 *
 * @author J. William Murdock
 * @version 0.2, 2003-2-19
 */
public class Javalisp {
    /**
     * Name of file that holds configuration info (specifically
     * {@link #LISP LISP} and {@link #LISPLOADARG LISPLOADARG}).
     */
    static final String CONFIGFILE = "config.txt";

    /**
     * Command to invoke the Common Lisp interpretter
     */
    static String LISP = "lisp";

    /**
     * A command-line argument to load a file in Lisp.  For Allegro
     * Lisp, this is "-L".  For LispWorks this is "-init".  For CLISP
     * it's "-i".  For other Lisp environments, see your manual.
     */
    static String LISPLOADARG = "-L";

    static final String LISPFILE = "umd/cs/javalisp/javalisp";

    /**
     * Name of file used to send commands to Lisp
     */
    static final String OUTFILE = "tolisp.txt";

    /**
     * Name of file used to receive responses from Lisp
     */
    static final String INFILE = "tojava.txt";

    /**
     * Number of milliseconds to wait when there is partial input.
     * This is zero by default but you may want to set it higher if
     * performing many file accesses cause problems on your system.
     */
    static final long PARTIAL_INPUT_WAIT = 0; /* or maybe 100; */

    /**
     * Number of milliseconds to wait when there is partial input.
     * This is zero by default but you may want to set it higher if
     * performing many file accesses cause problems on your system.
     */
    static final long NO_INPUT_WAIT = 0; /* or maybe 500; */

    /**
     * OS dependent newline string.
     */
    static String newline = System.getProperty("line.separator");

    /**
     * This flag is set to false if the class is unable to access
     * Tcl; it indicates to any calling program that it should not
     * try to invoke Tcl.
     * @see #active() active()
     */
    private static boolean active = false;

    /**
     * @param viewLisp If true, all text sent to and received from Lisp
     * will be displayed on the standard output.
     */
    public Javalisp (boolean viewLisp) {
	Process lispProc;
	ProcessOutputThread lispThread;

	if (!active) {
	    loadConfigFile();

	    try {
		BufferedWriter outwriter = 
		    new BufferedWriter(new FileWriter(OUTFILE));
		outwriter.newLine();
		outwriter.close();
		outwriter = new BufferedWriter(new FileWriter(INFILE));
		outwriter.newLine();
		outwriter.close();

		lispProc = Runtime.
		    getRuntime().exec(LISP+" "+LISPLOADARG+" "+LISPFILE);
		lispThread = new ProcessOutputThread(lispProc,viewLisp);
		lispThread.start();
	    } catch (java.io.IOException e) {
		System.err.println("Unable to start Lisp using command "
				   +LISP+" "+LISPLOADARG+" "+LISPFILE);
		System.exit(1);
	    }
	    active = true;
	}
    }

    /**
     * Executes a Lisp command
     * @param lispCmd The command
     * @return The string representation of the value returned from
     * the Lisp command
     */
    public String requestLisp (String lispCmd) {
	FileReader inreader;
	BufferedWriter outwriter;
	String result;
	long wait;

	try {
	    outwriter = new BufferedWriter(new FileWriter(OUTFILE));
	    outwriter.write(lispCmd);
	    outwriter.newLine();
	    outwriter.close();
	} catch (java.io.IOException e) {
	    System.err.println("File I/O error in JavaLisp");
	    System.exit(1);
	}

	while (true) {
	    try {
		inreader = new FileReader(INFILE);
		if (inreader.ready()) {
		    int i;
			
		    result = readString(inreader);
		    inreader.close();
		    
		    i = result.indexOf(newline);
		    if (i<1) {
			wait = PARTIAL_INPUT_WAIT;
		    } else {
			/* Empty the input file */
			outwriter = new BufferedWriter(new FileWriter(INFILE));
			outwriter.newLine();
			outwriter.close();
			return result.substring(0,i);
		    }
		} else wait = NO_INPUT_WAIT;
	    } catch (java.io.IOException e) {
		wait = NO_INPUT_WAIT;
	    }

	    if (wait > 0) {
		try {
		    Thread.currentThread().sleep(wait);
		} catch (java.lang.InterruptedException e) {
		    System.err.println("Thread error in JavaLisp");
		    System.exit(1);
		}
	    }
	}
    }

    /**
     * Simple helper function that reads an entire file into a string
     * @param infile The input stream
     */
    private String readString(FileReader infile) throws java.io.IOException {
	StringBuffer retval = new StringBuffer(40);
	char c[] = new char[1];

	while ((infile.read(c,0,1)) != -1)
	    retval.append(c[0]);

	return retval.toString();
    }


    /**
     * Terminates the Lisp interpretter
     */
    public void endLisp ()  {
	try {
	    FileWriter outwriter = new FileWriter(OUTFILE);
	    outwriter.write("(exit)"+newline);
	    outwriter.close();
	} catch (java.io.IOException e) {
	    System.err.println("File I/O error in JavaLisp during exit");
	    System.exit(1);
	}
    }

    /**
     * Returns {link #active active}
     */
    public static boolean active() {
        return active;
    }

    /**
     * Loads configuration information from {@link #CONFIGFILE
     * CONFIGFILE}
     */
    private void loadConfigFile() {
	String line;
	try {
	    BufferedReader config = 
		new BufferedReader(new FileReader(CONFIGFILE));
	    line = config.readLine();
	    while (line != null) {
		if (line.indexOf("LISP=")==0)
		    LISP=line.substring(line.indexOf("=")+1);
		else if (line.indexOf("LISPLOADARG=")==0)
		    LISPLOADARG=line.substring(line.indexOf("=")+1);
		line = config.readLine();
	    }
	    config.close();
	} catch (java.io.IOException e) {
	    System.err.println("Warning: Unable to load configuration file: " 
			       + CONFIGFILE);
	}
    }
}
