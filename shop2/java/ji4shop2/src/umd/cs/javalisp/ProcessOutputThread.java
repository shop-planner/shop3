package umd.cs.javalisp;
import java.lang.Object.*;
import java.io.*;

/**
 * Thread for reading the output of a process spawned by Runtime.exec. 
 *
 * @author J. William Murdock
 * @version 0.2, 2003-2-20
 */
class ProcessOutputThread extends Thread {
    /**
     * Process to get the output from
     */
    private Process proc;

    /**
     * If true, all output from the process is displayed to the standard
     * output.  If false, output is ignored.
     */
    private boolean display;

    /**
     * @param p Value for {@link #proc proc}
     * @param d Value for {@link #display display}
     */
    public ProcessOutputThread(Process p, boolean d) {
	proc = p;
	display = d;
    }

    /**
     * Reads lines output by the {@link #proc proc}.  Displays them to
     * the standard output if and only if {@link #display display} is
     * true.
     */
    public void run () {
	BufferedReader po = 
	    new BufferedReader(new InputStreamReader(proc.getInputStream()));
	try {
	    String s = po.readLine();
	    while (s != null) {
		if (display)
		    System.out.println(s);
		s = po.readLine();
	    }
	} catch (IOException e) {
	}
    }
}
