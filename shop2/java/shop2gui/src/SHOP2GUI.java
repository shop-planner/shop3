import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;


public class SHOP2GUI extends JFrame {
    // Used to iterate through commandList
    protected int iterator;
    
    // Used in various places when a global counter is convenient    
    protected int count;
    
    // Tracks which plan is currently being worked on    
    protected int planNumber;
    
    protected int numPlansTotal;
    
    // Holds the data that make up a command from the input    
    protected Command newCommand;
    
    // Two strings that temporarily hold the method and precondition info that will be
    // transferred to the "reduced" command that will follow
    protected String newMethod;    
    protected String newPreconditions;
    
    // A vector to store the commands in the order in which they're received from SHOP2.
    // Each element will be a Command object.
    protected Vector commandList;
    
    // A vector to store the leaf nodes that make up the plan.  The elements are
    // DefaultMutableTreeNode's, and their indices represent the order in which they were
    // visited.
    protected Vector leafNodes;
    
    // The tree object that is displayed on-screen    
    protected JTree tree;
    
    // The tree model that contains all the data within the tree structure    
    protected DefaultTreeModel treeModel;
    
    // The invisible root node that is the parent for the first node displayed in the tree    
    protected DefaultMutableTreeNode rootNode;
    
    // The label that displays the action taken at every step of the plan    
    protected Label messageLabel;
    
    protected Label stateLabel;
    
    
    // The text area where information about the current state and preconditions is displayed
    protected TextArea stateTextArea;
    
    protected TextArea methodsTextArea;
    
    // The text input box used to specifiy the step interval length
    protected TextField stepIntervalField;
    
    // A hashtable of DefaultMutableTreeNode references used to keep track of the nodes in the tree.
    // Currently, nodes that are deleted in the tree when backtracking aren't deleted in this
    // hashtable.  This doesn't seem to cause a problem except that it may become inefficient for 
    // large domains that backtrack a lot.  In those cases, a search for a particular node within
    // treeNodeReferences may waste a lot of time looking over nodes that have been deleted.
    protected Hashtable treeNodeReferences;
    
    // The progress bar object    
    protected JProgressBar progressBar;
    
    // The label used to show the number of steps in the plan
    protected Label progressLabel;  
    
    protected LeafTrackerDialog leafTracker;
    
    // The name of the currently selected node
    protected String selectedNodeName; 
    
    
    
    
    public SHOP2GUI() {
        iterator = 0;
        count = 0;
        planNumber = 0;
        numPlansTotal = 0;
        treeNodeReferences = new Hashtable();
        getContentPane().setLayout( new BorderLayout() );
        
        newMethod = "";
        newPreconditions = "";
        selectedNodeName = "";
        
        commandList = new Vector();         
        leafNodes = new Vector();
        
        // Creating the leafTracker object
        leafTracker = new LeafTrackerDialog( this );
        
        // Creating the menu bar and menus
        MenuBar mbar = new MenuBar();
        setMenuBar( mbar );
        SHOP2GUIMenuHandler handler = new SHOP2GUIMenuHandler();
        
        Menu file = new Menu( "File" );
        MenuItem item1;
        file.add( item1 = new MenuItem("Exit") );
        mbar.add( file );        
        
        // Registering listeners for File menu items        
        item1.addActionListener( handler );
        
        Menu view = new Menu( "View" );
        CheckboxMenuItem checkItem1;
        view.add( checkItem1 = new CheckboxMenuItem("Leaf Node Tracker") );
        mbar.add( view );
        
        // Registering listeners for View menu items
        checkItem1.addItemListener( handler );
     
        
        
        // ********************************************************* //
        // ------------------ Creating the Center ------------------ //
        // ********************************************************* //        
        
        // Creating tree and its scroll pane
        rootNode = new DefaultMutableTreeNode( "ROOTNODE" );
        treeModel = new DefaultTreeModel(rootNode);
        tree = new JTree( treeModel );
        tree.setRootVisible(false);
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.setCellRenderer( new NodeRenderer() ); // setting cell rendered to paint nodes
        ToolTipManager.sharedInstance().registerComponent(tree); //enable tool tips

        
        tree.getSelectionModel().setSelectionMode( TreeSelectionModel.SINGLE_TREE_SELECTION );
        
        // Adding a treeSelectionListner to the tree
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent e) {                
                DefaultMutableTreeNode treeNode = 
                            (DefaultMutableTreeNode)e.getPath().getLastPathComponent();
                Node node = (Node)treeNode.getUserObject();
                
                // setting the name for the currently selected node
                selectedNodeName = node.getName();
                
                // setting the text for stateTextArea
                String msg = node.getState(); 
                stateTextArea.replaceRange( msg, 0, stateTextArea.getText().length() );
                
                // setting the text for methodsTextArea
                msg = "Method:\n";
                msg += node.getMethod();
                msg += "\n\nPreconditions:\n";
                msg += node.getPreconditions();
                methodsTextArea.replaceRange( msg, 0, methodsTextArea.getText().length() );
                
                msg = "Current State          ( total: ";
                msg += node.getStateSize();
                msg += " )";
                
                stateLabel.setText( msg );
            }
        });        
        
        JScrollPane jpane = new JScrollPane(tree);
        
        getContentPane().add( jpane, BorderLayout.CENTER );
        
        
        
        // ****************************************************** //
        // --------------- Creating the East Side --------------- //
        // ****************************************************** //        
        
        // Creating buttons
        JButton singleStepButton = new JButton( "Single Step" );
        singleStepButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                runOneStep();
            }
        });
        
        
        JButton stepButton = new JButton( "Step" );
        stepButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int numSteps = ( Integer.valueOf(stepIntervalField.getText()) ).intValue();                
                for ( int i = 0; i < numSteps; i++ ) {
                    if ( runOneStep() == false )
                        break;
                }
                /*
                for (int i = 0; i < commandList.size(); i++)
                    ((Command)commandList.elementAt(i)).print();
                */
            }
        });
        
        JButton runButton = new JButton( "Run" );
        runButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                while ( runOneStep() ) {}
            }
        });
        
        
        JButton showStateButton = new JButton( "Show State" );
        showStateButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	String title = "State for ";
            	title += selectedNodeName;
                new StateWindowDialog( title );
            }
        });
        
        JButton restartButton = new JButton( "Restart" );
        restartButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                iterator = 0;
                planNumber = 0;
                treeNodeReferences.clear();
                
                // clearing the tree model and refreshing the display
                rootNode.removeAllChildren();
                treeModel.reload();
                
                // clearing the text in the top status label
                messageLabel.setText("");
                
                // clearing text in the bottom message boxes
                stateTextArea.replaceRange( "", 0, stateTextArea.getText().length() );
                methodsTextArea.replaceRange( "", 0, methodsTextArea.getText().length() );
                
                // clearing the list of leaf nodes
                leafNodes.clear();
                
                // resetting progress bar and label
                String msg = "Progress:  0 / ";
                msg += String.valueOf( commandList.size() );
                progressLabel.setText( msg );
                progressBar.setValue( 0 );
                
                // resetting stateLabel
                stateLabel.setText( "Current State          ( total: -- )" );
                
                // updating leaf node tracker
                leafTracker.updateNodeCount();
                
            }
        });
        
        // Creating step interval input field
        stepIntervalField = new TextField("1");
        
        // Adding components        
        JPanel innerPanel_1 = new JPanel();
        innerPanel_1.setLayout( new GridLayout(0, 1, 5, 5) );
        innerPanel_1.add( stepIntervalField );
        innerPanel_1.add( stepButton );
        innerPanel_1.add( singleStepButton );
        innerPanel_1.add( runButton );
        innerPanel_1.add( showStateButton );
        innerPanel_1.add( restartButton );
        
        JPanel innerPanel_2 = new JPanel();
        innerPanel_2.setLayout( new GridLayout(0, 1) );
        innerPanel_2.add( new Label( "Step Interval", Label.CENTER ) );
        innerPanel_2.add( stepIntervalField );
        
        
        JPanel outerPanel = new JPanel();
        outerPanel.setLayout( new BorderLayout() );        
        outerPanel.add( innerPanel_1, BorderLayout.CENTER );
        outerPanel.add( innerPanel_2, BorderLayout.NORTH );
        
        getContentPane().add( outerPanel, BorderLayout.EAST );
        
        
        
        // ******************************************************* //
        // --------------- Creating the South Side --------------- //
        // ******************************************************* //        
        
        // Creating message area  
        outerPanel = new JPanel();
        getContentPane().add( outerPanel, BorderLayout.SOUTH ); 
        outerPanel.setLayout( new BorderLayout() );
        
        JPanel messagePanel = new JPanel();
        outerPanel.add( messagePanel, BorderLayout.CENTER );
        messagePanel.setLayout( new GridLayout(1, 0) );
        
        JPanel leftMessagePanel = new JPanel();
        messagePanel.add( leftMessagePanel );
        leftMessagePanel.setLayout( new BorderLayout() );
        stateTextArea = new TextArea();
        stateTextArea.setEditable( false );
        leftMessagePanel.add( stateTextArea, BorderLayout.CENTER );
        stateLabel = new Label( "Current State          ( total: -- )" );
        leftMessagePanel.add( stateLabel, BorderLayout.NORTH );        
        
        JPanel rightMessagePanel = new JPanel();
        messagePanel.add( rightMessagePanel );
        rightMessagePanel.setLayout( new BorderLayout() );
        methodsTextArea = new TextArea();
        methodsTextArea.setEditable( false );
        rightMessagePanel.add( methodsTextArea, BorderLayout.CENTER );  
        rightMessagePanel.add( new Label("Methods & Preconditions"), BorderLayout.NORTH );
        
        
        // Creating progress bar
        progressBar = new JProgressBar();
        progressLabel = new Label( "Progress:  0 / 0" );
        
        innerPanel_1 = new JPanel();
        outerPanel.add( innerPanel_1, BorderLayout.EAST );        
        innerPanel_1.setLayout( new BorderLayout() );        
        
        innerPanel_2 = new JPanel();
        innerPanel_1.add( innerPanel_2, BorderLayout.NORTH );
        innerPanel_2.setLayout( new GridLayout(0,1) );
        innerPanel_2.add( progressLabel );
        innerPanel_2.add( progressBar );
        
        
        
        // ******************************************************* //
        // --------------- Creating the North Side --------------- //
        // ******************************************************* //        
        
        // Creating message Label        
        messageLabel = new Label("Loading..."); 
        getContentPane().add( messageLabel, BorderLayout.NORTH );
        
        
        
        // ********************************************************* //
        // --------------- Making the window visible --------------- //
        // ********************************************************* //        
        
        // Create main window
        setSize( new Dimension(800, 700) );
        setTitle( "Graphical Interface for SHOP2" );
        setVisible( true );
        
        // Register listeners
        addWindowListener( new SHOP2GUIWindowAdapter() );
        tree.addKeyListener( new SHOP2GUIKeyAdapter() );
    }
    
    
    /**
     * Processes the input into a usable form
     */    
    public void processInput( String input ) {        
        input = input.substring( 1, input.length()-1 ); // removing double quotes
        System.out.print(input + "\n");
        
        // a fix to compensate for the fact that I don't know when SHOP2 
        // has finished sending input for one command.  Here, I have used this
        // 0 tag to notify processInput() that the input for the current command
        // has ended and a new one has begun.
        if ( input.charAt(0) == '0' ) {
            // setting progress label
            String msg = "Progress:  ";
            msg += String.valueOf( iterator );
            msg += " / ";
            msg += String.valueOf( commandList.size() );
            progressLabel.setText( msg );
            
            count = 0;
            newCommand = new Command();
            commandList.add( newCommand );            
            
            // The code below assumes that the input for the method name and preconditions
            // will always be directly followed by the input that facilitates the creation
            // of the "reduced" command related to the said method and preconditions.
            newCommand.method = newMethod;
            newCommand.preconditions = newPreconditions;
            newMethod = "";
            newPreconditions = "";
        }
        
        // reading in action        
        if ( input.charAt(0) == '2' ) {           
            newCommand.action = input.substring( 1, input.length() );
            
            // creating new vector objects only out of need for efficiency purposes
            if ( newCommand.action.equals("TRYING") )
                newCommand.state = new Vector();
            else if ( newCommand.action.equals("REDUCED") )
                newCommand.children = new Vector();
        }
            
        // reading in task atom
        else if ( input.charAt(0) == '3' && input.charAt(1) == '('  ) 
            newCommand.taskAtom = editTaskAtom( input.substring(1, input.length()) );
        
            
        // reading in children           
        else if ( input.charAt(0) == '4' ) {
            if ( count == 0 ) {
                if ( input.equals("4:UNORDERED") )
                    newCommand.ordered = false;
                count++;
            } else 
                newCommand.children.add( editTaskAtom(input.substring(1, input.length())) );
        }
        
        // reading in the state
        else if ( input.charAt(0) == '5' && newCommand.action.equals("TRYING") ) 
            newCommand.state.add(input.substring( 1, input.length() ));
            
        
        // creating a plan found notification command
        else if ( input.charAt(0) == '6' ) {
            newCommand = new Command();
            newCommand.planFound = true;
            commandList.add( newCommand );
            numPlansTotal++;
        }
        
        
        // Notifying rest of program that end-of-input has been reached
        else if ( input.charAt(0) == '7' ) {
            messageLabel.setText( "Ready" );            
            
            // Setting Progress Bar
            int size = commandList.size();
            String msg = "Progress:  ";
            msg += String.valueOf( iterator );
            msg += " / ";
            msg += String.valueOf( size );
            progressLabel.setText( msg );
            progressBar.setMaximum( size );
            progressBar.setValue( 0 );
        }
        
        else if ( input.charAt(0) == '8' )
            newMethod = input.substring( 1, input.length() );
        
        else if ( input.charAt(0) == '9' )
            newPreconditions = input.substring( 1, input.length() );
    }
    
    
    /**
     * Deletes unnecessary information in the task atom (e.g. :TASK, :IMMEDIATE)
     */  
    private String editTaskAtom( String taskIn ) {
        String retval = "(";
        
        if ( taskIn.startsWith("(:TASK :IMMEDIATE") )
            retval += taskIn.substring(18);
        else
            retval += taskIn.substring(7);
        
        return retval;
    }
    
    
    /**
     * Executes a single step in the command list
     */    
    private boolean runOneStep() {
        boolean retval = true;
        Vector toAdd = new Vector();
        DefaultMutableTreeNode parent = null;
        
        if ( iterator < commandList.size() ) {
            Command cmd = (Command)commandList.elementAt(iterator++);
            
            // setting progress bar
            String msg = "Progress:  ";
            msg += String.valueOf( iterator );
            msg += " / ";
            msg += String.valueOf( commandList.size() );
            progressLabel.setText( msg );
            progressBar.setValue( iterator );
            
            
            // if a plan has been found            
            if ( cmd.planFound == true ) {
                processPlanFound();
                
                // retval is set to false here so that the "run" feature will stop every time
                // a plan is found.  
                retval = false;
            }
            
            // trying a task
            else if ( cmd.action.equals("TRYING") ) {
                // If the last command was a "plan found" command, then the leafNodes
                // vector has to be cleared now that it is working on a new plan.  Here,
                // iterator is subtracted by 2 due to the fact that it's been post-incremented
                // above.  This step assumes that a "plan found" command will always be followed
                // by a "trying" command, and it's implemented to enable dynamic leaf node tracking.
                if ( iterator - 2 >= 0 )
                    if ( ((Command)commandList.elementAt(iterator - 2)).planFound == true )
                        leafNodes.clear();
                parent = processTrying( cmd, toAdd );
            }
                
            // reducing a task
            else if ( cmd.action.equals("REDUCED") )
                parent = processReduced( cmd, toAdd );
                
            // backtracking   
            else if ( cmd.action.equals("BACKTRACKING") )
                processBacktracking( cmd );
                
            // updating leaf node tracker
            leafTracker.updateNodeCount();
            
            // adding nodes to treeModel and treeNodeReferences            
            for (int i = 0; i < toAdd.size(); i++) {
                Node add = (Node)toAdd.elementAt(i);
                String childName = add.getName();
                DefaultMutableTreeNode child = new DefaultMutableTreeNode( add );
                treeNodeReferences.put( childName, child );
                treeModel.insertNodeInto( child, parent, parent.getChildCount() );
                tree.scrollPathToVisible(new TreePath(child.getPath())); // makes the node visible
                if (iterator == 1) // special case when displaying the goal task
                    tree.setSelectionPath(new TreePath(child.getPath())); 
            }
        } else
            retval = false;
        
        return retval;
    }
    
    
    
    
    /**
     * Helper function to runOneStep().
     * This function executes the steps required to display the plan on-screen
     */
    private void processPlanFound() {
        planNumber++;
        Vector planList = new Vector(); // vector of strings containing the leaves' numbered names
        
        // setting messageLabel
        messageLabel.setText( "Plan found" );
        
        // Labeling leaf nodes with appropriate numbers.
        // Although renumberLeaves() does the same thing, it isn't called here
        // so that planList can be created in the process of renumbering the leaves.
        for (int i = 0; i < leafNodes.size(); i++) {                    
            DefaultMutableTreeNode leaf = (DefaultMutableTreeNode)leafNodes.elementAt(i);
            Node node = (Node)leaf.getUserObject();
            node.setTag( i+1 );            
            planList.add( node.toString() );
            
            treeModel.nodeChanged( leaf );
        }        
        
        // Creating found plan dialog box
        String title = "Plan ";
        title += String.valueOf( planNumber );
        title += " of ";
        title += String.valueOf( numPlansTotal );
        new PlanDialog( title, planList );
    }
    
    
    /**
     * Helper function to runOneStep().
     * This function determines the current state of the world for every node
     * and inserts the goal task into the tree.
     */    
    private DefaultMutableTreeNode processTrying( Command cmd, Vector toAdd ) {        
        DefaultMutableTreeNode parent = null;
        
        // set messageLabel
        String msg = "Trying ";
        msg += cmd.taskAtom;
        messageLabel.setText( msg );                
                    
        // if adding the root node
        if ( iterator == 1 ) {
            parent = rootNode;
            Node temp = new Node( cmd.taskAtom );
            temp.setState( cmd.state );
            toAdd.add( temp );
            
            // I should add temp to the leafNodes list here, but it's problematic
            // because I don't have access to its DefaultMutableTreeNode wrapper object
            // due to it not being created yet.  This is only a problem in the trivial case
            // where the plan consists of only the root node, so I deemed it unnecessary to
            // deal with...for now.
            
        } else { // setting node's state to current state
            DefaultMutableTreeNode treeNode = 
                    (DefaultMutableTreeNode)treeNodeReferences.get( cmd.taskAtom );
            Node temp = (Node)treeNode.getUserObject();
            temp.setState( cmd.state );  
            treeModel.nodeChanged( treeNode ); 
            
            // adding the node as a leaf, as long as it's not (!!INOP)
            if ( !cmd.taskAtom.equals("(!!INOP)") ) {
                leafNodes.add( treeNode );
                renumberLeaves();
            }
            
            // selecting the node onscreen                       
            tree.setSelectionPath(new TreePath(treeNode.getPath())); 
            tree.scrollPathToVisible(new TreePath(treeNode.getPath())); // makes the node visible
        }
        
        return parent;
    }
    
    
    /**
     * Helper function to runOneStep().
     * This function adds children to existing nodes and marks them if they are ordered.
     */  
    private DefaultMutableTreeNode processReduced( Command cmd, Vector toAdd ) {
        DefaultMutableTreeNode parent = null;        
        
        // set messageLabel
        String msg = "Reduced ";
        msg += cmd.taskAtom;
        msg += " into the following: ";
        
        // The term "parent" here is referring to the current node (parent as in parent of
        // the children that are about to be added).
        parent = (DefaultMutableTreeNode)treeNodeReferences.get( cmd.taskAtom );
        
        // Setting the method name and preconditions for this node       
        Node node = (Node)parent.getUserObject();
        node.setMethod( cmd.method );
        node.setPreconditions( cmd.preconditions );
        
        // Removing this node from the leaf nodes list, since it now has children.
        // This step assumes that a REDUCED statement will always follow a TRYING
        // statement pertaining to the same task atom, resulting in the deletion of the 
        // last element in leafNodes.
        if ( !leafNodes.isEmpty() ) {
            // removing numbering from this node
            node.deleteTag();
            treeModel.nodeChanged( parent );
            
            // removing this node from leafNodes list
            leafNodes.removeElementAt( leafNodes.size()-1 );
        }
        
        // backtrack if this node has already been reduced and is being reduced again  
        if ( !parent.isLeaf() )
            backtrack( parent );
        
        // selecting the node onscreen         
        tree.setSelectionPath(new TreePath(parent.getPath()));
        tree.scrollPathToVisible(new TreePath(parent.getPath())); // makes the node visible
        
        // creating the children to be added
        for ( int i = 0; i < cmd.children.size(); i++ ) {
            String childName = (String)cmd.children.elementAt(i);
            Node newNode = new Node( childName );
            msg += childName;
            msg += " ";
            if ( !cmd.ordered )
                newNode.setUnordered();
            toAdd.add(newNode);
        } 
        
        messageLabel.setText( msg ); 
        
        
        
    	return parent;
    }
    
    
    /**
     * Helper function to runOneStep().
     * This function takes care of backtracking procedures.
     */
    private void processBacktracking( Command cmd ) {
        String msg = "Backtracking from ";
        msg += cmd.taskAtom;
        messageLabel.setText( msg );
        
        DefaultMutableTreeNode treeNode = 
                    (DefaultMutableTreeNode)treeNodeReferences.get( cmd.taskAtom );
                    
        if ( !treeNode.isLeaf() )        
            backtrack( treeNode );
        
        // selecting the node onscreen 
        tree.setSelectionPath(new TreePath(treeNode.getPath())); // selects the node
        tree.scrollPathToVisible(new TreePath(treeNode.getPath())); // makes the node visible
    }
    
    
    /**
     * This function handles the procedures that take place when backtracking.
     * First, it modifies the leafNodes list and then deletes any children under
     * treeNode.
     */
    private void backtrack( DefaultMutableTreeNode treeNode ) {
        // removing any leaves in leafNodes that are descendants of treeNode
        for (int i = 0; i < leafNodes.size(); i++) {
            if ( treeNode.isNodeDescendant((DefaultMutableTreeNode)leafNodes.elementAt(i)) ) {
                leafNodes.removeElementAt(i);
                i--;
            }
        }        
        
        // reunumbering leaves on-screen
        renumberLeaves();
            
        // deleting children from this node            
        treeNode.removeAllChildren();        
        
        treeModel.reload( treeNode );
    }
    
    
    /**
     * This function renumbers all current leaf nodes in the order they were visited
     * relative to one another.
     */
    private void renumberLeaves() {
        for (int i = 0; i < leafNodes.size(); i++) {
            DefaultMutableTreeNode leaf = (DefaultMutableTreeNode)leafNodes.elementAt(i);
            Node node = (Node)leaf.getUserObject();
            node.setTag( i+1 );
            
            treeModel.nodeChanged( leaf );
        }
    }
    
    
    
    /**
     * Listener Classes    
     */    
    private class SHOP2GUIWindowAdapter extends WindowAdapter {
        public void windowClosing(WindowEvent we) {
            System.exit(0);
        }
    }
    
    private class SHOP2GUIKeyAdapter extends KeyAdapter {
        public void keyTyped(KeyEvent ke) {
            if ( ke.getKeyChar() == ' ' ) {
                runOneStep();
            }
        }
    }
    
    private class SHOP2GUIMenuHandler implements ActionListener, ItemListener {
        public void actionPerformed( ActionEvent ae ) {
            String arg = (String)ae.getActionCommand();
            if ( arg.equals("Exit") )
                System.exit(0);
        }
        
        // Handle item events
        public void itemStateChanged( ItemEvent ie ) {
            String arg = ie.paramString();
            if ( arg.lastIndexOf("Leaf Node Tracker") != -1 ) {
                if ( arg.lastIndexOf( "DESELECTED") != -1 )
                    leafTracker.visible( false );
                else
                    leafTracker.visible( true );
            }
        }
    }
    
    
    /**
     * User object utilized by the tree model    
     */
    private class Node {
        protected String name;
        protected String method;
        protected String preconditions;
        protected boolean ordered;
        protected Vector state;
        protected Integer tag;
        
        public Node( String nameIn ) {
            name = nameIn;
            method = "";
            preconditions = "";
            ordered = true;
            state = null;
            tag = null;
        }
        
        public String getName() {
            return name;
        }
        
        public void setName( String nameIn ) {
            name = nameIn;
        }
        
        public String getMethod() {
            return method;
        }
        
        public void setMethod( String methodIn ) {
            method = methodIn;
        }
        
        public String getPreconditions() {
            return preconditions;
        }
        
        public void setPreconditions( String preconditionsIn ) {
            preconditions = preconditionsIn;
        }
        
        public void setTag( int newTag ) {
            tag = new Integer( newTag );
        }
        
        public void deleteTag() {
            tag = null;
        }

        
        public String toString() {
            if ( tag == null )
                return name;
            else {
                String newName = "[ ";
                newName += String.valueOf( tag.intValue() );
                newName += " ]    ";
                newName += name;
                return newName;
            }
        }
        
        public void setUnordered() {
            ordered = false;
        }
        
        public boolean isOrdered() {
            return ordered == true;
        }
        
        public boolean hasState() {
            return state != null;
        }
        
        public void setState( Vector stateIn ) {
            state = stateIn;
        }
        
        public String getState() {
            String retval = null;
            
            // the state is unknown
            if ( state == null)
                retval = "unknown";
            // the state is empty
            else if ( state.size() == 0 ) {
                retval = "";
            } else {                
                retval = (String)state.firstElement();            
                for ( int i=1; i < state.size(); i++ ) {
                    retval += "\n";
                    retval += (String)state.elementAt(i);                
                }            
            }
            return retval;
        }
        
        public String getStateSize() {
            String retval = null;
            
            // if the state is unknown
            if ( state == null )
                retval = "--";
            
            // if the state is known
            else 
                retval = String.valueOf( state.size() );            
            
            return retval;
        }
    }
    
    
    /**
     * Object used to render the nodes in the tree
     */    
    private class NodeRenderer extends DefaultTreeCellRenderer {
        ImageIcon yellowBall, blueBall, smYellowBall, smBlueBall;
        
        public NodeRenderer() {
            yellowBall = new ImageIcon( "images/yellow.gif" );
            blueBall = new ImageIcon( "images/blue.gif" );
            smYellowBall = new ImageIcon( "images/small-yellow.gif" );
            smBlueBall = new ImageIcon( "images/small-blue.gif" );
        }
        
        public Component getTreeCellRendererComponent(
                            JTree tree,
                            Object value,
                            boolean sel,
                            boolean expanded,
                            boolean leaf,
                            int row,
                            boolean hasFocus) {

            super.getTreeCellRendererComponent(
                            tree, value, sel,
                            expanded, leaf, row,
                            hasFocus);
                            
            if ( value instanceof DefaultMutableTreeNode ) {
                DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)value;
                if ( isOrdered(treeNode) ) {                    
                    if ( isVisited(treeNode) ) {
                        setIcon( yellowBall );
                        setToolTipText( "Ordered, Visited" );
                    } else {
                        setIcon( smYellowBall );
                        setToolTipText( "Ordered, Unvisited" );
                    }
                    
                } else {                  
                    if ( isVisited(treeNode) ) {                        
                        setIcon( blueBall );
                        setToolTipText( "Unordered, Visited" );
                    } else {
                        setIcon( smBlueBall );
                        setToolTipText( "Unordered, Unvisited" );
                    }
                } 
            }

            return this;
        }   
        
        private boolean isOrdered( DefaultMutableTreeNode treeNode ) {
            if ( treeNode.getUserObject() instanceof Node ) {
                Node node = (Node)treeNode.getUserObject();            
                return node.isOrdered();
            } else
                return false;
        }
        
        
        private boolean isVisited( DefaultMutableTreeNode treeNode ) {
            if ( treeNode.getUserObject() instanceof Node ) {
                Node node = (Node)treeNode.getUserObject();            
                return node.hasState();
            } else
                return false;
        }

    }
    
    
    /**
     * An object that holds a single command.  All data members are
     * intentionally made public for the sake of efficiency when reading
     * in input, although I'm not sure if it is more efficient this way.
     */    
    private class Command {
        public String taskAtom;
        public String action;
        public String method;
        public String preconditions;
        public Vector state;
        public Vector children;
        public boolean ordered;
        public boolean planFound;
        
        
        public Command() {
            taskAtom = "";
            action = "";
            method = "";
            preconditions = "";
            state = null;
            children = null;
            ordered = true;
            planFound = false;  
        }
        
        public void print() {
            System.out.print( "Task Atom: " + taskAtom + "\n" );
            System.out.print( "Action: " + action + "\n" );
            System.out.print( "Method: " + method + "\n" );
            System.out.print( "Preconditions: " + preconditions + "\n" );
            System.out.print( "Children: ");            
            if ( children != null ) {
                for (int i = 0; i < children.size(); i++)
                    System.out.print(( String)children.elementAt(i) );
                System.out.print("\n");
            } else
                System.out.print("none\n");
                
            System.out.print( "State: ");            
            if ( state != null ) {
                for (int i = 0; i < state.size(); i++)
                    System.out.print(( String)state.elementAt(i) );
                System.out.print("\n");
            } else
                System.out.print("none\n");
                
            System.out.print( "Ordered: ");
            if (ordered)
                System.out.print( "true\n");
            else
                System.out.print( "false\n");
                
            System.out.print("\n\n");
        }
        
    }
    
    
    /**
     * The object that creates the dialog box showing plan results whenever
     * a plan is found.
     */    
    private class PlanDialog extends JDialog {
        public PlanDialog(  String title, Vector planList ) {
            setTitle( title );
            
            getContentPane().setLayout( new FlowLayout(FlowLayout.CENTER, 5, 20) );
            
            // Creating message string to display in text area            
            String msg = "";
            for ( int i = 0; i < planList.size(); i++ ) {
                msg += (String)planList.elementAt(i);
                msg += "\n";
            }
            
            // Creating the text area that will display the plan            
            TextArea textBox = new TextArea( msg, 24, 63 );
            textBox.setEditable( false );
            
            // Creating the "Close" button
            JButton closeButton = new JButton( "  Close  " );
            closeButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            });
            
            // Adding components to the dialog box          
            getContentPane().add( textBox );
            getContentPane().add( closeButton );
           
            
            setSize( new Dimension(500,500) );
            setVisible( true );
        }
    }
   
    private class StateWindowDialog extends JDialog {
    	public StateWindowDialog( String title ) {
    		setTitle( title );
    		getContentPane().setLayout( new FlowLayout(FlowLayout.CENTER, 5, 20) );
    		
    		 // Creating the text area that will display the plan            
            TextArea textBox = new TextArea( stateTextArea.getText(), 24, 63 );
            textBox.setEditable( false );
            
            // Creating the "Close" button
            JButton closeButton = new JButton( "  Close  " );
            closeButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            });
            
            // Adding components to the dialog box          
            getContentPane().add( textBox );
            getContentPane().add( closeButton );   
            
            
            setSize( new Dimension(500,500) );
            setVisible( true );
    		
    	}
    	
    }
    
    
    /**
     * The object that creates the leaf node tracker dialog box.
     */
    private class LeafTrackerDialog extends JDialog {
        private Label leafTotalLabel;
        private TextField leafNumberField;
        
        public LeafTrackerDialog( Frame parent ) {
            super( parent, "Leaf Node Tracker" );
            getContentPane().setLayout( new BorderLayout() );
            
                // adding leafTotalLabel
            leafTotalLabel = new Label("Leaf Nodes Total:    0");
            getContentPane().add( leafTotalLabel, BorderLayout.NORTH );
        
                // creating a new border layout container for the input text field
            JPanel innerPanel_1 = new JPanel();
            getContentPane().add( innerPanel_1, BorderLayout.CENTER );        
            innerPanel_1.setLayout( new BorderLayout() );
            JPanel innerPanel_2 = new JPanel();
            innerPanel_1.add( innerPanel_2, BorderLayout.NORTH );
            innerPanel_2.setLayout( new GridLayout(1,0) );
            innerPanel_2.add( new Label("Leaf number:") );
            leafNumberField = new TextField("1");
            innerPanel_2.add( leafNumberField );
        
        
                // creating a new border layout container for the buttons
            innerPanel_2 = new JPanel();
            innerPanel_1.add( innerPanel_2, BorderLayout.CENTER );        
            innerPanel_2.setLayout( new BorderLayout() );
            JPanel innerPanel_3 = new JPanel();
            innerPanel_2.add( innerPanel_3, BorderLayout.NORTH );
            innerPanel_3.setLayout( new GridLayout(1,0) );
        
                // adding buttons
            JButton prevButton = new JButton("Prev");
            prevButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int leafNum = ( Integer.valueOf(leafNumberField.getText()) ).intValue();
                    leafNum--;
                    if ( leafNum > 0 && leafNum <= leafNodes.size() ) {
                        leafNumberField.setText( String.valueOf(leafNum) );
                        DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)leafNodes.elementAt(leafNum - 1);
                        tree.setSelectionPath(new TreePath(treeNode.getPath())); // selects the node
                        tree.scrollPathToVisible(new TreePath(treeNode.getPath())); // makes the node visible
                    }
                
                }
            });
        
            JButton findButton = new JButton("Find");
            findButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int leafNum = ( Integer.valueOf(leafNumberField.getText()) ).intValue();
                    if ( leafNum > 0 && leafNum <= leafNodes.size() ) {
                        DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)leafNodes.elementAt(leafNum - 1);
                        tree.setSelectionPath(new TreePath(treeNode.getPath())); // selects the node
                        tree.scrollPathToVisible(new TreePath(treeNode.getPath())); // makes the node visible
                    }
                }
            });
        
            JButton nextButton = new JButton("Next");
            nextButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int leafNum = ( Integer.valueOf(leafNumberField.getText()) ).intValue();
                    leafNum++;
                    if ( leafNum > 0 && leafNum <= leafNodes.size() ) {
                        leafNumberField.setText( String.valueOf(leafNum) );
                        DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)leafNodes.elementAt(leafNum - 1);
                        tree.setSelectionPath(new TreePath(treeNode.getPath())); // selects the node
                        tree.scrollPathToVisible(new TreePath(treeNode.getPath())); // makes the node visible
                    }
                
                }
            });
            innerPanel_3.add( prevButton );
            innerPanel_3.add( findButton );
            innerPanel_3.add( nextButton );
            
            setSize( new Dimension(200,110) );
        }
        
        public void visible( boolean in ) {
            setVisible( in );
        }
        
        public void updateNodeCount() {
            String msg = "Leaf Nodes Total:    ";
            msg += String.valueOf(leafNodes.size());
            leafTotalLabel.setText( msg );
        }
    }    
}