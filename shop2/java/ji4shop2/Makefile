CC = gcc
JAVAC=javac
DIR=src/umd/cs/shop2/
BIN=bin/
LDIR=src/umd/cs/javalisp/

.SUFFIXES: .java .class

all: bin $(LDIR)ProcessOutputThread.class $(LDIR)Javalisp.class $(DIR)LogicalAtom.class $(DIR)State.class $(DIR)TaskAtom.class $(DIR)TaskNode.class $(DIR)Shop.class

.java.class: bin
	$(JAVAC) -d $(BIN) -classpath $(BIN) $*.java

bin:
	mkdir -p $(BIN)

doc/index.html:  $(LDIR)ProcessOutputThread.java $(LDIR)Javalisp.java $(DIR)LogicalAtom.java $(DIR)State.java $(DIR)TaskAtom.java $(DIR)TaskNode.java $(DIR)Shop.java
	javadoc -private -d doc umd.cs.shop2 umd.cs.javalisp

clean:
	- rm *~ $(DIR)*~ $(LDIR)*~
	- rm -rf $(BIN)*
