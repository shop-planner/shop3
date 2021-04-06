#! /usr/bin/env perl

# Script to run all of the problems in a number of domains (all the domains with a 
# file named "run-all.lisp", in order to collect useful statistics.


use File::Find;

sub wanted {
  if ( $_ eq "run-all.lisp" ) {
    print "Running all domains for $File::Find::dir\n";
    my $exit = system("sbcl --eval '(shop3)' --eval '(load \"$File::Find::name\")' --eval '(uiop:quit 0)'");
    print "SBCL invocation exited with code $exit\n";
    return 1;
  }
  else {
    return 0;
  }
}

find(\&wanted, ("/home/rpg/lisp/shop/shop3/examples"));

