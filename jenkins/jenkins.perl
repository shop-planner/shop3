#!/usr/bin/env perl
package shop_jenkins;

use strict;
use POSIX;

use FindBin;
use lib ( $FindBin::Bin );  # The absolute directory where this file
                            # lives.
chdir $FindBin::Bin;

use Carp qw ( verbose );
use Cwd;
use Data::Dumper;
use File::Basename;
use File::Copy qw ( copy );
use File::Find;
use File::Glob qw( :glob );
use File::Path qw(); # do not import any symbols
use File::Spec;
use FileHandle;
use Getopt::Long qw( :config no_ignore_case bundling );
use Sys::Hostname;

my @failures;

unlink "log"  if (-x "log") && !(-d "log");
mkdir "log"  if !(-x "log");
die "Can't create/verify log directory" unless -d "log";

die "SHOP_DIR not set" unless defined $ENV{SHOP_DIR};
die "SHOP_DIR not a directory" unless -d $ENV{SHOP_DIR};
my $shopdir = $ENV{SHOP_DIR};

die "NST_DIR not set" unless defined $ENV{NST_DIR};
die "NST_DIR not a directory" unless -d $ENV{NST_DIR};
my $nstdir = $ENV{NST_DIR};

die "CLOSER_MOP_DIR not set" unless defined $ENV{CLOSER_MOP_DIR};
die "CLOSER_MOP_DIR not a directory" unless -d $ENV{CLOSER_MOP_DIR};
my $closerdir = $ENV{CLOSER_MOP_DIR};

my $shopdirlisp = $FindBin::Bin . "/shopdir.lisp";
print "Writing $shopdirlisp\n";
open DIRSETTER, ("> ".$shopdirlisp);
print DIRSETTER "(defconstant +SHOP-DIRECTORY+ #p\"", $shopdir, "\")\n";
print DIRSETTER "(defconstant +NST-DIRECTORY+ #p\"", $nstdir, "\")\n";
print DIRSETTER "(defconstant +CLOSER-DIRECTORY+ #p\"", $closerdir, "\")\n";
close DIRSETTER;

my @lisps = ({
              tag => 'SHOP_ALLEGRO_MIXEDCASE',
              name => "Allegro mixed-case",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'SHOP_ALLEGRO_UPCASE',
              name => "Allegro uppercase",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'SHOP_ALLEGRO_MIXEDCASE8',
              name => "Allegro mixed-case 8-bit mode",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'SHOP_ALLEGRO_UPCASE8',
              name => "Allegro uppercase 8-bit mode",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'SHOP_CLISP_UPCASE',
              name => "CLISP uppercase",
              leadArgs => [ "-m", "1000M", "-on-error", "exit", "-x" ],
              fileArgsLead => [ "-i" ],
              trailArgs => [ "-i", "quit.lisp" ]
             },
             {
              tag => 'SHOP_CLISP_MIXEDCASE',
              name => "CLISP mixed-case",
              leadArgs => [ "-m", "1000M",
                            "-modern", "-on-error", "exit", "-x" ],
              fileArgsLead => [ "-i" ],
              trailArgs => [ "-i", "quit.lisp" ]
             },
             {
              tag => 'SHOP_CCL64_UPCASE',
              name => "64-bit Clozure CL",
              leadArgs => [ "--no-init", "--batch" ],
              fileArgsLead => [ "--load" ],
              trailArgs => [ "--load", "quit.lisp" ]
             },
             {
              tag => 'SHOP_CCL_UPCASE',
              name => "Clozure CL",
              leadArgs => [ "--no-init", "--batch" ],
              fileArgsLead => [ "--load" ],
              trailArgs => [ "--load", "quit.lisp" ]
             },
             {
              tag => 'SHOP_SBCL_UPCASE',
              name => "SBCL uppercase",
              leadArgs => [ "--lose-on-corruption", "--no-sysinit", "--no-userinit" ],
              fileArgsLead => [ "--load" ],
              trailArgs => [ "--load", "quit.lisp", "--end-toplevel-options" ]
             },
##             {
##              tag => 'SHOP_LW_UPCASE',
##              name => "LispWorks uppercase",
##              leadArgs => [ "-siteinit", "-", "-init" ],
##              fileArgsLead => [  ],
##              trailArgs => [  ]
##             },
            );

sub runLisp {
  my $tag = shift;
  my $name = shift;
  my $action = shift;
  my @call = @_;
  print "Running $name $action\n";
  # print join(' ', @call), "\n";
  my $OLD_OUT = \*STDOUT;
  my $pid = fork;
  if ($pid == 0) {
    my $logfile = lc "log/$tag.log";
    open STDOUT, "> $logfile" or die "Can't reset STDOUT: $!";
    open STDERR, ">&STDOUT" or die "Can't dup STDERR to STDOUT: $!";
    select STDERR; $| = 1;      # make unbuffered
    select STDOUT; $| = 1;      # make unbuffered
    exec @call;
    exit 1;
  }
  waitpid $pid, 0;
  my $result = $?;
  if ($result != 0) {
    push @failures, "$name exits with failure $action";
    print "$name exits with failure $action\n";
  } else {
    print "Normal exit\n";
  }
  return $result;
}

foreach my $lispConfig (@lisps) {
  my $tag = $lispConfig->{tag};
  my $ignoreTag = "SKIP_".$tag;
  my $name = $lispConfig->{name};
  my $leadArgs = $lispConfig->{leadArgs};
  my $fileArgsLead = $lispConfig->{fileArgsLead};
  my $trailArgs = $lispConfig->{trailArgs};
  print "--------------------\n";

  if (!(defined $ENV{$ignoreTag})) {
    my $executable = $ENV{$tag};
    if (defined $executable) {
      if (-x $executable) {
        my @precall;
        push @precall, $executable;
        push @precall, @$leadArgs;
        push @precall, @$fileArgsLead;
        push @precall, $shopdirlisp;
        push @precall, @$fileArgsLead;
        push @precall, "require-asdf.lisp";
        push @precall, @$fileArgsLead;
        push @precall, "path-init.lisp";

        runLisp "$tag-full", $name, 'on full recompile',
          @precall, @$fileArgsLead, "test-shop.lisp", @$trailArgs;
      } else {
        push @failures, "No binary $executable for $name";
      }
    } else {
      push @failures, "$tag not set";
    }
  } else {
    print "Skipping $name\n";
  }
}
  print "====================\n";

if ($#failures >= 0) {
  foreach my $f (@failures) {
    print $f, "\n";
  }
  exit 1;
} else {
  print "No failures detected.\n";
  exit 0;
}

