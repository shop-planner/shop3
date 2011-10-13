#!/usr/bin/env perl
package shop_jenkins;
use Carp qw ( verbose );
use strict;
use BeastPaths;
use FullMultiplatform;

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

my $control = new FullMultiplatform;
$control->execute();
