#!/usr/bin/env perl
use strict;
use Carp;

$ENV{NST_DIR} = '~/Lib/Lisp/nst/';
$ENV{SHOP_DIR} = '~/Shop/trunk/';

use LepinayPaths;
use FullMultiplatform;
my $control = new FullMultiplatform;
$control->execute();
