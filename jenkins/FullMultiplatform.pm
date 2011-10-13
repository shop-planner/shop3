
package FullMultiplatform;
use strict;
use Carp;
use NSTjenkins::Master;
our @ISA = ("NSTjenkins::Master");

use FindBin;
use lib ( $FindBin::Bin );

use ShopTestRunBase;
use NSTjenkins::TestRun;
use NSTjenkins::System;
use NSTjenkins::Allegro;
use NSTjenkins::SBCL;
use NSTjenkins::CLISP;
use NSTjenkins::CCL;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);

  my $testrun = new ShopTestRunBase("SHOP2 test suite", 'shop2');
  $testrun->add_system(new NSTjenkins::System('full-test-suite'));

  $self->name('SHOP2 multiplatform tests');
  $self->tag('shop2');
  $self->add_testrun($testrun);

  ## Run one of each kind of platfrom first.
  $self->add_platform(NSTjenkins::CCL::instance()->arch64(1));
  $self->add_platform(NSTjenkins::Allegro::instance());
  #$self->add_platform(NSTjenkins::CLISP::instance()->mixedcase(1));
  $self->add_platform(NSTjenkins::SBCL::instance());

  ## Run their other versions.
  #$self->add_platform(NSTjenkins::CLISP::instance());
  $self->add_platform(NSTjenkins::CCL::instance());
  #$self->add_platform(NSTjenkins::Allegro::instance()->mixedcase(1));
  $self->add_platform(NSTjenkins::Allegro::instance()->eightbitmode(1));
  #$self->add_platform
  #    (NSTjenkins::Allegro::instance()->mixedcase(1)->eightbitmode(1));
}

1;
