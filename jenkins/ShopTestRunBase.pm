
package ShopTestRunBase;
use strict;
use Carp;
use NSTjenkins::TestRun;
our @ISA = ("NSTjenkins::TestRun");
our $gensym=0;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->add_asdf_path($ENV{SHOP_DIR}.'temporal-shop2/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'jenkins/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'cndshop2/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shoplifter/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shop2/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shop2/examples/depots/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shop2/examples/UMT2/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shop2/examples/blocks/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'shop2/examples/logistic/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'equirer/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'type-checked-domain/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'lcwshop/');
  $self->add_asdf_path($ENV{SHOP_DIR}.'ltml-planner/');
  $self->add_asdf_path($ENV{CLOSER_MOP_DIR});
  $self->add_asdf_path($ENV{NST_DIR});
  $self->add_asdf_path($ENV{NST_DIR}.'ext/defdoc/');
  $self->add_asdf_path($ENV{NST_DIR}.'ext/defcontract/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/asdf/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/direct/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/manual/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/meta/');
  $self->add_asdf_path($ENV{NST_DIR}.'test/util/');
  $self->add_asdf_path($ENV{CLOSER_MOP_DIR});
}

1;
