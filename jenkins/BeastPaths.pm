
package BeastPaths;
use strict;
use Carp;

use lib "$ENV{NST_DIR}/jenkins/NSTjenkins";
use NSTjenkins::CCL;
use NSTjenkins::Allegro;
use NSTjenkins::SBCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure/ccl';
$NSTjenkins::Allegro::PATH = '/usr/local/acl/acl82';
$NSTjenkins::SBCL::PATH = '/usr/local/bin/sbcl';
