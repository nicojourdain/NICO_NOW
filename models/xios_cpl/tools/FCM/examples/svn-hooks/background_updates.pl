#!/usr/bin/perl

use strict;
use warnings;
use File::Spec;

my $home    = (getpwuid($<))[7];
my $command = File::Spec->catfile($home, qw{FCM sbin fcm-commit-update});
exec($command, @ARGV) || die("$command: cannot execute.\n");
__END__
