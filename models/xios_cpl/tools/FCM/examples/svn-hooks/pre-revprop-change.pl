#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   pre-revprop-change.pl
#
# SYNOPSIS
#   pre-revprop-change.pl REPOS REV USER PROPNAME ACTION <&0
#
# DESCRIPTION
#   This script e-mails authors and watchers when a user attempts to modify the
#   svn:log of a particular revision. The new property value is passed via
#   STDIN. Watchers are set in the "watch.cfg" file, which should be located in
#   the root within the Subversion repository. The watch.cfg file is a standard
#   INI-type configuration file with the basic format:
#
#     [repos_base]
#     path/in/repos = list,of,watchers
#
#   E.g.:
#
#     [FCM_svn]
#
#     FCM/trunk/src            = fcm,frsn
#     FCM/trunk/doc            = fcm,frsn,frdm,frbj
#     FCM/branches/dev/*/*/src = fcm,frsn
#     FCM/branches/dev/*/*/doc = fcm,frsn,frdm,frbj
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

use strict;
use warnings;

use File::Basename;
use File::Spec;
use File::Temp qw/tempfile/;
use Mail::Mailer;
use Config::IniFiles;

# Arguments
# ------------------------------------------------------------------------------
my ($repos, $rev, $user, $propname, $action) = @ARGV;

# Basename of repository
my $base = basename $repos;

# Top level variables
# ------------------------------------------------------------------------------
# The watch configuration file, at the root of the current repository
my $watch_config = 'watch.cfg';

# Determine whether change is permitted
# ------------------------------------------------------------------------------
# Switch off for most revision properties
my $return = 1;

# Switch on only for "svn:log"
$return = 0 if $propname eq 'svn:log' and $action eq 'M';

if ($return == 0) {
  # Diagnostic
  print $repos, ': ', $propname, ' at revision ', $rev,
        ' is being modified by ', $user, '.', "\n";

  my %mail_to = ();

  # Mail original author, if he/she is not the current user
  # ----------------------------------------------------------------------------
  # Find out who is the author of the changeset at $rev
  my @command = (qw/svnlook author -r/, $rev, $repos);
  my $author  = qx(@command);
  chomp $author;

  # Add author to mail list, if necessary
  $mail_to{$author} = 1 if $author ne $user;

  # Mail watchers, if changeset involves files being watched
  # ----------------------------------------------------------------------------
  # Find out what files were involved in the changeset
  @command    = (qw/svnlook changed -r/, $rev, $repos);
  my @changed = qx(@command);

  # Get list of watchers for current repository
  my %watch = &get_watchers ();

  for my $file (@changed) {
    # Remove trailing line break and leading status
    chomp $file;
    $file = substr ($file, 4);

    # Find out who are watching this file
    my @watchers = &who_watch ($file, \%watch);

    # If necessary, add watchers to list, unless he/she is the current user
    for my $watcher (@watchers) {
      $mail_to{$watcher} = 1 if $user ne $watcher;
    }
  }

  # Send mail if necessary
  # ----------------------------------------------------------------------------
  if (keys %mail_to) {
    # Old value of revision property
    my @command = (qw/svnlook pg -r/, $rev, '--revprop', $repos, $propname);
    my $oldval  = qx(@command);

    # Addresses as a comma-separated list
    my $address = join (',', sort keys %mail_to);

    # Invoke a new Mail::Mailer object
    my $mailer  = Mail::Mailer->new ();
    $mailer->open ({
      From    => 'my.name@somewhere.org',
      To      => $address,
      Subject => $base . '@' . $rev . ': ' . $propname . ' modified by ' . $user,
    }) or die 'Cannot e-mail ', $address, ' (', $!, ')';

    # Write the mail
    # Old value
    print $mailer <<EOF;
Old value:
----------
$oldval

New value:
----------
EOF

    # New value from STDIN
    print $mailer $_ while (<STDIN>);

    # Send the mail
    $mailer->close;

    print 'Mail notification has been sent to ', $address, '.', "\n";

  } else {
    print 'No mail notification is required for this change.', "\n";
  }
}

exit $return;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %watch = &get_watchers ();
#
# DESCRIPTION
#   From the list of watch configuration files, get a list of watched files and
#   their watchers for the current repository. Returns the results in a hash
#   containing the watched paths (keys) and their corresponding list of
#   watchers (values, array references).
# ------------------------------------------------------------------------------

sub get_watchers {
  my %watch;

  # Get contents in watch file
  my @command = (qw/svnlook cat/, $repos, $watch_config);
  my @output  = qx(@command);

  if (@output) {
    # Write result to temporary file
    my ($fh, $temp_file) = tempfile (UNLINK => 1);
    print $fh @output;
    close $fh;

    # Parse the configuration
    my $cfg = Config::IniFiles->new ('-file' => $temp_file);

    # Check if current repository name exists in the configuration file
    if ($cfg and $cfg->SectionExists ($base)) {
      # The name of the parameter is a sub-path in the repository
      # The value of the parameter is a comma-delimited list of the watchers
      my $separator = '/';
      for my $parameter ($cfg->Parameters ($base)) {
        # Parameter may contain wildcards * and ?
        $parameter =~ s#\*#[^$separator]*#g;
        $parameter =~ s#\?#[^$separator]#g;

        $watch{$parameter} = [split (/,/, $cfg->val ($base, $parameter))];
      }
    }
  }

  return %watch;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   my @watchers = &who_watch ($file, \%watch);
#
# DESCRIPTION
#   Using the %watch hash, determine who are the watchers watching $file.
#   Returns the list of watchers.
# ------------------------------------------------------------------------------

sub who_watch {
  my $file  = $_[0];
  my %watch = %{ $_[1] };

  my %watchers;
  my $separator = '/';

  for my $watched (keys %watch) {
    # Test if $file or its parent path is being watched
    next unless $file =~ m#^$watched(?:$separator+|$)#;

    # Add watchers to the return list
    $watchers{$_} = 1 for (@{ $watch{$watched} });
  }

  return keys %watchers;
}

# ------------------------------------------------------------------------------

__END__
