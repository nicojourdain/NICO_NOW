# ------------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

use strict;
use warnings;

package FCM::Admin::Config;

use File::Spec;

my $USER      = (getpwuid($<))[0];
my $HOME      = (getpwuid($<))[7];
my $LOCALDATA = File::Spec->catfile(q{/project/local}, $USER);

# Default values for read-only attributes
my %DEFAULT_R = (
    admin_email          => q{fcm@metoffice.gov.uk},
    fcm_home             => $HOME,
    fcm_dist_on_desktops => q{/project/ukmo/utils/supported/portable},
    fcm_dist_on_HPCs     => ['hpc1f:FCM/work/FCM/'],
    fcm_wc               => File::Spec->catfile($HOME, qw{FCM work FCM}),
    fcm_admin_wc         => File::Spec->catfile($HOME, qw{FCM work Admin}),
    trac_gid             => scalar(getgrnam(q{apache})),
    user_number_min      => 500,
);

# Default values for read-write attributes
my %DEFAULT_RW = (
    svn_backup_dir     => File::Spec->catfile($HOME, qw{svn backups}),
    svn_dump_dir       => File::Spec->catfile($HOME, qw{svn dumps}),
    svn_hook_dir       => File::Spec->catfile($HOME, qw{FCM work Admin svn-hooks}),
    svn_live_dir       => File::Spec->catfile($LOCALDATA, qw{svn live}),
    svn_passwd_file    => q{passwd},
    svn_project_suffix => q{_svn},
    trac_backup_dir    => File::Spec->catfile($HOME, qw{trac backups}),
    trac_live_dir      => File::Spec->catfile($LOCALDATA, qw{trac live}),
    trac_ini_file      => q{trac.ini},
    trac_passwd_file   => q{trac.htpasswd},
);

my $INSTANCE;

# ------------------------------------------------------------------------------
# Returns a unique instance of this class.
sub instance {
    my ($class) = @_;
    if (!$INSTANCE) {
        $INSTANCE = bless({%DEFAULT_R, %DEFAULT_RW}, $class);
    }
    return $INSTANCE;
}

# ------------------------------------------------------------------------------
# Getters
for my $name (keys(%DEFAULT_R), keys(%DEFAULT_RW)) {
    no strict qw{refs};
    my $getter = qq{get_$name};
    *$getter = sub {
        my ($self) = @_;
        return $self->{$name};
    };
}

# ------------------------------------------------------------------------------
# Setters
for my $name (keys(%DEFAULT_RW)) {
    no strict qw{refs};
    my $setter = qq{set_$name};
    *$setter = sub {
        my ($self, $value) = @_;
        $self->{$name} = $value;
    };
}

1;
__END__

=head1 NAME

FCM::Admin::Config

=head1 SYNOPSIS

    $config = FCM::Admin::Config->instance();
    $dir = $config->get_svn_backup_dir();
    # ...

=head1 DESCRIPTION

This class is used to retrieve/store configurations required by FCM
admininstration scripts.

=head1 METHODS

=over 4

=item FCM::Admin::Config->instance()

Returns a unique instance of this class. On first call, creates the instance
with the configurations set to their default values.

=item $config->get_admin_email()

Returns the e-mail address of the FCM administrator.

=item $config->get_fcm_home()

Returns the HOME directory of the FCM administrator.

=item $config->get_fcm_dist_on_desktops()

Returns the path for distributing FCM on the desktops.

=item $config->get_fcm_dist_on_HPCs()

Returns a list of locations for distributing FCM on the HPCs.

=item $config->get_fcm_wc()

Returns the (working copy) source path of the default FCM distribution.

=item $config->get_fcm_admin_wc()

Returns the (working copy) source path of the default FCM admin distribution.

=item $config->get_svn_backup_dir()

Returns the path to a directory containing the backups of SVN repositories.

=item $config->get_svn_dump_dir()

Returns the path to a directory containing the revision dumps of SVN
repositories.

=item $config->get_svn_hook_dir()

Returns the path to a directory containing source files of SVN hook scripts.

=item $config->get_svn_live_dir()

Returns the path to a directory containing the live SVN repositories.

=item $config->get_svn_passwd_file()

Returns the base name of the SVN password file.

=item $config->get_svn_project_suffix()

Returns the suffix added to the name of each SVN repository.

=item $config->get_trac_backup_dir()

Returns the path to a directory containing the backups of Trac environments.

=item $config->get_trac_gid()

Returns the group ID of the Trac server.

=item $config->get_trac_live_dir()

Returns the path to a directory containing the live Trac environments.

=item $config->get_trac_ini_file()

Returns the base name of the Trac INI file.

=item $config->get_trac_passwd_file()

Returns the base name of the Trac password file.

=item $config->get_user_number_min()

Returns the expected minimum number of users.

=item $config->set_svn_backup_dir($value)

Sets the path to a directory containing the backups of SVN repositories.

=item $config->set_svn_dump_dir($value)

Sets the path to a directory containing the revision dumps of SVN
repositories.

=item $config->set_svn_hook_dir($value)

Sets the path to a directory containing source files of SVN hook scripts.

=item $config->set_svn_live_dir($value)

Sets the path to a directory containing the live SVN repositories.

=item $config->set_svn_passwd_file($value)

Sets the base name of the SVN password file.

=item $config->set_svn_project_suffix($value)

Sets the suffix added to the name of each SVN repository.

=item $config->set_trac_backup_dir($value)

Sets the path to a directory containing the backups of Trac environments.

=item $config->set_trac_live_dir($value)

Sets the path to a directory containing the live Trac environments.

=item $config->set_trac_ini_file($value)

Sets the base name of the Trac INI file.

=item $config->set_trac_passwd_file($value)

Sets the base name of the Trac password file.

=back

=head1 COPYRIGHT

E<169> Crown copyright Met Office. All rights reserved.

=cut
