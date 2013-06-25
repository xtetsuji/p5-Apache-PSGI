# -*- perl -*-

use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
    NAME           => 'Apache::PSGI',
    VERSION_FROM   => 'lib/Apache/PSGI.pm',
    ABSTRACT_FROM  => 'lib/Apache/PSGI.pm',
    AUTHOR         => 'OGATA Tetsuji <tetsuji.ogata at cpan.org>',
    PREREQ_PM      => {
        'Apache::Constants' => 0,
    },
    PMLIBDIRS      => ["lib"],
    META_MERGE     => {
        resources => {
            license    => 'http://dev.perl.org/licenses/',
            bugtracker => 'https://github.com/xtetsuji/p5-Apache-PSGI/issues',
            repository => 'git://github.com/xtetsuji/p5-Apache-PSGI.git',
        },
    },
    LISENSE        => "perl",
    PL_FILES       => {},
    EXE_FILES      => [],
    dist           => {
        SUFFIX => 'gz',
        COMPRESS => 'gzip -9f',
    },
);

# TODO: Better ModPerl::MM than EUMM?
