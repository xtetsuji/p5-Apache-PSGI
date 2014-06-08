package Apache::PSGI;
# ref: http://search.cpan.org/~miyagawa/PSGI-1.101/PSGI.pod
# ref: https://gist.github.com/gardejo/398635/

use strict;
use warnings;

our $VERSION = '0.01';

use File::Spec        ();
use IO::Handle        ();
use Scalar::Util      ();

use Apache            ();
use Apache::Constants qw(OK HTTP_NOT_FOUND);
use Apache::Table     ();
use Apache::URI       ();

use constant TRUE  => 1==1;
use constant FALSE => !TRUE;

my %apps; # cache of apps. filename => psgi_app_coderef

sub new { bless {}, shift }

sub handler ($$) {
    #my $class = __PACKAGE__;
    my $class = shift;
    my $r     = shift;
    my $psgi  = $r->dir_config->get('psgi_app');
    return $class->call_app($r, $class->load_app($psgi, $r));
}

sub preload {
    my $class = shift;
    for my $app (@_) {
        $class->load_app($app);
    }
}

# see: Plack::Handler::Apache2#load_app
sub load_app {
    my ($class, $psgi, $r) = @_;
    # cache psgi file to %apps by code-reference.
    return $apps{$psgi} ||= do {
        # see: Plack::Handler::Apache2#load_app comment.
        local $ENV{MOD_PERL};
        delete $ENV{MOD_PERL};
        load_psgi($psgi);
    };
}

# see: Plack::Util#load_psgi
sub load_psgi {
    my $stuff = shift;

    my $file = $stuff =~ /^[a-zA-Z0-9\_\:]+$/ ? class_to_file($stuff) : File::Spec->rel2abs($stuff);

    my $app = _load_sandbox($file);
    die "Error while loading $file: $@" if $@;

    return $app;
}

# see: Plack::Util#class_to_file
sub class_to_file {
    my $class_str = shift;
    $class_str =~ s{::}{/}g;
    return $class_str . ".pm";
}

# see: Plack::Util#_load_sandbox
sub _load_sandbox {
    my $_file = shift;
    my $_package = $_file;
    $_package =~ s/([^A-Za-z0-9_])/sprintf("_%2x", unpack("C", $1))/eg;
    # mod_perl1's $0 is "/dev/null".
    # Apache is to segmanetation fault that this argument is modified.
    #local $0 = $_file; # so FindBin etc. works
    local @ARGV = ();  # Some frameworks might try to parse @ARGV

    #print STDERR "_package=$_package\n";

    # Protect tainted other namespace.
    return eval sprintf <<'END_EVAL', $_package;
package Apache::PSGI::Internal::Sandbox::%s;
{
    local $@;
    local $!;
    my $app = do $_file;
    if ( !$app && ( my $error = $@ || $! )) { die $error; }
    $app;
}
END_EVAL
}

sub call_app {
    my $class = shift;
    my $r     = shift;
    m y $app   = shift;
    my $headers_in = $r->headers_in;

    # subprocess_env creates CGI like %ENV called at void context.
    $r->subprocess_env;

    my $env = {
        # At first, create minimum environments
        %ENV,

        # psgi.* variables
        'psgi.version'           => [ 1, 1],
        'psgi.url_scheme'        => ($ENV{HTTPS}||'off') =~ /^(?:on|1)$/i ? 'https' : 'http',
        'psgi.input'             => $r, # AS-IS Apache#read. (same as Plack::Handler::Apache1)
        'psgi.errors'            => *STDERR,
        'psgi.multithread'       => FALSE,
        'psgi.multiprocess'      => TRUE,
        'psgi.run_once'          => FALSE,
        'psgi.streaming'         => TRUE,
        'psgi.nonblocking'       => FALSE,

        # psgix.* variables
        'psgix.harakiri'         => TRUE,
        'psgix.cleanup'          => TRUE,
        'psgix.cleanup.handlers' => [],

        # original support by Apache::PSGI.
        'psgix.logger'           => sub {
            my $param = shift;
            if ( !$param || ref $param ne 'HASH' ) {
                die qq(psgix.logger gives invalid argument. It expects hashref.);
            }
            my ($level, $message) = @$param{qw/level message/};
            if ( !defined $level || !defined $message ) {
                die qq(psgix.logger's hashref requires keys both "level", "message".);
            }
            $level = 'emerg' if $level eq 'fatal';
            if ( $r->log->can($level) ) {
                $r->log->$level($message);
            }
            else {
                die qq(psgix.logger's "level" value requires which "debug", "warn", "info", "error" or "fatal". level="$level" is invalid.);
            }
        },
    };

    $env->{CONTENT_LENGTH} = $headers_in->{'Content-Length'} || '' if exists $headers_in->{'Content-Length'};
    $env->{CONTENT_TYPE} = $headers_in->{'Content-Type'} || ''     if exists $headers_in->{'Content-Type'};

    if ( exists $env->{CONTENT_LENGTH} && defined $env->{CONTENT_LENGTH} ) {
        $env->{CONTENT_LENGTH} =~ s/,.*//;
    }

    delete $env->{HTTP_CONTENT_LENGTH};
    delete $env->{HTTP_CONTENT_TYPE};

    # TODO: Exam Apache::URI squeezes multi slashes into one slash.
    my ($unparsed_uri) = $r->the_request =~ /^$env->{REQUEST_METHOD} +(\S+)/;
    my $uri = Apache::URI->parse($r, $env->{'psgi.url_scheme'}.'://'.$r->hostname.$unparsed_uri);
    $env->{PATH_INFO} = Apache::Util::unescape_uri($uri->path); # TODO: same result of URI#path ?
    # unescape_uri_info ? ("+" converts to " ") is not defferent operation comparing with Plach::Handler::Apache2 (it uses URI#uri_unescape)


#     my $uri = APR::URI->parse($r->pool, $env->{'psgi.url_scheme'}.'://'.$r->hostname.$r->unparsed_uri);
#     $env->{PATH_INFO} = $uri->path; # TODO: same result of URI#path ?
#     Apache2::URI::unescape_url($env->{PATH_INFO});


    if ( !defined $env->{PATH_INFO} || 0 == length $env->{PATH_INFO} ) {
        $env->{SCRIPT_NAME} = $r->unparsed_uri;
    }

    # TODO: Need fixup_path ?
    $class->fixup_path($r, $env);

    my $res = $class->run_app($app, $env);

    if (ref $res eq 'ARRAY') {
        _handle_response($r, $res);
    }
    elsif (ref $res eq 'CODE') {
        # TODO: I don't know this situation.
        $res->(sub {
            _handle_response($r, shift);
        });
    }
    else {
        die "Bad response $res";
    }

    if (@{ $env->{'psgix.cleanup.handlers'} }) {
        $r->push_handlers(
            PerlCleanupHandler => sub {
                for my $cleanup_handler (@{ $env->{'psgix.cleanup.handlers'} }) {
                    $cleanup_handler->($env);
                }

                if ($env->{'psgix.harakiri.commit'}) {
                    $r->child_terminate;

                }
            },
        );
    }
    else {
        if ($env->{'psgix.harakiri.commit'}) {
            $r->child_terminate;
        }
    }

    return OK;
}

# Feature Plack::Hander::Apache2 mechanism.
sub fixup_path {
    my $class = shift;
    my $r     = shift;
    my $env   = shift;

    my $path_info = $env->{PATH_INFO} || '';
    my $location  = $r->location;
    if ( $location eq '/' ) {
        $env->{SCRIPT_NAME} = '';
    }
    elsif ( $path_info =~ s{^($location)/?}{/} ) {
        $env->{SCRIPT_NAME} = $1 || '';
    }
    else {
        $r->server->log_error(
            "Your request path is '$path_info' and it doesn't matech your Location(Match) '$location'. " .
            "This should be due to the configuration error. See perldoc Apache::PSGI for details."
        );
    }

    $env->{PATH_INFO} = $path_info;
}

sub _handle_response {
    my ($r, $res) = @_;

    my ($status, $headers, $body) = @{ $res };

    my $modperl_headers_out = ($status >= 200 && $status < 300)
        ? $r->headers_out : $r->err_headers_out;

    # see: Plack::Util#header_iter
    my @headers = @$headers;
    while ( my ($key, $value) = splice @headers, 0, 2 ) {
        if ( lc $key eq 'content-type' ) {
            $r->content_type($value);
        }
        else {
            # not ->set for multiple header keys (e.g. Set-Cookie)
            $modperl_headers_out->add( $key => $value );
        }
    }

    # Apache::Constants ":http" constants fits actual HTTP status code number.
    $r->status($status);

    $r->send_http_header;

    # see: Plack::Util#foreach
    if ( ref $body eq 'ARRAY' ) {
        for my $line (@$body) {
            $r->print($line) if length $line;
        }
    }
    elsif ( Scalar::Util::blessed($body)
            and $body->can('path')
            and my $path = $body->path) {
        if ( open my $fh, '<', $path ) {
            $r->send_fd($fh);
        }
        else {
            $r->status(HTTP_NOT_FOUND);
        }
    }
    elsif (defined $body && Scalar::Util::openhandle($body)) {
        # NOTE: by xtetsuji at 2013/05/03 12:10
        #   If $body is TRUE filehandle, then can we gives it to $r->send_fd()?
        #   It seems that PSGI spec says it situation.
        # Following method is same as Plack::Handler::Apache2
        local $/ = \65536 unless ref $/;
        while (defined(my $line = $body->getline)) {
            $r->print($line) if length $line;
        }
        $body->close;
        $r->rflush;
    }
#     else {
#         return Apache::PSGI::Internal::ResponseObject->new($r);
#     }

    return TRUE; # This value may be trashed at void context.
}

sub run_app {
    # see: Plack::Util#run_app
    my ($class, $app, $env) = @_;
    return eval { $app->($env) } || do {
        my $body = "Internal Server Error";
        $env->{'psgi.errors'}->print($@);
        [ 500, [ 'Content-Type' => 'text/plain', 'Content-Length' => length($body) ], [ $body ] ];
    };
}

{
    package Apache::PSGI::Internal::ErrorHandle;
    sub new {
        my $class = shift;
        my $r     = shift;
        bless { r => $r }, $class;
    }
    sub print {
        my $self = shift;
        $self->{r}->log_error(@_);
    }
}

# {
#     package Apache::PSGI::Internal::ResponseObject;
#     sub new {
#         my $class = shift;
#         my $r     = shift;
#         bless { r => $r }, $class;
#     }
#     # $r is global variable it currently.
#     sub write {
#         my $self = shift;
#         my $r = shift;
#         $self->{r}->print(@_);
#         $self->{r}->rflush;
#     }
#     sub close {
#         my $self = shift;
#         $self->{r}->rflush;
#     }
# }

1;

__END__

=pod

=encoding utf-8

=head1 NAME

Apache::PSGI - Lightweight PSGI adaptor for mod_perl1

=head1 SYNOPSIS

 # e.g. in Location or VirtualHost directive
 <Location /path/to/foo>
   SetHandler perl-script
   PerlHandler Apache::PSGI
   PerlSetVar psgi_app /real/path/to/foo/app.psgi
 </Location>

=head1 DESCRIPTION

This module is yet another PSGI implementation on mod_perl1.

This concept likes L<Plack>'s L<Plack::Handler::Apache1>,
but this module has some advantages:

=head2 Very low dependencies

L<Plack::Handler::Apache1> has L<Plack>'s dependencies.
It is not huge, but it is not few too.
If Your environment has some restriction of module installation,
maybe you can not ignore L<Plack>'s dependencies.

ModPerl::PSGI depends L<ONLY> mod_perl1 and Perl5.8 later core moduels.

=head2 Some process is delegated Apache Portable Runtime (apr)

For example, L<Plack> uses L<URI> and L<URI::Escape> for
URI parsing and processing.
In ModPerl::PSGI, this parsing and processing are delegated
mod_perl API and "Apache Portable Runtime" (APR) API.
Those implementes are C and glued by Perl XS.

=head2 Only on Apache web server

You may know combination of "plackup" and web server(Apache/Nginx)'s
reverse proxy for deploy PSGI app.
However this practice is not only one server about web server.
If you wish to opration that web server is only one,
then this concept is that you are comfortable.

Do you care Apache process size on this approach?
Use L<Apache2::SizeLimit> module for this problem
if it become actual.

For your advice, any persistent process have no small the problem.
Your operation skill is tried.

=head1 NOTE

L<Plack::Handler::Apache2> have more function than L<Plack::Handler::Apache1>.
But it seems some function can be realized on mod_perl1 environment too.
This Apache::PSGI is realized those function perhaps.

E.g. cleanup handlers (psgix.cleanup.handlers), ...

=head1 SEE ALSO

L<Plack>, L<Plack::Handler::Apache2>, L<Plack::Handler::Apache1>, L<Plack::Util>,
L<ModPerl::PSGI>

Many code base is referred to L<Plack>'s stuffs.
But Apache::PSGI does not have any Plack dependencies.

I aknowledge all Plack developers!

At first, L<ModPerl::PSGI> (for mod_perl2) is created.
It is inspired by L<Plack>'s stuffs.
This Apache::PSGI (for mod_perl1) is based on L<ModPerl::PSGI>.
In other words, Apache::PSGI is ported from L<ModPerl::PSGI>.

=head1 AUTHOR

OGATA Tetsuji, E<lt>tetsuji.ogata {at} gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by OGATA Tetsuji

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
