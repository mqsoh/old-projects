"""Truss is a bit more than a SimpleHTTPServer.

It uses SSL, basic HTTP auth, and supports file uploading. Its purpose is to
facilitate development in the cloud.

Usage:
    truss myuser mypassword 8000
    truss -d /path/to/docroot -h myhost myuser mypassword 8000
"""

import argparse
import sys

import core


def main():
    config = parse_args(sys.argv[1:])
    print('Serving on port {}...'.format(config['port']))
    core.make_server(**config).serve_forever()


def parse_args(args):
    """Converts arguments to program configuration.

    Args:
        args: Probably sys.argv[1:].

    Returns:
        A dictionary with the following keys: username, password, host, port,
        and docroot.
    """
    parser = argparse.ArgumentParser(description=sys.modules[__name__].__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('-n', '--hostname', default='', help=(
        'When creating the server, you can specify a host. I never use this.'))
    parser.add_argument('-d', '--docroot', default='.', help=(
        'The path to serve; defaults to the current directory.'))
    parser.add_argument('username', help='The accepted user name for basic HTTP auth.')
    parser.add_argument('password', help='The accepted password for basic HTTP auth.')
    parser.add_argument('port', type=int, help='The port to which we are binding.')

    return vars(parser.parse_args(args))
